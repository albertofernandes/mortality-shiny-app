#!/usr/bin/env Rscript
# Check that the prepared files in data/prepared/ hold exactly what the app
# computes from the snapshot archive (see R/prepared_data.R).
#
#   Rscript tools/check_prepared_data.R [years=all] [quick=false]
#
# 1. Every file that should exist does, and the manifest records the inputs and
#    code it was built from as they are now.
# 2. Deaths: for every year and cause, several selections of areas - Portugal,
#    a municipality, a region's municipalities, INE's own regional row, a mix -
#    taken from the prepared file are identical to load_snapshot_deaths() asked
#    for those areas. Then whole series, several years at once.
# 3. Population: the same for each year and both bases, against the frame
#    get_data_for_snapshot() builds.
# 4. The app's own requests, with the prepared files on and off.
# 5. Regions: every region with INE rows, under each vintage, deaths alone and
#    with population under each basis, as a whole series - the stored
#    substitutions against substitute_regional_deaths() run on the same frame.
#
# quick=true checks one selection per file instead of all. Exits non-zero if
# anything differs.

suppressMessages({
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  value <- if (length(hit) == 0) Sys.getenv(toupper(name), unset = "") else sub(paste0("^", name, "="), "", hit[[1]])
  if (!nzchar(value)) default else value
}

script_dir <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])))
repo_root <- normalizePath(file.path(script_dir, ".."))
setwd(repo_root)

# The reference is always the computation from the archive, never the
# prepared files themselves.
options(mortality.prepared_data = FALSE)

app <- new.env(parent = globalenv())
assign("app_dir", repo_root, envir = app)
assign("get_app_dir", function() repo_root, envir = app)
for (f in c(
  "R/config.R", "R/helpers.R", "R/cache.R", "R/snapshots.R", "R/ine_client.R", "R/regions.R",
  "R/infant.R", "R/regional_rows.R", "R/metadata.R", "R/metrics.R", "R/standardisation.R",
  "R/avoidable.R", "R/planning_parish.R", "R/planning_indicators.R", "R/life_expectancy.R",
  "R/planning_under75.R", "R/planning_standardised.R", "R/data_versions.R", "R/data_access.R",
  "R/prepared_data.R"
)) {
  sys.source(file.path(repo_root, f), envir = app)
}

quick <- tolower(get_arg("quick", "false")) %in% c("true", "1", "yes")
years_arg <- get_arg("years", "all")

check <- function() {
  set.seed(20261031)
  started <- Sys.time()
  problems <- character(0)
  problem <- function(...) {
    text <- paste0(...)
    problems <<- c(problems, text)
    message("  DIFFERENT: ", text)
  }
  counts <- c(files = 0L, death_comparisons = 0L, population_comparisons = 0L, series = 0L)

  years <- if (identical(years_arg, "all")) NULL else as.integer(strsplit(years_arg, ",", fixed = TRUE)[[1]])
  targets <- prepared_targets(years)
  manifest <- read_prepared_manifest()
  code_hashes <- list(deaths = prepared_code_hash(), population = prepared_code_hash(), regions = prepared_region_code_hash())

  # 1. Present and current.
  message("== 1. Files and fingerprints")
  for (i in seq_len(nrow(targets))) {
    t <- targets[i, ]
    path <- prepared_target_path(t$kind, t$year, t$cause)
    hit <- which(manifest$kind == t$kind & manifest$year == t$year & manifest$cause %in% t$cause)
    label <- paste(t$kind, t$year, if (is.na(t$cause)) "" else t$cause)
    if (!file.exists(path) || length(hit) != 1) {
      problem(label, ": missing")
      next
    }
    if (!identical(manifest$code_hash[[hit]], code_hashes[[t$kind]])) problem(label, ": built by different code")
    if (!identical(manifest$inputs_hash[[hit]], prepared_inputs_hash(prepared_input_paths(t$kind, t$year, t$cause)))) {
      problem(label, ": its inputs have changed since it was built")
    }
    counts[["files"]] <- counts[["files"]] + 1L
  }
  message(sprintf("   %d files present and current", counts[["files"]]))

  # Compared as the app would use them: same rows, same order, same values
  # and types.
  same <- function(expected, got) {
    identical(as.data.frame(expected), as.data.frame(got))
  }

  lookup <- get_nuts_lookup("2024")
  norte <- lookup$municipality[lookup$nuts2 == "Norte"]

  # 2. Deaths, file by file.
  message("== 2. Deaths, every year and cause")
  death_targets <- targets[targets$kind == "deaths", , drop = FALSE]
  for (year in sort(unique(death_targets$year))) {
    for (cause in death_targets$cause[death_targets$year == year]) {
      path <- prepared_death_path(year, cause)
      if (!file.exists(path)) next
      prepared <- decode_prepared_frame(readRDS(path))
      areas <- unique(as.character(prepared$area))
      municipalities <- intersect(areas, lookup$municipality)
      selections <- list(
        Portugal = intersect("Portugal", areas),
        municipality = if (length(municipalities) > 0) sample(municipalities, 1) else character(0),
        `Norte's municipalities` = intersect(norte, areas),
        `INE regional row` = intersect(c("Norte", "Centro", "Alentejo"), areas)[1],
        mix = intersect(c("Lisboa", "Loures", "Barrancos", "Portugal"), areas)
      )
      selections <- Filter(function(s) length(s) > 0 && !anyNA(s), selections)
      if (quick) selections <- utils::head(selections, 1)
      for (name in names(selections)) {
        selection <- selections[[name]]
        expected <- tryCatch(load_snapshot_deaths(as.integer(year), selection, cause), error = function(e) e)
        got <- prepared[prepared$area %in% selection, , drop = FALSE]
        counts[["death_comparisons"]] <- counts[["death_comparisons"]] + 1L
        if (inherits(expected, "error")) {
          problem("deaths ", year, " ", cause, " (", name, "): the app refused: ", conditionMessage(expected))
        } else if (!same(expected, got)) {
          problem("deaths ", year, " ", cause, " (", name, ")")
        }
      }
    }
    memoise::forget(read_snapshot_object)
    message(sprintf("   %d done (%d comparisons, %.0f s)", year, counts[["death_comparisons"]],
                    as.numeric(difftime(Sys.time(), started, units = "secs"))))
  }

  # Whole series: a request for several years is the years' files one after
  # the other.
  message("== 2b. Death series")
  series_years <- sort(unique(death_targets$year))
  for (case in list(
    list(cause = "Todas as causas de morte", areas = c("Norte", "Portugal")),
    list(cause = "Todas as causas de morte", areas = norte),
    list(cause = "Tuberculose", areas = c("Porto", "Barrancos"))
  )) {
    years_for_cause <- death_targets$year[death_targets$cause == case$cause]
    expected <- tryCatch(load_snapshot_deaths(years_for_cause, case$areas, case$cause), error = function(e) e)
    got <- dplyr::bind_rows(lapply(sort(years_for_cause), function(year) {
      prepared <- decode_prepared_frame(readRDS(prepared_death_path(year, case$cause)))
      prepared[prepared$area %in% case$areas, , drop = FALSE]
    }))
    counts[["series"]] <- counts[["series"]] + 1L
    label <- sprintf("series %s, %d areas, %d-%d", case$cause, length(case$areas), min(years_for_cause), max(years_for_cause))
    if (inherits(expected, "error")) problem(label, ": the app refused: ", conditionMessage(expected))
    else if (!same(expected, got)) problem(label) else message("   ", label, ": identical")
  }

  # 3. Population, as get_data_for_snapshot() builds it.
  message("== 3. Population, every year, both bases")
  app_population <- function(years, area, basis) {
    dplyr::bind_rows(
      get_snapshot_population_data(years, area),
      load_previous_population(years, basis, function(year) get_snapshot_population_data(year, area))
    ) %>%
      apply_population_basis(years, basis)
  }
  prepared_population <- function(years, area, basis) {
    dplyr::bind_rows(lapply(sort(years), function(year) {
      frame <- decode_prepared_frame(readRDS(prepared_population_path(year))[[basis]])
      frame[frame$area %in% area, , drop = FALSE]
    }))
  }
  population_years <- sort(targets$year[targets$kind == "population"])
  for (year in population_years) {
    frame <- decode_prepared_frame(readRDS(prepared_population_path(year))$end)
    areas <- unique(as.character(frame$area))
    municipalities <- intersect(areas, lookup$municipality)
    selections <- list(
      Portugal = intersect("Portugal", areas),
      municipality = sample(municipalities, 1),
      `Norte's municipalities` = intersect(norte, areas),
      mix = intersect(c("Lisboa", "Loures", "Barrancos"), areas)
    )
    selections <- Filter(function(s) length(s) > 0, selections)
    if (quick) selections <- utils::head(selections, 1)
    for (name in names(selections)) for (basis in c("end", "mean")) {
      expected <- tryCatch(app_population(year, selections[[name]], basis), error = function(e) e)
      counts[["population_comparisons"]] <- counts[["population_comparisons"]] + 1L
      if (inherits(expected, "error")) {
        problem("population ", year, " ", basis, " (", name, "): the app refused: ", conditionMessage(expected))
      } else if (!same(expected, prepared_population(year, selections[[name]], basis))) {
        problem("population ", year, " ", basis, " (", name, ")")
      }
    }
  }
  for (basis in c("end", "mean")) {
    expected <- app_population(population_years, norte, basis)
    got <- prepared_population(population_years, norte, basis)
    counts[["series"]] <- counts[["series"]] + 1L
    label <- sprintf("population series, Norte's municipalities, %d-%d, %s", min(population_years), max(population_years), basis)
    if (!same(expected, got)) problem(label) else message("   ", label, ": identical")
  }

  # A selection mixing areas with and without the previous year's estimate is
  # the one case a per-area file cannot reproduce: there the app gives every
  # area its own estimate. Say whether any year has such areas.
  uneven <- character(0)
  for (year in population_years[-1]) {
    now <- unique(as.character(readRDS(prepared_population_path(year))$end$area))
    before <- unique(as.character(readRDS(prepared_population_path(year - 1L))$end$area))
    if (length(setdiff(now, before)) > 0) uneven <- c(uneven, sprintf("%d (%s)", year, paste(utils::head(setdiff(now, before), 3), collapse = ", ")))
  }
  message("   areas without the previous year's population: ", if (length(uneven)) paste(uneven, collapse = "; ") else "none")

  # 4. What the app asks for, with the prepared files and without: the same
  # result, or the same refusal. Each case must also have been answered by
  # the prepared files rather than by the fallback, or it proves nothing.
  message("== 4. The app's requests, prepared files on and off")
  with_prepared <- function(on, expr) {
    old <- options(mortality.prepared_data = on)
    on.exit(options(old))
    tryCatch(force(expr), error = function(e) e)
  }
  cases <- list(
    list(label = "Portugal, all causes, 1991-2025", area = "Portugal", cause = "Todas as causas de morte", years = 1991:2025),
    list(label = "Norte's municipalities, all causes, 2015-2025", area = norte, cause = "Todas as causas de morte", years = 2015:2025),
    list(label = "Porto + Barrancos, tuberculosis, 2000-2024, years given newest first", area = c("Porto", "Barrancos"),
         cause = "Tuberculose", years = 2024:2000),
    # INE's Algarve row has population from 2021 but no 2020 row, the one case
    # where the mean depends on what is selected with it. The death archive has
    # no Algarve row for 2021, so a request for it is refused either way; the
    # population step itself is compared separately below.
    list(label = "Algarve's row with Porto, 2021 (must be refused: no deaths row)", area = c("Algarve", "Porto"),
         cause = "Todas as causas de morte", years = 2021, refused = TRUE),
    list(label = "Algarve's row with Porto, 2021-2022 (must be refused: no deaths row)", area = c("Algarve", "Porto"),
         cause = "Todas as causas de morte", years = 2021:2022, refused = TRUE),
    list(label = "Algarve's row with Porto, 2020-2022 (must be refused)", area = c("Algarve", "Porto"),
         cause = "Todas as causas de morte", years = 2020:2022, refused = TRUE),
    list(label = "a place that does not exist (must be refused)", area = "Nenhures", cause = "Todas as causas de morte",
         years = 2020:2021, refused = TRUE),
    list(label = "tuberculosis in 2025 (must be refused)", area = "Norte", cause = "Tuberculose", years = 2025, refused = TRUE)
  )
  if (!is.null(years)) cases <- Filter(function(cs) all(cs$years %in% years), cases)
  counts[["app_requests"]] <- 0L
  for (cs in cases) for (basis in c("end", "mean")) {
    label <- paste0(cs$label, ", ", basis)
    computed <- with_prepared(FALSE, get_data_for(cs$area, cs$cause, cs$years, data_source = "snapshot", population_basis = basis))
    prepared <- with_prepared(TRUE, get_data_for(cs$area, cs$cause, cs$years, data_source = "snapshot", population_basis = basis))
    deaths_computed <- with_prepared(FALSE, get_death_data_for(cs$area, cs$cause, cs$years, data_source = "snapshot"))
    deaths_prepared <- with_prepared(TRUE, get_death_data_for(cs$area, cs$cause, cs$years, data_source = "snapshot"))
    counts[["app_requests"]] <- counts[["app_requests"]] + 1L

    if (isTRUE(cs$refused)) {
      if (!inherits(computed, "error") || !inherits(prepared, "error")) {
        problem(label, ": should have been refused")
      } else if (!identical(conditionMessage(computed), conditionMessage(prepared))) {
        problem(label, ": refused with a different message")
      } else {
        message("   ", label, ": refused, same message")
      }
      next
    }
    if (inherits(computed, "error") || inherits(prepared, "error")) {
      problem(label, ": ", conditionMessage(if (inherits(computed, "error")) computed else prepared))
      next
    }
    used <- with_prepared(TRUE, !is.null(prepared_population_for(cs$years, cs$area, basis)) &&
      !is.null(prepared_deaths_for(cs$years, cs$area, cs$cause)))
    if (!isTRUE(used)) {
      problem(label, ": answered by the fallback, not the prepared files")
    } else if (!same(computed$full, prepared$full) || !same(computed$trunc, prepared$trunc) || !same(deaths_computed, deaths_prepared)) {
      problem(label)
    } else {
      message("   ", label, ": identical (prepared files used)")
    }
  }

  # The population step on its own for that case: with Porto, Algarve's
  # missing 2020 row makes the app give both their own 2021 estimate.
  for (pop_case in list(list(years = 2021L, area = c("Algarve", "Porto")), list(years = 2021:2022, area = c("Algarve", "Porto")),
                        list(years = 2021L, area = "Algarve"), list(years = 2021L, area = "Porto"))) {
    if (!is.null(years) && !all(pop_case$years %in% years)) next
    for (basis in c("end", "mean")) {
      label <- sprintf("population step, %s, %s, %s", paste(pop_case$area, collapse = " + "),
                       paste(range(pop_case$years), collapse = "-"), basis)
      expected <- tryCatch(app_population(pop_case$years, pop_case$area, basis), error = function(e) e)
      got <- with_prepared(TRUE, prepared_population_for(pop_case$years, pop_case$area, basis))
      counts[["app_requests"]] <- counts[["app_requests"]] + 1L
      if (inherits(expected, "error") || is.null(got) || inherits(got, "error")) {
        problem(label, ": ", if (inherits(expected, "error")) conditionMessage(expected) else "prepared files gave no answer")
      } else if (!same(expected, got)) {
        problem(label)
      } else {
        message("   ", label, ": identical (prepared files used)")
      }
    }
  }

  message(sprintf(
    "\n%d app requests compared with the prepared files on and off.", counts[["app_requests"]]
  ))

  # 5. Regions: for each region with INE rows, under each vintage, deaths
  # alone and with population under each basis, a whole series substituted by
  # substitute_regional_deaths() as the app does without the prepared files,
  # against prepared_region_substitution(). The frame is the one the app
  # loads; the reference substitution reads the subtracted municipalities from
  # the archive with the prepared files off. Every year with a substitution
  # must have come from the files.
  message("== 5. Regions, every region and vintage, deaths and both population bases")
  region_targets <- targets[targets$kind == "regions", , drop = FALSE]
  counts[["region_series"]] <- 0L
  if (nrow(region_targets) > 0) {
    region_years <- sort(unique(region_targets$year))
    all_years <- sort(unique(c(region_years, death_targets$year)))
    minus_reference <- function(areas, years, causes) {
      with_prepared(FALSE, regional_minus_deaths(areas, years, causes))
    }
    causes_for <- function(region) {
      if (quick) return("Todas as causas de morte")
      # Every cause for one plain row, one composition with a municipality
      # added and one with a municipality subtracted; three for the rest.
      if (region %in% c("Norte", "Centro", "Oeste e Vale do Tejo")) return(sort(unique(region_targets$cause)))
      c("Todas as causas de morte", "Tuberculose", "Doenças do aparelho circulatório")
    }
    available <- unique(unlist(get_snapshot_inventory()$areas, use.names = FALSE))

    for (vintage in unname(nuts_vintage_choices)) {
      lookup_v <- get_nuts_lookup(vintage)
      for (region in unique(REGIONAL_ROW_TERRITORIES$region)) {
        if (!region %in% get_known_regions(lookup_v) || nrow(regional_row_plan(region, vintage, region_years)) == 0) next
        resolved <- resolve_region_areas(region, default_region_mode(), lookup_v, available)
        for (cause in intersect(causes_for(region), region_targets$cause)) {
          years_for_cause <- intersect(all_years, c(region_targets$year[region_targets$cause == cause],
                                                    death_targets$year[death_targets$cause == cause]))
          for (kind in c("deaths", "end", "mean")) {
            label <- sprintf("%s (NUTS %s), %s, %s, %d-%d", region, vintage, cause,
                             if (kind == "deaths") "deaths" else paste("population", kind), min(years_for_cause), max(years_for_cause))
            frame <- with_prepared(TRUE, if (kind == "deaths") {
              get_death_data_for(resolved$areas, cause, years_for_cause, year_order = "desc", data_source = "snapshot")
            } else {
              get_data_for(resolved$areas, cause, years_for_cause, year_order = "desc", data_source = "snapshot", population_basis = kind)$full
            })
            counts[["region_series"]] <- counts[["region_series"]] + 1L
            if (inherits(frame, "error")) {
              problem(label, ": the app could not load it: ", conditionMessage(frame))
              next
            }
            expected <- substitute_regional_deaths(frame, region, vintage, lookup_v, "ine_rows", load_municipal_deaths = minus_reference)
            got <- with_prepared(TRUE, prepared_region_substitution(frame, region, vintage, lookup_v, "ine_rows", regional_minus_deaths))
            substituted <- unique(attr(expected, "regional_substitutions")$year)
            served <- with_prepared(TRUE, {
              stored <- prepared_region_entries(frame, region, vintage, cause)
              stored$years[!vapply(stored$entries, is.null, logical(1))]
            })
            if (inherits(got, "error") || is.null(got)) {
              problem(label, ": no answer from the prepared files", if (inherits(got, "error")) paste(":", conditionMessage(got)))
            } else if (!setequal(substituted, served)) {
              problem(label, ": years substituted ", length(substituted), ", taken from the files ", length(served))
            } else if (!identical(expected, got)) {
              problem(label, if (same(expected, got)) ": rows identical, attributes differ" else "")
            }
          }
        }
        memoise::forget(read_snapshot_object)
        message(sprintf("   NUTS %s %s done (%d series, %.0f s)", vintage, region, counts[["region_series"]],
                        as.numeric(difftime(Sys.time(), started, units = "secs"))))
      }
    }
  }
  message(sprintf(
    "\n%d files, %d death and %d population comparisons, %d series, %d regional series, in %.0f s: %s",
    counts[["files"]], counts[["death_comparisons"]], counts[["population_comparisons"]], counts[["series"]],
    counts[["region_series"]],
    as.numeric(difftime(Sys.time(), started, units = "secs")),
    if (length(problems) == 0) "ALL IDENTICAL" else paste(length(problems), "PROBLEM(S)")
  ))
  length(problems) == 0
}

environment(check) <- app
if (!check()) quit(status = 1)
