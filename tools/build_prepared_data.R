#!/usr/bin/env Rscript
# Build the prepared death and population files in data/prepared/ from the
# snapshot archive (see R/prepared_data.R for what they hold and why).
#
#   Rscript tools/build_prepared_data.R [years=all] [force=false] [workers=cores-1]
#
# Deaths and population are built first, then the regional results from them;
# those take longest - most of an hour on four cores for every year - and run
# in `workers` parallel R processes.
#
# Reads only local files - nothing is requested from INE. A file is rebuilt
# when it is missing, when any input it was built from has changed, or when the
# code that computes it has changed; the rest are left as they are, so a run
# after a refresh rebuilds only what the refresh touched. force=true rebuilds
# everything.
#
# The files are derived, not data in their own right: they are written in place
# rather than through R/data_versions.R, whose archive and import log record the
# snapshot files they are built from.

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

# The app's own functions, loaded as the app loads them.
# The reference is always the computation from the archive, never the
# prepared files themselves.
options(mortality.prepared_data = FALSE)

load_app <- function(repo_root) {
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
  app
}
app <- load_app(repo_root)

force <- tolower(get_arg("force", "false")) %in% c("true", "1", "yes")
years_arg <- get_arg("years", "all")
# The regional stage runs in parallel R processes, a year each.
workers <- max(1L, as.integer(get_arg("workers", max(1L, parallel::detectCores() - 1L))))

build <- function() {
  targets <- prepared_targets(if (identical(years_arg, "all")) NULL else as.integer(strsplit(years_arg, ",", fixed = TRUE)[[1]]))

  code_hashes <- list(deaths = prepared_code_hash(), population = prepared_code_hash(), regions = prepared_region_code_hash())
  manifest <- read_prepared_manifest()
  counts <- c(built = 0L, current = 0L, empty = 0L)
  started <- Sys.time()

  # Whether a target's file exists and the manifest records the same inputs
  # and code.
  is_current <- function(kind, year, cause, inputs_hash) {
    hit <- which(manifest$kind == kind & manifest$year == year & manifest$cause %in% cause)
    !force && length(hit) == 1 && file.exists(prepared_target_path(kind, year, cause)) &&
      identical(manifest$inputs_hash[[hit]], inputs_hash) && identical(manifest$code_hash[[hit]], code_hashes[[kind]])
  }

  record <- function(row) {
    if (is.null(row)) {
      counts[["empty"]] <<- counts[["empty"]] + 1L
      return(invisible())
    }
    hit <- which(manifest$kind == row$kind & manifest$year == row$year & manifest$cause %in% row$cause)
    manifest <<- dplyr::bind_rows(manifest[setdiff(seq_len(nrow(manifest)), hit), , drop = FALSE], row)
    counts[["built"]] <<- counts[["built"]] + 1L
  }

  # One target: rebuilt unless current.
  ensure <- function(kind, year, cause, path, inputs, builder) {
    inputs_hash <- prepared_inputs_hash(inputs)
    if (is_current(kind, year, cause, inputs_hash)) {
      counts[["current"]] <<- counts[["current"]] + 1L
      return(invisible())
    }
    record(write_prepared_target(kind, year, cause, inputs_hash, code_hashes[[kind]], builder()))
  }

  population_years <- targets$year[targets$kind == "population"]
  for (year in population_years) {
    ensure("population", year, NA_character_, prepared_population_path(year),
           prepared_input_paths("population", year), function() build_prepared_population(year))
  }
  message(sprintf("population: %d years done", length(population_years)))

  deaths <- targets[targets$kind == "deaths", , drop = FALSE]
  for (year in sort(unique(deaths$year))) {
    causes <- deaths$cause[deaths$year == year]
    for (cause in causes) {
      ensure("deaths", year, cause, prepared_death_path(year, cause),
             prepared_input_paths("deaths", year, cause), function() build_prepared_deaths(year, cause))
    }
    # Each year's files are read once; letting them pile up in the readers'
    # memo would hold the whole archive in memory.
    memoise::forget(read_snapshot_object)
    write_prepared_rds(manifest, prepared_manifest_path(), encode = FALSE)
    message(sprintf("deaths %d: %d causes (%.0f s so far)", year, length(causes),
                    as.numeric(difftime(Sys.time(), started, units = "secs"))))
  }

  # The regional results are keyed by the frames the app loads, which come
  # from the prepared files when they are current - as they now are. Each
  # result is substitute_regional_deaths() of its key frame however that frame
  # was read, so reading it the fast way changes nothing stored.
  regions <- targets[targets$kind == "regions", , drop = FALSE]
  regions$inputs_hash <- vapply(seq_len(nrow(regions)), function(i) {
    prepared_inputs_hash(prepared_input_paths("regions", regions$year[[i]], regions$cause[[i]]))
  }, character(1))
  current <- vapply(seq_len(nrow(regions)), function(i) {
    is_current("regions", regions$year[[i]], regions$cause[[i]], regions$inputs_hash[[i]])
  }, logical(1))
  counts[["current"]] <- counts[["current"]] + sum(current)
  pending <- regions[!current, , drop = FALSE]

  if (nrow(pending) > 0) {
    write_prepared_rds(manifest, prepared_manifest_path(), encode = FALSE)
    code_hash <- code_hashes[["regions"]]
    message(sprintf("regions: %d files to build, %d workers", nrow(pending), workers))

    # A year at a time per worker, so a year's regional rows and totals are
    # read once. Run inside the app's functions, in this process or a worker.
    build_year <- function(year, pending, code_hash, app) {
      run <- function() {
        options(mortality.prepared_data = TRUE)
        Sys.setenv(MORTALITY_PREPARED_DATA = "true")
        prepared_state$manifest_key <- NULL
        p <- pending[pending$year == year, , drop = FALSE]
        rows <- lapply(seq_len(nrow(p)), function(i) {
          write_prepared_target("regions", year, p$cause[[i]], p$inputs_hash[[i]], code_hash,
                                build_prepared_regions(year, p$cause[[i]]))
        })
        memoise::forget(read_snapshot_object)
        memoise::forget(read_prepared_file)
        rows
      }
      environment(run) <- list2env(list(year = year, pending = pending, code_hash = code_hash), parent = app)
      run()
    }
    environment(build_year) <- globalenv()

    years <- sort(unique(pending$year), decreasing = TRUE)
    results <- if (workers > 1) {
      cluster <- parallel::makePSOCKcluster(min(workers, length(years)))
      on.exit(parallel::stopCluster(cluster), add = TRUE)
      start_worker <- function(root, loader) {
        setwd(root)
        suppressMessages(library(dplyr))
        assign("worker_app", loader(root), envir = globalenv())
        NULL
      }
      run_worker <- function(year, pending, code_hash, build_year) {
        build_year(year, pending, code_hash, get("worker_app", envir = globalenv()))
      }
      environment(start_worker) <- environment(run_worker) <- environment(load_app) <- globalenv()
      parallel::clusterCall(cluster, start_worker, repo_root, load_app)
      parallel::parLapplyLB(cluster, years, run_worker, pending, code_hash, build_year)
    } else {
      lapply(years, build_year, pending, code_hash, app)
    }

    for (rows in results) for (row in rows) record(row)
    message(sprintf("regions: done (%.0f s so far)", as.numeric(difftime(Sys.time(), started, units = "secs"))))
  }

  manifest <- manifest[order(manifest$kind, manifest$year, manifest$cause), , drop = FALSE]
  write_prepared_rds(manifest, prepared_manifest_path(), encode = FALSE)
  message(sprintf(
    "Done in %.0f s: %d built, %d already current, %d with no data. Manifest: %d files.",
    as.numeric(difftime(Sys.time(), started, units = "secs")),
    counts[["built"]], counts[["current"]], counts[["empty"]], nrow(manifest)
  ))
}

environment(build) <- app
build()
