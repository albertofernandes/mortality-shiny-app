# =========================================================
# Prepared death and population data
# =========================================================
# Reading deaths and population from the snapshot archive repeats the same
# preparation on every request: the two death indicators are merged row by row,
# INE's values are corrected (data/source_corrections.rds), deaths without a
# published age are filled in, the latest year is rebuilt from weekly deaths,
# and population is averaged over two year-ends. None of it depends on what the
# user selects, and on a cold start it is most of the waiting.
#
# So it is done once, when the data change, and stored beside the archive in
# data/prepared/:
#
#   deaths/year_<year>/cause_<cause>.rds   load_snapshot_deaths() for that year
#                                          and cause, every area
#   population/year_<year>.rds             list(end, mean): population for that
#                                          year, every area, under each basis
#   regions/year_<year>/cause_<cause>.rds  substitute_regional_deaths() for each
#                                          region with INE regional rows, keyed
#                                          by the exact frame it was given
#   manifest.rds                           what each file was built from
#
# The files are built by the app's own functions - load_snapshot_deaths(),
# get_snapshot_population_data(), apply_population_basis() - not by a second
# implementation, so they hold exactly what those functions return. Each area's
# rows do not depend on which other areas are loaded with them, so a request is
# answered by taking its areas' rows from these files.
#
# A file is current only while the inputs it was built from and the code that
# built it are unchanged. Both are fingerprinted in the manifest:
# prepared_input_paths() lists the input files, prepared_code_hash() the
# functions. tools/build_prepared_data.R builds what is missing or stale, and
# tools/check_prepared_data.R proves the files equal what the app computes.

PREPARED_FORMAT_VERSION <- 2L

prepared_root <- function() file.path(dirname(get_snapshot_dir()), "prepared")

prepared_death_path <- function(year, cause) {
  file.path(prepared_root(), "deaths", paste0("year_", year), paste0("cause_", snapshot_file_token(cause), ".rds"))
}

prepared_population_path <- function(year) {
  file.path(prepared_root(), "population", paste0("year_", year, ".rds"))
}

prepared_region_path <- function(year, cause) {
  file.path(prepared_root(), "regions", paste0("year_", year), paste0("cause_", snapshot_file_token(cause), ".rds"))
}

prepared_manifest_path <- function() file.path(prepared_root(), "manifest.rds")

# The functions whose results the files store, and those they call to compute
# them. A change to any of them makes every file stale, so a correction to the
# method reaches the prepared data on the next build.
prepared_code_functions <- c(
  "load_snapshot_deaths", "get_snapshot_death_data", "get_flat_or_chunked_death_snapshot",
  "read_chunked_death_snapshot", "collapse_death_snapshot_chunks", "read_snapshot_object",
  "apply_source_corrections", "fill_death_ages", "death_age_fill", "planning_complete_by_age",
  "death_totals_raw", "calendar_municipal_deaths", "planning_calendar_deaths",
  "get_snapshot_population_data", "get_flat_or_chunked_population_snapshot",
  "read_chunked_population_snapshot", "apply_population_basis"
)

prepared_function_texts <- function(names) {
  lapply(names, function(name) {
    if (!exists(name, mode = "function", inherits = TRUE)) return(paste(name, "missing"))
    fn <- get(name, mode = "function", inherits = TRUE)
    # memoise wraps the function; fingerprint what it wraps.
    inner <- environment(fn)$`_f`
    paste(deparse(if (is.function(inner)) inner else fn), collapse = "\n")
  })
}

prepared_code_hash <- function() {
  constants <- list(age_levels, exclude_bands, planning_published_areas, PREPARED_FORMAT_VERSION)
  rlang::hash(list(prepared_function_texts(prepared_code_functions), constants))
}

# The regional substitution and what it reads beyond the frame it is given:
# INE's regional rows, the death totals it tops them up to, the region
# memberships, and the deaths of the municipalities a composition subtracts.
# The frame itself is not fingerprinted here - each stored result is keyed by
# it (prepared_frame_key()).
prepared_region_code_functions <- c(
  "substitute_regional_deaths", "top_up_regional_rows", "regional_row_plan", "read_regional_rows",
  "split_list", "normalize_region_source", "death_totals_for", "repair_death_totals",
  "death_totals_raw", "read_death_file", "region_municipalities", "region_members",
  "health_unit_members", "get_death_data_for", "regional_minus_deaths", "prepared_frame_key"
)

prepared_region_code_hash <- function() {
  rlang::hash(list(
    prepared_code_hash(),
    prepared_function_texts(prepared_region_code_functions),
    REGIONAL_ROW_TERRITORIES
  ))
}

prepared_kind_code_hash <- function(kind) {
  if (identical(kind, "regions")) prepared_region_code_hash() else prepared_code_hash()
}

prepared_snapshot_dir <- function() get_snapshot_dir()
prepared_data_dir <- function() dirname(get_snapshot_dir())

# Every file a prepared file's content depends on. Missing files are listed
# too, under their path: a file that appears later changes the fingerprint.
prepared_input_paths <- function(kind, year, cause = NULL) {
  year <- as.integer(year)
  snap <- prepared_snapshot_dir()
  indicators <- c("0013166", "0008206")
  lookup <- file.path(prepared_data_dir(), "nuts_lookup_2024.rds")
  corrections <- file.path(prepared_data_dir(), "source_corrections.rds")
  population <- function(y) file.path(snap, "population", paste0("year_", y, ".rds"))

  if (identical(kind, "population")) {
    return(c(population(year), population(year - 1L)))
  }

  if (identical(kind, "regions")) {
    # The deaths files cover the totals the rows are topped up to, their
    # repair, and the subtracted municipalities.
    return(c(
      file.path(snap, "regional_deaths", indicators, paste0("year_", year, ".rds")),
      prepared_input_paths("deaths", year, cause),
      file.path(prepared_data_dir(), c("nuts_lookup_2013.rds", "nuts_lookup_2024.rds", "uls_lookup.rds"))
    ))
  }

  if (year %in% active_calendar_years()) {
    # Rebuilt from the region's deaths by age and its municipalities'
    # population; see calendar_municipal_deaths().
    return(c(file.path(snap, "calendar_deaths_by_age", paste0("year_", year, ".rds")), population(year), lookup))
  }

  token <- snapshot_file_token(cause)
  c(
    file.path(snap, "deaths", indicators, paste0("year_", year), paste0("cause_", token, ".rds")),
    # The age fill reads the year's all-ages totals and population, and the
    # municipality list.
    file.path(snap, "death_totals", indicators, paste0("year_", year, ".rds")),
    population(year),
    lookup,
    corrections
  )
}

# A path relative to the data directory, so a fingerprint does not depend on
# where the repository is or on how the path is written: the app reaches the
# directory through a Windows path with backslashes, the tools through one
# with forward slashes, and the drive letter's case varies.
prepared_relative_path <- function(paths) {
  root <- paste0(normalizePath(prepared_data_dir(), winslash = "/", mustWork = FALSE), "/")
  full <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  inside <- startsWith(tolower(full), tolower(root))
  ifelse(inside, substring(full, nchar(root) + 1L), full)
}

prepared_inputs_hash <- function(paths) {
  present <- file.exists(paths)
  rlang::hash(list(
    prepared_relative_path(paths),
    present,
    vapply(paths[present], rlang::hash_file, character(1), USE.NAMES = FALSE)
  ))
}

# The areas in a year's death files for a cause, every indicator together - the
# areas load_snapshot_deaths() can be asked for.
prepared_death_areas <- function(year, cause) {
  if (as.integer(year) %in% active_calendar_years()) {
    rows <- calendar_municipal_deaths(year)
    return(if (is.null(rows)) character(0) else sort(unique(as.character(rows$area))))
  }
  paths <- prepared_input_paths("deaths", year, cause)[1:2]
  paths <- paths[file.exists(paths)]
  sort(unique(unlist(lapply(paths, function(path) as.character(read_snapshot_object(path)$area)), use.names = FALSE)))
}

# Every prepared file there should be: one per population year, and one per
# year and cause in the death archive (all causes only, for the rebuilt latest
# year). `years` limits it.
prepared_targets <- function(years = NULL) {
  inventory <- get_snapshot_inventory()
  deaths <- inventory[inventory$dataset == "deaths", c("year", "cause"), drop = FALSE]
  deaths <- unique(data.frame(year = as.integer(deaths$year), cause = as.character(deaths$cause), stringsAsFactors = FALSE))
  calendar <- as.integer(active_calendar_years())
  deaths <- rbind(
    deaths[!deaths$year %in% calendar, , drop = FALSE],
    data.frame(year = calendar, cause = rep("Todas as causas de morte", length(calendar)), stringsAsFactors = FALSE)
  )
  population_years <- as.integer(sub("^year_(\\d+)\\.rds$", "\\1",
    list.files(file.path(get_snapshot_dir(), "population"), pattern = "^year_\\d+\\.rds$")))

  # Regional files for the years INE's regional rows cover.
  regions <- deaths[deaths$year %in% prepared_region_years(), , drop = FALSE]

  targets <- rbind(
    data.frame(kind = "population", year = population_years, cause = NA_character_, stringsAsFactors = FALSE),
    data.frame(kind = rep("deaths", nrow(deaths)), year = deaths$year, cause = deaths$cause, stringsAsFactors = FALSE),
    data.frame(kind = rep("regions", nrow(regions)), year = regions$year, cause = regions$cause, stringsAsFactors = FALSE)
  )
  if (!is.null(years)) targets <- targets[targets$year %in% as.integer(years), , drop = FALSE]
  targets[order(match(targets$kind, c("population", "deaths", "regions")), targets$year, targets$cause), , drop = FALSE]
}

prepared_target_path <- function(kind, year, cause) {
  switch(kind,
    population = prepared_population_path(year),
    regions = prepared_region_path(year, cause),
    prepared_death_path(year, cause)
  )
}

build_prepared_deaths <- function(year, cause) {
  areas <- prepared_death_areas(year, cause)
  if (length(areas) == 0) return(NULL)
  load_snapshot_deaths(as.integer(year), areas, cause)
}

# Population of one year under each basis, every area. The mean takes the
# previous year only for the areas that have it, so each area reads as it does
# requested on its own: one without a previous estimate keeps its own.
build_prepared_population <- function(year) {
  year <- as.integer(year)
  areas <- sort(unique(as.character(read_snapshot_object(prepared_input_paths("population", year)[[1]])$area)))
  current <- get_snapshot_population_data(year, areas)

  previous_path <- prepared_input_paths("population", year)[[2]]
  previous <- NULL
  if (file.exists(previous_path)) {
    shared <- intersect(areas, unique(as.character(read_snapshot_object(previous_path)$area)))
    previous <- tryCatch(get_snapshot_population_data(year - 1L, shared), error = function(e) NULL)
  }

  list(
    end = apply_population_basis(current, year, "end"),
    mean = apply_population_basis(dplyr::bind_rows(current, previous), year, "mean")
  )
}

# ---------------------------------------------------------
# Regions
# ---------------------------------------------------------
# A region selected on its own is loaded as its municipalities and then handed
# to substitute_regional_deaths() (R/regional_rows.R), which swaps in INE's
# regional row, tops it up to the death totals and, for the composed regions,
# adds or subtracts municipalities. On a long series that is most of the
# waiting, repeated on every request.
#
# The substitution treats each year on its own, so its result for a year
# depends only on that year's rows of the frame and on the archive. The build
# runs it, year by year, on the frames the app loads for each region, and
# stores each result under a fingerprint of the exact frame it was given. The
# app fingerprints its own frame the same way and takes a stored result only
# when the two are identical; any other year is computed as before. So a stored
# result is always what substitute_regional_deaths() returns for that input.

# The years INE's regional rows cover.
prepared_region_years <- function() {
  dirs <- file.path(prepared_snapshot_dir(), "regional_deaths", c("0013166", "0008206"))
  files <- list.files(dirs, pattern = "^year_\\d+\\.rds$")
  sort(unique(as.integer(sub("^year_(\\d+)\\.rds$", "\\1", files))))
}

# Rows for the municipalities a composition subtracts; the same read the app's
# apply_region_source() passes to substitute_regional_deaths().
regional_minus_deaths <- function(areas, years, causes) {
  tryCatch(
    get_death_data_for(area = areas, cause = causes, years = years, data_source = "snapshot"),
    error = function(e) NULL
  )
}

# A fingerprint of a frame's content, whatever order its rows come in: the
# loader of a multi-place request returns the same rows as a single-place load,
# but not necessarily in the same order. Column names, types, factor levels and
# every value count.
prepared_frame_key <- function(frame) {
  frame <- as.data.frame(frame, stringsAsFactors = FALSE)
  keys <- intersect(c("year", "area", "sex", "cause", "age_band"), names(frame))
  if (nrow(frame) > 0 && length(keys) > 0) {
    frame <- frame[do.call(order, c(unname(lapply(frame[keys], as.character)), list(method = "radix"))), , drop = FALSE]
  }
  attributes(frame) <- list(names = names(frame), class = "data.frame", row.names = .set_row_names(nrow(frame)))
  rlang::hash(frame)
}

prepared_region_entry_id <- function(vintage, region, key) paste(vintage, region, key, sep = "|")

# The regions with INE rows in `year`, each with the areas the app loads for it
# when selected on its own, under each vintage. A region whose rows are the
# same under both vintages and whose municipalities are too - Norte, the
# autonomous regions, the Alentejo ULS - is substituted identically under
# either, so it is built once, under the vintage "any".
prepared_region_specs <- function(year) {
  # As the server's get_available_areas("snapshot").
  available <- unique(unlist(get_snapshot_inventory()$areas, use.names = FALSE))
  vintages <- unname(nuts_vintage_choices)
  lookups <- lapply(vintages, get_nuts_lookup)
  names(lookups) <- vintages

  specs <- list()
  for (region in unique(REGIONAL_ROW_TERRITORIES$region)) {
    resolved <- list()
    for (vintage in vintages) {
      lookup <- lookups[[vintage]]
      if (nrow(regional_row_plan(region, vintage, year)) == 0 || !region %in% get_known_regions(lookup)) next
      r <- resolve_region_areas(region, default_region_mode(), lookup, available)
      if (!identical(r$expanded, region) || length(r$areas) == 0) next
      resolved[[vintage]] <- list(
        vintage = vintage, region = region, areas = r$areas, lookup = lookup,
        members = sort(region_municipalities(region, lookup))
      )
    }

    rows <- REGIONAL_ROW_TERRITORIES[REGIONAL_ROW_TERRITORIES$region == region, , drop = FALSE]
    same <- length(resolved) == length(vintages) && all(is.na(rows$vintage)) &&
      all(vapply(resolved[-1], function(s) identical(s$areas, resolved[[1]]$areas) && identical(s$members, resolved[[1]]$members), logical(1)))
    if (same) {
      resolved <- list(c(resolved[[1]], list(id_vintage = "any")))
    } else {
      resolved <- lapply(resolved, function(s) c(s, list(id_vintage = s$vintage)))
    }
    specs <- c(specs, unname(resolved))
  }
  specs
}

# Every stored result for one year and cause: for each region and vintage, the
# frame the app loads for deaths alone and with population under each basis.
# Entries hold the rows encoded as the other files are, and the
# `regional_substitutions` attribute; a frame the substitution leaves as it is
# is not stored.
#
# A request reaching the rebuilt latest year loads the deaths column as a
# one-dimensional array rather than a plain vector, so every year of it is a
# different frame - and substitute_regional_deaths() keeps the difference. For
# the causes that year has, each frame is also built as such a request loads
# it: with the latest year, then cut to this one.
build_prepared_regions <- function(year, cause) {
  year <- as.integer(year)
  load_frames <- function(areas, years) {
    frames <- list(tryCatch(
      get_death_data_for(area = areas, cause = cause, years = years, year_order = "desc", data_source = "snapshot"),
      error = function(e) NULL
    ))
    for (basis in unname(population_basis_choices)) {
      frames[[length(frames) + 1L]] <- tryCatch(
        get_data_for(
          area = areas, cause = cause, years = years, year_order = "desc",
          data_source = "snapshot", population_basis = basis
        )$full,
        error = function(e) NULL
      )
    }
    lapply(frames, function(frame) if (!is.null(frame)) frame[as.integer(frame$year) == year, , drop = FALSE])
  }
  latest <- if (identical(cause, "Todas as causas de morte")) setdiff(active_calendar_years(), year) else integer(0)

  entries <- list()
  for (spec in prepared_region_specs(year)) {
    frames <- load_frames(spec$areas, year)
    if (length(latest) > 0) frames <- c(frames, load_frames(spec$areas, c(year, latest)))

    for (frame in frames) {
      if (is.null(frame) || nrow(frame) == 0) next
      out <- substitute_regional_deaths(
        frame, spec$region, spec$vintage, spec$lookup,
        source = "ine_rows", load_municipal_deaths = regional_minus_deaths
      )
      applied <- attr(out, "regional_substitutions")
      if (is.null(applied) || nrow(applied) == 0) next
      attr(out, "regional_substitutions") <- NULL
      entries[[prepared_region_entry_id(spec$id_vintage, spec$region, prepared_frame_key(frame))]] <- list(
        rows = encode_prepared_frame(out),
        applied = applied
      )
    }
  }
  if (length(entries) == 0) NULL else entries
}

# Text columns are stored as factors, and the columns listed in an attribute:
# rebuilding some 17,000 strings per column is most of the time readRDS() takes
# on a death file (36 ms a file against 3 ms as factors). The reader turns them
# back into text after keeping the requested areas, so what it returns is
# exactly what was built.
encode_prepared_frame <- function(frame) {
  text <- names(frame)[vapply(frame, is.character, logical(1))]
  for (col in text) frame[[col]] <- factor(frame[[col]])
  attr(frame, "prepared_text") <- text
  frame
}

# `rows` keeps rows before the columns are turned back into text, which is
# where the saving is.
decode_prepared_frame <- function(frame, rows = NULL) {
  text <- attr(frame, "prepared_text")
  if (!is.null(rows)) frame <- frame[rows, , drop = FALSE]
  for (col in text) frame[[col]] <- as.character(frame[[col]])
  attr(frame, "prepared_text") <- NULL
  frame
}

# Written atomically, as the snapshot files are, but xz-compressed: with the
# text columns as factors, the smallest and fastest to read of gzip, xz and no
# compression. `encode` is FALSE for the manifest.
write_prepared_rds <- function(x, path, encode = TRUE) {
  if (encode) {
    x <- if (is.data.frame(x)) encode_prepared_frame(x) else lapply(x, encode_prepared_frame)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  saveRDS(x, tmp, version = 2, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not move temporary file into ", path, call. = FALSE)
}

# Write one built target and return its manifest row; NULL when there was
# nothing to build. Region entries are encoded as they are built.
write_prepared_target <- function(kind, year, cause, inputs_hash, code_hash, value) {
  if (is.null(value)) return(NULL)
  path <- prepared_target_path(kind, year, cause)
  write_prepared_rds(value, path, encode = !identical(kind, "regions"))
  tibble::tibble(
    kind = kind, year = as.integer(year), cause = cause,
    path = prepared_relative_path(path),
    inputs_hash = inputs_hash, code_hash = code_hash,
    rows = switch(kind,
      population = nrow(value$end),
      regions = sum(vapply(value, function(entry) nrow(entry$rows), integer(1))),
      nrow(value)
    ),
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

read_prepared_manifest <- function() {
  path <- prepared_manifest_path()
  empty <- tibble::tibble(
    kind = character(0), year = integer(0), cause = character(0), path = character(0),
    inputs_hash = character(0), code_hash = character(0), rows = integer(0), built_at = character(0)
  )
  if (!file.exists(path)) return(empty)
  tryCatch(readRDS(path), error = function(e) empty)
}

# ---------------------------------------------------------
# Reading
# ---------------------------------------------------------
# The app asks prepared_deaths_for() and prepared_population_for() first, and
# falls back to computing from the archive when they return NULL: when a file
# is missing or stale, when the snapshots are read from a URL, or when the
# option mortality.prepared_data is FALSE (the build and the check set it, so
# their reference is always the computation itself). A request the archive
# would refuse - an area or year it lacks - also falls back, so it is refused
# with the archive's own message.

prepared_state <- new.env(parent = emptyenv())

# Off with options(mortality.prepared_data = FALSE), or from outside R with
# MORTALITY_PREPARED_DATA=false.
prepared_data_enabled <- function() {
  !identical(getOption("mortality.prepared_data"), FALSE) &&
    !tolower(Sys.getenv("MORTALITY_PREPARED_DATA", unset = "true")) %in% c("false", "0", "no") &&
    !is_url_path(get_snapshot_dir()) &&
    file.exists(prepared_manifest_path())
}

# The manifest, and the code fingerprint it is compared against, once per
# process.
prepared_session_manifest <- function() {
  key <- prepared_manifest_path()
  if (!identical(prepared_state$manifest_key, key)) {
    prepared_state$manifest <- read_prepared_manifest()
    prepared_state$code_hash <- list(deaths = prepared_code_hash(), regions = prepared_region_code_hash())
    prepared_state$current <- new.env(parent = emptyenv())
    prepared_state$manifest_key <- key
  }
  prepared_state$manifest
}

# Whether a file may be used: built by this code, from these inputs. Checked
# once per file per process.
prepared_file_current <- function(kind, year, cause = NA_character_) {
  manifest <- prepared_session_manifest()
  id <- paste(kind, year, cause, sep = "|")
  known <- prepared_state$current[[id]]
  if (!is.null(known)) return(known)

  hit <- which(manifest$kind == kind & manifest$year == as.integer(year) & manifest$cause %in% cause)
  path <- prepared_target_path(kind, year, cause)
  ok <- length(hit) == 1 && file.exists(path) &&
    identical(manifest$code_hash[[hit]], prepared_state$code_hash[[if (identical(kind, "regions")) "regions" else "deaths"]]) &&
    identical(manifest$inputs_hash[[hit]], prepared_inputs_hash(prepared_input_paths(kind, year, cause)))
  assign(id, ok, envir = prepared_state$current)
  ok
}

# Files read stay in memory, up to a limit, so moving between selections does
# not read them again.
read_prepared_file <- memoise::memoise(
  function(path) readRDS(path),
  cache = cachem::cache_mem(max_size = 256 * 1024^2)
)

# load_snapshot_deaths(years, area, cause), from the prepared files.
prepared_deaths_for <- function(years, area, cause) {
  if (!prepared_data_enabled() || length(cause) != 1) return(NULL)
  years <- sort(unique(as.integer(years)))
  area <- as.character(area)
  if (length(years) == 0 || length(area) == 0) return(NULL)
  if (!all(vapply(years, function(year) prepared_file_current("deaths", year, cause), logical(1)))) return(NULL)

  calendar <- intersect(years, active_calendar_years())
  frames <- lapply(years, function(year) {
    frame <- read_prepared_file(prepared_death_path(year, cause))
    decode_prepared_frame(frame, frame$area %in% area)
  })
  # Outside the rebuilt latest year the archive refuses an area or year it
  # does not have; let it.
  for (i in seq_along(years)) {
    if (!years[[i]] %in% calendar && !all(area %in% frames[[i]]$area)) return(NULL)
  }
  dplyr::bind_rows(frames)
}

# The population get_data_for_snapshot() builds, with the prepared files in
# place of the archive reads: the requested years must all be there for every
# area, a previous year outside them is used only if it is there for every
# area, and apply_population_basis() then does what it does today.
prepared_population_for <- function(years, area, basis) {
  if (!prepared_data_enabled()) return(NULL)
  years <- as.integer(years)
  area <- as.character(area)
  if (length(years) == 0 || length(area) == 0) return(NULL)

  read_year <- function(year) {
    if (!prepared_file_current("population", year)) return(NULL)
    frame <- read_prepared_file(prepared_population_path(year))$end
    frame <- decode_prepared_frame(frame, frame$area %in% area)
    if (!all(area %in% frame$area)) return(NULL)
    frame
  }

  wanted <- sort(unique(years))
  main <- lapply(wanted, read_year)
  if (any(vapply(main, is.null, logical(1)))) return(NULL)

  extra <- setdiff(population_years_to_load(years, basis), wanted)
  in_archive <- vapply(extra, function(year) file.exists(prepared_input_paths("population", year)[[1]]), logical(1))
  # A previous year the archive has but the prepared files do not cover is
  # not guessed at.
  if (!all(vapply(extra[in_archive], function(year) prepared_file_current("population", year), logical(1)))) return(NULL)
  previous <- lapply(extra[in_archive], read_year)

  dplyr::bind_rows(c(main, previous)) %>%
    apply_population_basis(years, basis)
}

# The frame split by year, and for each year the stored result for exactly
# that year's rows, or NULL.
prepared_region_entries <- function(df, region, vintage, cause) {
  years <- sort(unique(as.integer(df$year)))
  slices <- lapply(years, function(year) df[as.integer(df$year) == year, , drop = FALSE])
  entries <- lapply(seq_along(years), function(i) {
    if (!prepared_file_current("regions", years[[i]], cause)) return(NULL)
    stored <- read_prepared_file(prepared_region_path(years[[i]], cause))
    key <- prepared_frame_key(slices[[i]])
    entry <- stored[[prepared_region_entry_id(vintage, region, key)]]
    if (is.null(entry)) entry <- stored[[prepared_region_entry_id("any", region, key)]]
    entry
  })
  list(years = years, slices = slices, entries = entries)
}

# substitute_regional_deaths(df, expanded_regions, vintage, lookup, source,
# load_municipal_deaths) for a single region, with each year's result taken
# from the prepared files when that year's rows are exactly a frame the build
# substituted. Other years are substituted here, each on its own, as the
# function itself does; the years are then put together and ordered as it
# orders them. NULL - compute as before - when no year is stored, or for
# anything but one region and one cause.
prepared_region_substitution <- function(df, expanded_regions, vintage, lookup, source, load_municipal_deaths) {
  if (!prepared_data_enabled() || !identical(normalize_region_source(source), "ine_rows") ||
      length(expanded_regions) != 1 || is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  cause <- unique(as.character(df$cause))
  if (length(cause) != 1) return(NULL)
  region <- as.character(expanded_regions)
  vintage <- as.character(vintage)

  stored <- prepared_region_entries(df, region, vintage, cause)
  if (all(vapply(stored$entries, is.null, logical(1)))) return(NULL)

  parts <- lapply(seq_along(stored$years), function(i) {
    entry <- stored$entries[[i]]
    if (is.null(entry)) {
      return(substitute_regional_deaths(
        stored$slices[[i]], region, vintage, lookup,
        source = source, load_municipal_deaths = load_municipal_deaths
      ))
    }
    out <- decode_prepared_frame(entry$rows)
    attr(out, "regional_substitutions") <- entry$applied
    out
  })

  applied <- dplyr::bind_rows(lapply(parts, attr, "regional_substitutions"))
  out <- dplyr::bind_rows(parts) %>% dplyr::arrange(.data$year, .data$area, .data$sex, .data$age_band)
  attr(out, "regional_substitutions") <- applied
  out
}
