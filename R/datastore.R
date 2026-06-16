# Datastore data loading
#
# This file implements the `data_source = "datastore"` path for LoadPACNVeg().
# It downloads FTPC (and eventually EIPS) data packages from NPS DataStore via
# NPSutils, then transforms the data package tables to match the legacy schema
# used elsewhere in pacnvegetation.
#
# Two assumptions worth flagging if anything misbehaves:
#   1. NPSutils::load_data_package() returns a named list of tibbles where the
#      list names are the CSV file basenames (e.g. "PACN_FTPC_Understory").
#      If the returned names are different, adjust the dispatch in
#      transform_ftpc_tables() accordingly.
#   2. The `dynamicProperties` column in some tables is a JSON string. We use
#      jsonlite::fromJSON() row-by-row. Rows that fail to parse yield NA for
#      the extracted fields rather than failing the whole load.


#' Derive monitoring cycle from year
#'
#' Single source of truth for cycle derivation. Pre-2015 = cycle 1,
#' 2015-2020 = cycle 2, 2021+ = cycle 3.
#'
#' @param year Integer vector of years.
#'
#' @return Integer vector of cycle numbers.
#' @export
#'
#' @examples
#' derive_cycle(c(2010, 2016, 2022))
derive_cycle <- function(year) {
  year <- as.integer(year)
  dplyr::case_when(
    year <= 2014 ~ 1L,
    year >= 2015 & year <= 2020 ~ 2L,
    year >= 2021 ~ 3L,
    TRUE ~ NA_integer_
  )
}


#' Read PACN data from a DataStore data package
#'
#' Downloads a data package from NPS DataStore via NPSutils and transforms
#' the contents to match the legacy schema used elsewhere in pacnvegetation.
#'
#' @param reference_id Numeric DataStore reference ID (e.g. 2311238 for FTPC).
#' @param protocol One of "FTPC" or "EIPS". Determines which transformer to apply.
#'   EIPS is not yet implemented.
#' @param download_dir Folder where NPSutils will download the data package.
#'   Defaults to a subfolder of the pacnvegetation cache directory.
#'
#' @return A named list of tibbles matching the names used in GetColSpec().
#' @export
ReadDataStore <- function(reference_id, protocol = c("FTPC", "EIPS"),
                          download_dir = NULL) {
  protocol <- match.arg(protocol)

  if (!requireNamespace("NPSutils", quietly = TRUE)) {
    stop("Package 'NPSutils' is required for data_source = 'datastore'. ",
         "Install it with: remotes::install_github('doi-nps/NPSutils')",
         call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for data_source = 'datastore'. ",
         "Install it with: install.packages('jsonlite')",
         call. = FALSE)
  }

  if (is.null(download_dir)) {
    download_dir <- file.path(rappdirs::user_cache_dir("pacnvegetation"),
                              "datastore_downloads")
  }
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  # NPSutils::get_data_package() downloads into ./data/<reference_id> relative
  # to the working directory. Temporarily set wd so the download lands in our
  # managed cache folder.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(download_dir)

  message("Fetching data package ", reference_id, " from DataStore...")
  NPSutils::get_data_package(reference_id)

  message("Loading data package ", reference_id, "...")
  raw <- NPSutils::load_data_package(reference_id, assign_attributes = TRUE)

  if (protocol == "FTPC") {
    out <- transform_ftpc_tables(raw)
  } else {
    stop("EIPS data package loading is not yet implemented.", call. = FALSE)
  }

  out
}


#' Dispatch data package tables to their transformers
#'
#' @param raw Named list of tibbles returned by NPSutils::load_data_package().
#'
#' @return Named list of tibbles in legacy schema.
#' @keywords internal
transform_ftpc_tables <- function(raw) {
  # Map of data package table names to (legacy name, transformer function).
  # Tables without a transformer are passed through unchanged under their
  # original data package name.
  dispatch <- list(
    PACN_FTPC_Tree_Canopy_Height   = list(out = "Canopy",         fn = transform_canopy),
    PACN_FTPC_Woody_Debris         = list(out = "Debris",         fn = transform_debris),
    PACN_FTPC_Plot_Locations       = list(out = "Events_extra_xy", fn = transform_plot_locations),
    PACN_FTPC_Lg_Woody_Individuals = list(out = "LgTrees",        fn = transform_lg_trees),
    PACN_FTPC_Presence             = list(out = "Presence",       fn = transform_presence),
    PACN_FTPC_Sm_Woody_Tally       = list(out = "SmWoody",        fn = transform_sm_woody),
    PACN_FTPC_Understory           = list(out = "Understory",     fn = transform_understory)
  )

  out <- list()

  for (dp_name in names(raw)) {
    if (dp_name %in% names(dispatch)) {
      spec <- dispatch[[dp_name]]
      out[[spec$out]] <- spec$fn(raw[[dp_name]])
    } else {
      # Pass-through tables: Taxon_Lookup, Contacts_Lookup, Images, anything new
      out[[dp_name]] <- raw[[dp_name]]
    }
  }

  out
}


# ---- dynamicProperties JSON parsing ----------------------------------------

#' Parse the JSON-encoded `dynamicProperties` column into named fields
#'
#' @param df A tibble containing a `dynamicProperties` column.
#' @param fields Named character vector mapping JSON keys -> output column names.
#' @param drop_source Drop the `dynamicProperties` column after extraction?
#'
#' @return The input tibble with extracted columns added.
#' @keywords internal
parse_dynamic_properties <- function(df, fields, drop_source = TRUE) {
  if (!"dynamicProperties" %in% names(df)) {
    return(df)
  }

  parsed <- lapply(df$dynamicProperties, function(x) {
    if (is.na(x) || !nzchar(x)) return(stats::setNames(as.list(rep(NA, length(fields))),
                                                       unname(fields)))
    out <- tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
    if (is.null(out)) return(stats::setNames(as.list(rep(NA, length(fields))),
                                             unname(fields)))
    # Pull each requested JSON key; NA if missing
    vals <- lapply(names(fields), function(key) {
      v <- out[[key]]
      if (is.null(v) || (length(v) == 1 && is.na(v))) NA else v
    })
    stats::setNames(vals, unname(fields))
  })

  # Bind into columns. suppressWarnings handles type coercion across rows.
  extracted <- suppressWarnings(
    dplyr::bind_rows(lapply(parsed, function(x) tibble::as_tibble(x)))
  )

  df <- dplyr::bind_cols(df, extracted)
  if (drop_source) df$dynamicProperties <- NULL
  df
}


# ---- Per-table transformers ------------------------------------------------

#' @keywords internal
transform_canopy <- function(df) {
  df <- parse_dynamic_properties(df, c(
    topInPercent       = "Top",
    baseInPercent      = "Base",
    baseHeightInMeters = "Base_Ht",
    distanceInMeters   = "Distance",
    heightInMeters     = "Height"
  ))

  df <- dplyr::rename(df, dplyr::any_of(c(
    Unit_Code             = "custom_unitCode",
    Community             = "custom_plantCommunity",
    Sampling_Frame        = "custom_samplingFrameEnglish",
    Sampling_Frame_Formal = "custom_samplingFrameHawaiian",
    Year                  = "year",
    Plot_Type             = "custom_plotType",
    Plot_Number           = "custom_plotNumber",
    QA_Plot               = "custom_qa",
    Quad                  = "custom_quad",
    Status                = "custom_status",
    Method                = "custom_method",
    DBH                   = "custom_DBHInCentimeters",
    Scientific_Name       = "scientificName",
    Code                  = "custom_speciesCode",
    Life_Form             = "custom_lifeForm",
    Nativity              = "establishmentMeans",
    Comments              = "occurrenceRemarks",
    Certified             = "custom_certified",
    Verified              = "custom_verified"
  )))

  df$Cycle <- derive_cycle(df$Year)
  df <- coerce_logical_cols(df, c("QA_Plot", "Certified", "Verified"))
  df
}

#' @keywords internal
transform_debris <- function(df) {
  df <- parse_dynamic_properties(df, c(
    lengthInMeters = "Length"
  ))

  df <- dplyr::rename(df, dplyr::any_of(c(
    Unit_Code             = "custom_unitCode",
    Community             = "custom_plantCommunity",
    Sampling_Frame        = "custom_samplingFrameEnglish",
    Sampling_Frame_Formal = "custom_samplingFrameHawaiian",
    Year                  = "year",
    Plot_Type             = "custom_plotType",
    Plot_Number           = "custom_plotNumber",
    QA_Plot               = "custom_qa",
    Transect              = "custom_transect",
    Debris_Type           = "custom_debrisType",
    Diameter              = "measurementValue",
    Decay_Class           = "custom_decayClass",
    Comments              = "custom_debrisComments",
    Certified             = "custom_certified",
    Verified              = "custom_verified"
  )))

  df$Cycle <- derive_cycle(df$Year)
  df <- coerce_logical_cols(df, c("QA_Plot", "Certified", "Verified"))
  df
}

#' @keywords internal
transform_plot_locations <- function(df) {
  # Plot_Locations has one row per plot. Many legacy Events_extra_xy fields
  # (Year, Cycle, Start_/Center_/End_ Lat/Long, Lat_Dir, Long_Dir, Certified,
  # Verified) don't have direct sources in this table. Downstream code that
  # needs those will need to look elsewhere or join from observation tables.
  df <- dplyr::rename(df, dplyr::any_of(c(
    Unit_Code             = "custom_unitCode",
    Sampling_Frame        = "custom_samplingFrameEnglish",
    Sampling_Frame_Formal = "custom_samplingFrameHawaiian",
    Plot_Type             = "custom_plotType",
    Plot_Number           = "custom_plotNumber",
    Azimuth_Plot          = "custom_azimuthPlot",
    Center_Lat            = "decimalLatitude",
    Center_Long           = "decimalLongitude",
    GCS                   = "geodeticDatum"
  )))
  df
}

#' @keywords internal
transform_lg_trees <- function(df) {
  # Height / Height_Dead arrived already extracted as their own columns in the
  # sample data, but dynamicProperties is also present. If the extracted
  # columns are missing for some reason, fall back to JSON parsing.
  if (!all(c("Height", "Height_Dead") %in% names(df))) {
    df <- parse_dynamic_properties(df, c(
      heightInCentimeters     = "Height",
      heightDeadInCentimeters = "Height_Dead"
    ), drop_source = FALSE)
  }

  df <- dplyr::rename(df, dplyr::any_of(c(
    Unit_Code             = "custom_unitCode",
    Community             = "custom_plantCommunity",
    Sampling_Frame        = "custom_samplingFrameEnglish",
    Sampling_Frame_Formal = "custom_samplingFrameHawaiian",
    Year                  = "year",
    Plot_Type             = "custom_plotType",
    Plot_Number           = "custom_plotNumber",
    QA_Plot               = "custom_qa",
    Large_Woody_ID        = "custom_largeWoodyID",
    Quad                  = "custom_quad",
    Status                = "custom_status",
    Boles                 = "custom_boles",
    DBH                   = "custom_DBH",
    Measurement_Type      = "measurementType",
    Measurement           = "measurementValue",
    Vigor                 = "custom_vigor",
    Fruit_Flower          = "custom_fruitFlower",
    Rooting               = "custom_rooting",
    Foliar                = "custom_foliar",
    Caudex_Length         = "custom_caudexLengthInMeters",
    Shrublike_Growth      = "custom_shrublikeGrowth",
    Resprouts             = "custom_resprouts",
    DBH_Bole              = "custom_dbhBole",
    Status_Bole           = "custom_statusBole",
    Scientific_Name       = "scientificName",
    Code                  = "custom_speciesCode",
    Life_Form             = "custom_lifeForm",
    Nativity              = "establishmentMeans",
    Certified             = "custom_certified",
    Verified              = "custom_verified"
  )))

  df$Cycle <- derive_cycle(df$Year)
  df <- coerce_logical_cols(df, c("QA_Plot", "Fruit_Flower",
                                  "Shrublike_Growth", "Resprouts",
                                  "Certified", "Verified"))
  df
}

#' @keywords internal
transform_presence <- function(df) {
  df <- dplyr::rename(df, dplyr::any_of(c(
    Unit_Code             = "custom_unitCode",
    Community             = "custom_plantCommunity",
    Sampling_Frame        = "custom_samplingFrameEnglish",
    Sampling_Frame_Formal = "custom_samplingFrameHawaiian",
    Year                  = "year",
    Plot_Type             = "custom_plotType",
    Plot_Number           = "custom_plotNumber",
    QA_Plot               = "custom_qa",
    Fruit_Flower          = "custom_fruitFlower",
    Dead                  = "custom_dead",
    Outside_Plot          = "custom_outsidePlot",
    cf                    = "custom_cf",
    Scientific_Name       = "scientificName",
    Code                  = "custom_speciesCode",
    Life_Form             = "custom_lifeForm",
    Nativity              = "establishmentMeans",
    Comments              = "eventRemarks",
    Certified             = "custom_certified",
    Verified              = "custom_verified"
  )))

  df$Cycle <- derive_cycle(df$Year)
  df <- coerce_logical_cols(df, c("QA_Plot", "Fruit_Flower", "Dead",
                                  "Outside_Plot", "cf",
                                  "Certified", "Verified"))
  df
}

#' @keywords internal
transform_sm_woody <- function(df) {
  df <- dplyr::rename(df, dplyr::any_of(c(
    Unit_Code             = "custom_unitCode",
    Community             = "custom_plantCommunity",
    Sampling_Frame        = "custom_samplingFrameEnglish",
    Sampling_Frame_Formal = "custom_samplingFrameHawaiian",
    Year                  = "year",
    Plot_Type             = "custom_plotType",
    Plot_Number           = "custom_plotNumber",
    QA_Plot               = "custom_qa",
    Sample_Area           = "custom_samplingArea",
    LF_Sm_Woody           = "custom_lifeFormSW",
    DBH                   = "custom_DBH",
    Status                = "custom_status",
    Length                = "custom_length",
    Rooting               = "custom_rooting",
    Count                 = "individualCount",
    Scientific_Name       = "scientificName",
    Code                  = "custom_speciesCode",
    Life_Form             = "custom_lifeForm",
    Nativity              = "establishmentMeans",
    Comments              = "occurrenceRemarks",
    Certified             = "custom_certified",
    Verified              = "custom_verified"
  )))

  df$Cycle <- derive_cycle(df$Year)
  df <- coerce_logical_cols(df, c("QA_Plot", "Certified", "Verified"))
  df
}

#' @keywords internal
transform_understory <- function(df) {
  # Understory has both cycle_year and custom_cycle in the data package; prefer
  # custom_cycle if present, else derive from year.
  df <- dplyr::rename(df, dplyr::any_of(c(
    Unit_Code             = "custom_unitCode",
    Community             = "custom_plantCommunity",
    Sampling_Frame        = "custom_samplingFrameEnglish",
    Sampling_Frame_Formal = "custom_samplingFrameHawaiian",
    Year                  = "year",
    Cycle                 = "custom_cycle",
    Plot_Type             = "custom_plotType",
    Plot_Number           = "custom_plotNumber",
    QA_Plot               = "custom_qa",
    Point                 = "custom_point",
    Substrate             = "custom_substrate",
    Scientific_Name       = "scientificName",
    Code                  = "custom_speciesCode",
    Life_Form             = "custom_lifeForm",
    Nativity              = "establishmentMeans",
    Certified             = "custom_certified",
    Verified              = "custom_verified"
    # Stratum already has the legacy name in the sample data
  )))

  if (!"Cycle" %in% names(df)) {
    df$Cycle <- derive_cycle(df$Year)
  }
  df <- coerce_logical_cols(df, c("QA_Plot", "Certified", "Verified"))
  df
}


# ---- Small helpers ---------------------------------------------------------

#' Coerce 0/1 columns to logical, in place.
#' @keywords internal
coerce_logical_cols <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df) && !is.logical(df[[col]])) {
      v <- df[[col]]
      if (is.numeric(v)) {
        df[[col]] <- as.logical(v)
      } else if (is.character(v)) {
        df[[col]] <- toupper(v) %in% c("TRUE", "T", "1", "YES", "Y")
      }
    }
  }
  df
}
