#' @importFrom magrittr %>% %<>%

pkg_globals <- new.env(parent = emptyenv())

get_data <- function(data_type) {
  data <- get(data_type, pkg_globals)
  return(data)
}

#' Load raw data into package environment
#' @description Run this function before you do anything else.
#'
#' @param ftpc_params Connection information for FTPC database. Either a DSN name, a path to a csv containing connection information, or a named list of connection arguments. The csv or list should contain the following parameters:
#' * Driver
#' * Server
#' * Database
#' * Trusted_Connection
#' @param eips_paths Character vector of paths to EIPS database(s).
#' @param data_path A path to either:
#' * a folder containing the data in csv format
#' * a .zip file containing the data in csv format
#' * an .Rdata file
#' @param data_source Either "db" (fetch data from databases or cache) or "file" (fetch data from csv, zip, or rds file).
#' @param cache Should the data be cached locally to avoid reading from the databases every time?
#' @param expire_interval_days Amount of time (in days) before the cache expires and has to be refreshed from the Access db. Defaults to 7 days. Ignored if `ftpc_conn` and `eips_paths` are `NULL`.
#' @param force_refresh Refresh the cache from the databases even if it's not expired?
#'
#' @return Invisibly return a list containing all raw data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' eips <- c("path/to/database1_be.mdb", "path/to/database2_be.mdb")
#' ftpc <- "pacnveg"
#'
#' # Read data from veg databases and cache it locally for a week (default)
#' LoadPACNVeg(ftpc, eips)
#'
#' # Force refresh cached data that haven't expired
#' LoadPACNVeg(ftpc, eips, force_refresh = TRUE)
#'
#' # Read data from a folder of csv files. This will not be cached.
#' path_to_csv <- "path/to/csv/folder"
#' LoadPACNVeg(data_path = path_to_csv)
#' }
#'
LoadPACNVeg <- function(ftpc_params = "pacn", eips_paths, data_path, data_source = "db", cache = TRUE, expire_interval_days = 7, force_refresh = FALSE) {

  ## Read from cache or database
  if (data_source == "db") {
    # Standardize path names, create path to cache, check whether cache exists
    eips_paths <- normalizePath(eips_paths, mustWork = FALSE)
    cache_path <- normalizePath(paste0(rappdirs::user_cache_dir(appname = "pacnvegetation"), "/pacnveg_cache_data.rds"), mustWork = FALSE)
    cache_expiration_path <- normalizePath(paste0(rappdirs::user_cache_dir(appname = "pacnvegetation"), "/pacnveg_cache_expiration.rds"), mustWork = FALSE)
    cache_exists <- file.exists(cache_path) && file.exists(cache_expiration_path)
    cache_valid <- cache_exists && (is.null(readRDS(cache_expiration_path)) ||  # If cache expiration set to NULL, always read from cache
                                     readRDS(cache_expiration_path) > Sys.time())

    # If cache = TRUE and isn't expired, read data from there, otherwise load from databases
    if (cache & cache_valid) {
      cat("Loading data from local cache\n")
      data <- readRDS(cache_path)
    } else {
      # Verify that Access db(s) exist
      if (!(all(grepl(".*\\.mdb$", eips_paths)) && all(file.exists(eips_paths)))) {
        stop("Path(s) to EIPS Access database(s) are invalid")
      }

      # If FTPC params provided as cav, read it and store in a list
      if (length(ftpc_params) == 1 && grepl(".*\\.csv$", ftpc_params)) {
        ftpc_params <- readr::read_csv(ftpc_params, col_types = readr::cols(.default = readr::col_character()))
        ftpc_params <- as.list(ftpc_params)
      }
      # If FTPC params are in list form, use do.call to pass them all to dbConnect. Otherwise, just pass the DSN name straight to dbConnect.
      if (is.list(ftpc_params)) {
        ftpc_params$drv <- odbc::odbc()
        ftpc_conn <- do.call(DBI::dbConnect, ftpc_params)
      } else {
        ftpc_conn <- DBI::dbConnect(odbc::odbc(), ftpc_params)
      }

      ftpc_data <- ReadFTPC(ftpc_conn)
      DBI::dbDisconnect(ftpc_conn)
      eips_data <- ReadEIPS(eips_paths)

      data <- c(ftpc_data, eips_data)

      if (cache) {
        # Save data and expiration date to cache
        expiration <- Sys.time() + lubridate::days(expire_interval_days)
        saveRDS(data, file = cache_path)
        saveRDS(expiration, file = cache_expiration_path)
      }
    }
  # End read from cache or database
  # Read from csv, zip, or rds
  } else if (data_source == "file") {
    # Standardize data path
    data_path <- ifelse(missing(data_path), NA, normalizePath(data_path, mustWork = FALSE))
    # Figure out whether data path points to rds, folder of csv's, or zip
    is_rds <- !is.na(data_path) & grepl("*.\\.rds$", data_path, ignore.case = TRUE)  # Is the data a .rds file?
    is_zip <- !is.na(data_path) & grepl("*.\\.zip$", data_path, ignore.case = TRUE)  # Is the data in a .zip file?
    is_csv <- !is.na(data_path) & dir.exists(data_path)  # Is the data in a folder?

    if (is_zip) {
      temp_dir <- tempdir()
      zip::unzip(exdir = temp_dir)
      data <- ReadCSV(temp_dir)
      unlink(temp_dir, recursive = TRUE)
    } else if (is_csv) {
      data <- ReadCSV(data_path)
    } else if (is_rds) {
      data <- readRDS(data_path)
    } else {
      stop("data_path does not point to valid data")
    }
  } else {
    stop("data_source type is invalid. It should be set to either 'db' or 'file'.")
  }

  # Actually load the data into an environment for the package to use
  tbl_names <- names(data)
  lapply(tbl_names, function(n) {assign(n, data[[n]], envir = pkg_globals)})

  invisible(data)
}

#' Read data from FTPC database
#'
#' @param conn Database connection object returned by `DBI::dbConnect`
#'
#' @return A list of tibbles
#'
ReadFTPC <- function(conn) {

  # Tree density information collected for small trees (subalpine),
  # large trees (forest, coastal, subalpine), and large tree ferns (forest).
  LgWoodyIndividual <- dplyr::tbl(conn, "tbl_Lg_Woody_Individual") %>%
    dplyr::select(Large_Woody_ID, Event_ID, Species_ID, Life_Form, Quad, Status, Height,
           Height_Dead, Boles, DBH, DBH_Basal, Vigor, Fruit_Flower, Rooting, Foliar,
           Caudex_Length, Shrublike_Growth, Resprouts, Measurement_Type) %>%
    dplyr::collect()
  # Subtable - Bole DBH for trees that have multiple boles.
  LgWoodyMultipleBoles <- dplyr::tbl(conn, "tbl_Multiple_Boles") %>%
    dplyr::select(Large_Woody_ID, DBH_Bole = DBH, Root_Sprout, Status_Bole = Status) %>%
    dplyr::collect()
  # Subtable - Extra data for dead trees
  LgWoodySnags <- dplyr::tbl(conn, "tbl_Snags")%>%
    dplyr::select(Large_Woody_ID, Basal_diameter, Height_Snag = Height) %>%
    dplyr::collect()
  # Subtable - Sprout size data collected in recently disturbed sites for individuals
  # that have been top killed and are alive only by basal sprouts.
  LgWoodyBasalSprout <- dplyr::tbl(conn, "tbl_Basal_Sprout") %>%
    dplyr::select(Large_Woody_ID, Sprout_Ht, W1, W2, Area, Volume) %>%
    dplyr::collect()

  # Recorded within each plot by measuring the height of ~five canopy trees per
  # plot that represent the average canopy height within the plot.
  TreeCanopyHeight <- dplyr::tbl(conn, "tbl_Tree_Canopy_Height") %>%
    dplyr::select(Event_ID, Species_ID, Quad, Status, Top, Base, Base_ht,
           Distance, Height, Method, DBH, Comments) %>%
    dplyr::collect()

  # List of species present within plot.
  Presence <- dplyr::tbl(conn, "tbl_Presence") %>%
    dplyr::select(Event_ID, Species_ID, Fruit_Flower, Dead, Outside_Plot, cf, Comments) %>%
    dplyr::collect()

  # Count data, foliar height, and rooting height data tallied for
  # vines, seedlings, shrubs, small trees, and small tree ferns.
  SmWoodyTally <- dplyr::tbl(conn, "tbl_Sm_Woody_Tally") %>%
    dplyr::select(Event_ID, Species_ID, Transect, Life_Form, DBH, Status,
           Foliar, Rooting, Count, Comments) %>%
    dplyr::collect()

  # point-intercept cover of  species and substrate
  # two stratum: 1)low [0-1m] 2) high [1-2m]
  UnderstoryCover <- dplyr::tbl(conn, "tbl_Understory_Cover") %>%
    dplyr::select(Event_ID, Point_ID, Point, Substrate)
  UnderstoryLow <- dplyr::tbl(conn, "xref_Understory_Low") %>%
    dplyr::select(Event_ID, Point_ID, Species_ID, Dead)
  UnderstoryHigh <- dplyr::tbl(conn, "xref_Understory_High") %>%
    dplyr::select(Event_ID, Point_ID, Species_ID, Dead)

  UnderstoryLow <- dplyr::inner_join(UnderstoryCover, UnderstoryLow, by = "Point_ID") %>%
    dplyr::mutate(Stratum = "Low")
  UnderstoryHigh <- dplyr::inner_join(UnderstoryCover, UnderstoryHigh, by = "Point_ID") %>%
    dplyr::mutate(Stratum = "High")

  UnderstoryCover <- rbind(UnderstoryLow, UnderstoryHigh)

  # Dead, downed wood and tree fern logs
  # with a diameter >=7.6 at the point of planar intersection.
  WoodyDebris <- dplyr::tbl(conn, "tbl_Woody_Debris") %>%
    dplyr::select(Woody_Debris_ID, Event_ID, Transect, Length)
  # Subtable - Additional information on debris type (wood/tree fern), diameter,
  # and decay class
  WoodyDebrisSpecies <- dplyr::tbl(conn, "tbl_Debris_Species") %>%
    dplyr::select(Woody_Debris_ID, Debris_type, Diameter, Decay_Class, Comments)

  WoodyDebris <- dplyr::left_join(WoodyDebris, WoodyDebrisSpecies, by = "Woody_Debris_ID") %>%
    dplyr::collect()

  data <- list(
    LgWoodyIndividual = LgWoodyIndividual,
    LgWoodyMultipleBoles = LgWoodyMultipleBoles,
    LgWoodyBasalSprout = LgWoodyBasalSprout,
    LgWoodySnags = LgWoodySnags,
    TreeCanopyHeight = TreeCanopyHeight,
    Presence = Presence,
    SmWoodyTally = SmWoodyTally,
    # UnderstoryCover = UnderstoryCover,
    WoodyDebris = WoodyDebris
  )

  return(data)
}

#' Read data from EIPS database(s)
#'
#' @param db_paths A path or vector of paths to Access databases containing EIPS data
#'
#' @return A list of tibbles
#'
ReadEIPS <- function(db_paths) {
  data <- list()

  return(data)
}

#' Read data from a folder of csv files
#'
#' @param data_path A path to a folder containing the data in csv format
#'
#' @return A list of tibbles
#'
ReadCSV <- function(data_path) {
  data <- list()

  return(data)
}
