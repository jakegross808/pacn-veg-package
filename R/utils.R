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
    cache_lastrefreshed_path <- normalizePath(paste0(rappdirs::user_cache_dir(appname = "pacnvegetation"), "/pacnveg_cache_lastrefreshed.rds"), mustWork = FALSE)
    cache_exists <- file.exists(cache_path) && file.exists(cache_expiration_path)
    cache_valid <- cache_exists && (is.null(readRDS(cache_expiration_path)) ||  # If cache expiration set to NULL, always read from cache
                                     readRDS(cache_expiration_path) > Sys.time())

    # If cache = TRUE and isn't expired, read data from there, otherwise load from databases
    if (cache & cache_valid) {
      cache_last_refreshed <- readRDS(cache_lastrefreshed_path)
      cat("Loading data from local cache\n", "Data last read from database on ", cache_last_refreshed)
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
      # DBI::dbDisconnect(ftpc_conn)
      eips_data <- ReadEIPS(eips_paths)

      data <- c(ftpc_data, eips_data)

      if (cache) {
        # Save data and expiration date to cache
        refreshed <- Sys.time()
        expiration <- refreshed + lubridate::days(expire_interval_days)
        saveRDS(data, file = cache_path)
        saveRDS(expiration, file = cache_expiration_path)
        saveRDS(refreshed, file = cache_lastrefreshed_path)
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
  # A. Spatial -----------------------------------------------------------------
  # . . 1. tbl_Sites----

  #Sites (e.g. Park Codes)

  #Short
  tbl_Sites_short <- dplyr::tbl(conn, "tbl_Sites") %>%
    dplyr::select(Site_ID, Unit_Code)

  #Extra
  tbl_Sites_extra <- dplyr::tbl(conn, "tbl_Sites") %>%
    dplyr::select(Site_ID, Unit_Code, Site_Name)

  # . . 2. tbl_Locations----

  #Locations (e.g. Sampling Frame)

  #Short
  tbl_Locations_short <- dplyr::tbl(conn, "tbl_Locations") %>%
    dplyr::select(Location_ID, Site_ID, Community, Sampling_Frame)

  #Extra
  tbl_Locations_extra <- dplyr::tbl(conn, "tbl_Locations") %>%
    dplyr::select(Location_ID, Site_ID, Community, Sampling_Frame, Zone, Management_Unit)

  # . . 3. tbl_Plot----

  #Plots (e.g. Plot numbers, Plot type (Fixed vs. Rotational, Plot coordinates))

  #Short
  tbl_Plot_short <- dplyr::tbl(conn, "tbl_Plot") %>%
    dplyr::select(Plot_ID, Location_ID, Plot_Number, Plot_Type)

  #Extra
  tbl_Plot_extra <- dplyr::tbl(conn, "tbl_Plot") %>%
    dplyr::select(Plot_ID, Location_ID, Plot_Number, Azimuth_Plot,
           Start_Lat, Start_Long, Center_Lat, Center_Long, End_Lat, End_Long,
           GCS, GCS_Datum, Lat_Dir, Long_Dir, Plot_Notes)

  # B. Temporal ----------------------------------------------------------------
  # . . 1. tbl_Events----

  #Events (e.g. The date the plot was sampled, QA/QC records)

  #Short
  tbl_Events_short <- dplyr::tbl(conn, "tbl_Events") %>%
    dplyr::select(Event_ID, Plot_ID, Start_Date, QA_Plot)

  #Extra
  tbl_Events_extra <- dplyr::tbl(conn, "tbl_Events") %>%
    dplyr::select(Event_ID, Plot_ID, Start_Date, Images, Max_veg_ht,
           Entered_date, Updated_date, Verified, Verified_by, Verified_date,
           Certified, Certified_by, Certified_date, Completion_time,
           Event_Notes, QA_notes)


  # . . . . **join** Spatial & Temp ---------------------------------------------------

  # . . . . Events ----
  Events <- tbl_Events_short %>%
    dplyr::left_join(tbl_Plot_short, by = "Plot_ID") %>%
    dplyr::left_join(tbl_Locations_short, by = "Location_ID") %>%
    dplyr::left_join(tbl_Sites_short, by = "Site_ID") %>%
    dplyr::select(Start_Date, Unit_Code, Community, Sampling_Frame, Plot_Type,
           Plot_Number, QA_Plot, Event_ID)


  # . . . . Events_extra ----
  Events_extra <- tbl_Events_extra %>%
    dplyr::left_join(tbl_Plot_extra, by = "Plot_ID") %>%
    dplyr::left_join(tbl_Locations_extra, by = "Location_ID") %>%
    #Move long text columns to end because of SQL driver error:
    dplyr::relocate(Plot_Notes, .after = last_col()) %>%
    dplyr::relocate(Event_Notes, .after = last_col()) %>%
    dplyr::relocate(QA_notes, .after = last_col()) %>%
    dplyr::left_join(tbl_Sites_extra, by = "Site_ID") %>%
    #Move long text columns to end because of SQL driver error:
    dplyr::relocate(Plot_Notes, .after = last_col()) %>%
    dplyr::relocate(Event_Notes, .after = last_col()) %>%
    dplyr::relocate(QA_notes, .after = last_col())

  # . . Events_extra_QAQC
  Events_extra_QAQC <- Events_extra %>%
    dplyr::select(Start_Date, Unit_Code, Sampling_Frame, Plot_Number,
           Entered_date, Updated_date, Verified, Verified_by, Verified_date,
           Certified, Certified_by, Certified_date, Completion_time,
           Event_Notes, Plot_Notes, QA_notes) %>%
    dplyr::collect()

  # . . Events_extra_xy
  Events_extra_xy <- Events_extra %>%
    dplyr::select(Start_Date, Unit_Code, Sampling_Frame, Plot_Number,
           Azimuth_Plot, Start_Lat, Start_Long, Center_Lat, Center_Long,
           End_Lat, End_Long, GCS, GCS_Datum, Lat_Dir, Long_Dir) %>%
    dplyr::collect()

  # . . Events_extra_other
  Events_extra_other <- Events_extra %>%
    dplyr::select(Start_Date, Unit_Code, Sampling_Frame, Zone, Management_Unit,
                  Plot_Number, Max_veg_ht, Site_Name, Images) %>%
    dplyr::collect()


  # C. Species ----------------------------------------------------------------

  # . . 1. tlu_Species----

  #Short
  tlu_Species_short <- dplyr::tbl(conn, "tlu_Species") %>%
    dplyr::select(Species_ID, Scientific_name, Code, Life_form)

  #Extra
  tlu_Species_extra <- dplyr::tbl(conn, "tlu_Species") %>%
    dplyr::select(Species_ID, Code, Taxonomic_Order, Taxonomic_Family, Genus, Species,
           Subdivision, Authority, Synonym, Authority_Source, Citation,
           Common_name, Life_cycle, Complete, Update_date, Update_by,
           Update_comments)

  # . . 2. xref_Park_Species_Nativity----

  #Short
  xref_Park_Species_Nativity_short <- dplyr::tbl(conn, "xref_Park_Species_Nativity") %>%
    dplyr::select(Species_ID, Park, Nativity)

  #Extra
  xref_Park_Species_Nativity_extra <- dplyr::tbl(conn, "xref_Park_Species_Nativity") %>%
    dplyr::select(Species_ID, Park, Life_form, Nativity, Park_common_name,
           Distribution, Conservation_Status)

  # . . . . **join** Species & Nativity-------------------------------------------------

  # . . . . Species ----
  Species <- tlu_Species_short %>%
    dplyr::right_join(xref_Park_Species_Nativity_short, by = "Species_ID")

  # . . . . Species_extra ----
  Species_extra <- tlu_Species_extra %>%
    dplyr::right_join(xref_Park_Species_Nativity_extra, by = "Species_ID") %>%
    dplyr::collect()

  # D. Monitoring Data----------------------------------------------------------

  # . . 1. tbl_Lg_Woody_Individual----
  # Large Trees & Large Tree Ferns (>10 cm DBH)
  tbl_Lg_Woody_Individual <- dplyr::tbl(conn, "tbl_Lg_Woody_Individual") %>%
    dplyr::select(Large_Woody_ID, Event_ID, Species_ID, Quad, Status, Height,
                  Height_Dead, Boles, DBH, DBH_Other = DBH_Basal, Vigor,
                  Fruit_Flower, Rooting, Foliar, Caudex_Length,
                  Shrublike_Growth, Resprouts, Measurement_Type)

  # . . . . tbl_Multiple_Boles----
  # Sub-table - Bole DBH for trees that have multiple boles.
  tbl_Multiple_Boles <- dplyr::tbl(conn, "tbl_Multiple_Boles") %>%
    dplyr::select(Large_Woody_ID, DBH_Bole = DBH, Status_Bole = Status)

  # . . . . LgTrees ----
  LgTrees <- Events %>%
    dplyr::right_join(tbl_Lg_Woody_Individual, by = "Event_ID") %>%
    dplyr::left_join(tbl_Multiple_Boles, by = "Large_Woody_ID") %>%
    dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
    dplyr::select(-Large_Woody_ID, -Event_ID, -Species_ID) #%>%
    # dplyr::collect()


  # . . 2. tbl_Tree_Canopy_Height----
  # Height of canopy, sub-canopy, and emergent trees

  tbl_Tree_Canopy_Height <- dplyr::tbl(conn, "tbl_Tree_Canopy_Height") %>%
    dplyr::select(Event_ID, Species_ID, Quad, Status, Top, Base, Base_ht,
                  Distance, Height, Method, DBH, Comments)
  # . . . . Canopy ----
  Canopy <- Events %>%
    dplyr::right_join(tbl_Tree_Canopy_Height, by = "Event_ID") %>%
    dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
    dplyr::select(-Event_ID, -Species_ID) %>%
    #Move long text columns to end because of SQL driver error:
    dplyr::relocate(Comments, .after = last_col()) %>%
    dplyr::collect()


  # . . 3. tbl_Presence----
  # List of species present within plot
  tbl_Presence <- dplyr::tbl(conn, "tbl_Presence") %>%
    dplyr::select(Event_ID, Species_ID, Fruit_Flower, Dead,
                  Outside_Plot, cf, Comments)
  # . . . . Presence ----
  Presence <- Events %>%
    dplyr::right_join(tbl_Presence, by = "Event_ID") %>%
    dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
    dplyr::select(-Event_ID, -Species_ID) %>%
    #Move long text columns to end because of SQL driver error:
    dplyr::relocate(Comments, .after = last_col()) %>%
    dplyr::collect()



  # . . 4. tbl_Sm_Woody_Tally----
  # Vines, seedlings, shrubs, small trees, and small tree ferns.
  tbl_Sm_Woody_Tally <- dplyr::tbl(conn, "tbl_Sm_Woody_Tally") %>%
    dplyr::select(Event_ID, Species_ID, Transect, DBH, Status,
                  Foliar, Rooting, Count, Comments)
  # . . . . SmWoody ----
  SmWoody <- Events %>%
    dplyr::right_join(tbl_Sm_Woody_Tally, by = "Event_ID") %>%
    dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
    dplyr::select(-Event_ID, -Species_ID) %>%
    #Move long text columns to end because of SQL driver error:
    dplyr::relocate(Comments, .after = last_col()) %>%
    dplyr::collect()



  # . . 5. tbl_Understory_Cover----
  # point-intercept cover of  species and substrate
  # two stratum: 1)low [0-1m] 2) high [1-2m]
  UnderstoryCover <- dplyr::tbl(conn, "tbl_Understory_Cover") %>%
    dplyr::select(Event_ID, Point_ID, Point, Substrate)
  # . . . . xref_Understory_Low----
  UnderstoryLow <- dplyr::tbl(conn, "xref_Understory_Low") %>%
    dplyr::select(Point_ID, Species_ID, Dead)
  # . . . . xref_Understory_High----
  UnderstoryHigh <- dplyr::tbl(conn, "xref_Understory_High") %>%
    dplyr::select(Point_ID, Species_ID, Dead)

  UnderstoryLow <- dplyr::inner_join(UnderstoryCover, UnderstoryLow, by = "Point_ID") %>%
    dplyr::mutate(Stratum = "Low")
  UnderstoryHigh <- dplyr::inner_join(UnderstoryCover, UnderstoryHigh, by = "Point_ID") %>%
    dplyr::mutate(Stratum = "High")

  UnderstoryCover <- dplyr::union_all(UnderstoryLow, UnderstoryHigh)

  # . . . . Understory ----
  # Understory <- Events %>%
  #   dplyr::right_join(UnderstoryCover, by = "Event_ID") %>%
  #   dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
  #   dplyr::select(-Event_ID, -Species_ID)

  Understory <- UnderstoryCover %>%
    dplyr::inner_join(Events, by = "Event_ID") %>%
    dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
    dplyr::select(-Event_ID, -Species_ID)

  # . . 6. tbl_Woody_Debris----
  # Dead, downed wood and tree fern logs
  # with a diameter >=7.6 at the point of planar intersection.
  tbl_Woody_Debris <- dplyr::tbl(conn, "tbl_Woody_Debris") %>%
    dplyr::select(Woody_Debris_ID, Event_ID, Transect, Length)

  # . . . . tbl_Debris_Species----
  # Subtable - Additional information on debris type (wood/tree fern), diameter,
  # and decay class
  tbl_Debris_Species <- dplyr::tbl(conn, "tbl_Debris_Species") %>%
    dplyr::select(Woody_Debris_ID, Debris_type, Diameter, Decay_Class, Comments)

  # . . . . Debris ----
  Debris <- Events %>%
    dplyr::right_join(tbl_Woody_Debris, by = "Event_ID") %>%
    dplyr::left_join(tbl_Debris_Species, by = "Woody_Debris_ID") %>%
    dplyr::select(-Woody_Debris_ID, -Event_ID) %>%
    dplyr::collect()



  # List ----
  data <- list(
    Events_extra_QAQC = Events_extra_QAQC,
    Events_extra_xy = Events_extra_xy,
    Events_extra_other = Events_extra_other,
    Species_extra = Species_extra,
    LgTrees = LgTrees,
    Canopy = Canopy,
    Presence = Presence,
    SmWoody = SmWoody,
    Understory = Understory,
    Debris = Debris
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
