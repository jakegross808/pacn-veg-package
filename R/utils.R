#' @importFrom magrittr %>% %<>%

pkg_globals <- new.env(parent = emptyenv())

# Load data from global package environment
get_data <- function(data_name) {
  if (!missing(data_name)) {
    if (!(data_name %in% names(GetColSpec()))) {
      stop("Invalid data table name. Use names(pacnvegetation:::GetColSpec()) to see valid options for data_name.")
    }
    data <- get(data_name, pkg_globals)
  } else {
    data <- lapply(names(GetColSpec()), get, pkg_globals)
    names(data) <- names(GetColSpec())
  }

  return(data)
}

#' Clear cached data
#'
#' @param silent Silence feedback message?
#'
#' @return `TRUE` if cache was cleared, `FALSE` if no cache found
#' @export
#'
ClearPACNVegCache <- function(silent = FALSE) {
  cache_path <- normalizePath(paste0(rappdirs::user_cache_dir(appname = "pacnvegetation"), "/pacnveg_cache_data.rds"), mustWork = FALSE)
  cache_expiration_path <- normalizePath(paste0(rappdirs::user_cache_dir(appname = "pacnvegetation"), "/pacnveg_cache_expiration.rds"), mustWork = FALSE)
  cache_lastrefreshed_path <- normalizePath(paste0(rappdirs::user_cache_dir(appname = "pacnvegetation"), "/pacnveg_cache_lastrefreshed.rds"), mustWork = FALSE)
  cache_exists <- file.exists(cache_path)

  if (cache_exists) {
    unlink(cache_path)
    unlink(cache_expiration_path)
    unlink(cache_lastrefreshed_path)
    if (!silent) {
      message(paste("Cache cleared"))
    }
    return(TRUE)
  } else {
    message("No cache found")
    return(FALSE)
  }

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
#' @param data_source Either "db" (fetch data from databases or cache) or "file" (fetch data from folder or zip archive of csv's).
#' @param cache Should the data be cached locally to avoid reading from the databases every time?
#' @param expire_interval_days Amount of time (in days) before the cache expires and has to be refreshed from the Access db. Defaults to 7 days. Ignored if `ftpc_conn` and `eips_paths` are `NULL`.
#' @param force_refresh Refresh the cache from the databases even if it's not expired? Ignored if `cache == FALSE`.
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
    if (cache & cache_valid & !force_refresh) {
      message(paste0("Loading data from local cache\n", "Data last read from database on ", format(readRDS(cache_lastrefreshed_path), "%m/%d/%y")))
      data <- readRDS(cache_path)
    } else {
      # Verify that Access db(s) exist
      if (!(all(grepl(".*\\.mdb$", eips_paths)) && all(file.exists(eips_paths)))) {
        stop("Path(s) to EIPS Access database(s) are invalid")
      }

      # If FTPC params provided as csv, read it and store in a list
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
        # Create cache folder if it doesn't exist yet
        if (!dir.exists(dirname(cache_path))) {
          dir.create(dirname(cache_path), recursive = TRUE)
        }
        # Save data and expiration date to cache
        refreshed <- Sys.time()
        expiration <- refreshed + lubridate::days(expire_interval_days)
        saveRDS(data, file = cache_path)
        saveRDS(expiration, file = cache_expiration_path)
        saveRDS(refreshed, file = cache_lastrefreshed_path)
        message(paste0("Saved data to local cache\n", "Cache expires ", format(expiration, "%m/%d/%y")))
      }
    }
  # End read from cache or database
  # Read from csv, zip, or rds
  } else if (data_source == "file") {
    # Standardize data path
    data_path <- ifelse(missing(data_path), NA, normalizePath(data_path, mustWork = FALSE))
    # Figure out whether data path points to folder of csv's or zip
    is_zip <- !is.na(data_path) & grepl("*.\\.zip$", data_path, ignore.case = TRUE)  # Is the data in a .zip file?
    if(is_zip) {
      file_list <- basename(unzip(data_path, list = TRUE)$Name)
    } else {
      file_list <- list.files(data_path)
    }
    expected_files <- paste0(names(GetColSpec()), ".csv")

    if (!all(expected_files %in% file_list)) {
      missing_files <- setdiff(expected_files, file_list)
      missing_files <- paste(missing_files, collapse = "\n")
      stop(paste0("The folder provided is missing required data. Missing files:\n", missing_files))
    }

    if (is_zip) {
      temp_dir <- tempdir()
      # Use this trycatch so that even if there's an error unzipping or reading, the temp dir will be deleted
      tryCatch({
        unzip(data_path, overwrite = TRUE, exdir = temp_dir, junkpaths = TRUE)
        data <- ReadCSV(temp_dir)
        },
        finally = unlink(temp_dir, recursive = TRUE)
      )
    } else {
      data <- ReadCSV(data_path)
    }

  } else {
    stop("data_source type is invalid. It should be set to either 'db' or 'file'.")
  }

  # Tidy up the data
  data <- lapply(data, function(df) {
    df %>%
      dplyr::mutate_if(is.character, trimws, whitespace = "[\\h\\v]") %>%  # Trim leading and trailing whitespace
      dplyr::mutate_if(is.character, dplyr::na_if, "") %>%  # Replace empty strings with NA
      dplyr::mutate_if(is.character, dplyr::na_if, "NA") %>%  # Replace "NA" strings with NA
      dplyr::mutate_if(is.character, stringr::str_replace_all, pattern = "[\\v]+", replacement = ";  ")  # Replace newlines with semicolons - reading certain newlines into R can cause problems
  })

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
    dplyr::select(-Large_Woody_ID, -Event_ID, -Species_ID) %>%
    dplyr::collect()


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
    dplyr::select(Event_ID, Point_ID, Species_ID, Dead) %>%
    dplyr::mutate(Stratum = "Low")

  # . . . . xref_Understory_High----
  UnderstoryHigh <- dplyr::tbl(conn, "xref_Understory_High") %>%
    dplyr::select(Event_ID, Point_ID, Species_ID, Dead) %>%
    dplyr::mutate(Stratum = "High")

  UnderstorySpecies <- dplyr::union_all(UnderstoryLow, UnderstoryHigh)

  Understory <- Events %>%
    dplyr::right_join(UnderstoryCover, by = "Event_ID") %>%
    dplyr::left_join(UnderstorySpecies, by = c("Event_ID", "Point_ID")) %>%
    dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
    dplyr::select(-Event_ID, -Species_ID, -Point_ID) %>%
    dplyr::collect()


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
  conn_strings <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", db_paths)

  Events_extra_QAQC_EIPS <- tibble::tibble()
  Events_extra_xy_EIPS <- tibble::tibble()
  Events_extra_other_EIPS <- tibble::tibble()
  Species_extra_EIPS <- tibble::tibble()
  EIPS_data <- tibble::tibble()

  for (conn_string in conn_strings) {
    conn <- DBI::dbConnect(odbc::odbc(), .connection_string = conn_string)
    #Sites (e.g. Park Codes)
    #Short
    tbl_Sites_short <- dplyr::tbl(conn, "tbl_Sites") %>%
      dplyr::select(Site_ID, Unit_Code)
    #Extra
    tbl_Sites_extra <- dplyr::tbl(conn, "tbl_Sites") %>%
      dplyr::select(Site_ID, Unit_Code, Site_Name)

    #Locations (e.g. Sampling Frame)
    #Short
    tbl_Locations_short <- dplyr::tbl(conn, "tbl_Locations") %>%
      dplyr::select(Location_ID, Site_ID, Community = Plant_Community, Sampling_Frame)
    #Extra
    tbl_Locations_extra <- dplyr::tbl(conn, "tbl_Locations") %>%
      dplyr::select(Location_ID, Site_ID, Community = Plant_Community, Sampling_Frame, Zone, Management_Unit)

    # Transects
    tbl_Transects_short <- dplyr::tbl(conn, "tbl_Transects") %>%
      dplyr::select(Transect_ID, Location_ID, Transect_Number, Transect_Type)
    tbl_Transects_extra <- dplyr::tbl(conn, "tbl_Transects") %>%
      dplyr::select(Transect_ID, Location_ID, Transect_Type, Transect_Number, Azimuth_Transect = Azimuth, Lat = Latitude, Lat_Dir = Latitude_Dir, Long = Longitude, Long_Dir = Longitude_Dir, GCS, Transect_Notes)

    # Events (e.g. The date the plot was sampled, QA/QC records)
    # Short
    Events <- dplyr::tbl(conn, "tbl_Events") %>%
      dplyr::select(Event_ID, Transect_ID, Start_Date) %>%
      dplyr::left_join(tbl_Transects_short, by = "Transect_ID") %>%
      dplyr::left_join(tbl_Locations_short, by = "Location_ID") %>%
      dplyr::left_join(tbl_Sites_short, by = "Site_ID") %>%
      dplyr::select(Start_Date, Unit_Code, Community, Sampling_Frame, Transect_Type, Transect_Number, Event_ID)
    # Extra
    Events_extra <-  dplyr::tbl(conn, "tbl_Events") %>%
      dplyr::left_join(tbl_Transects_extra, by = "Transect_ID") %>%
      dplyr::left_join(tbl_Locations_extra, by = "Location_ID") %>%
      #Move long text columns to end because of SQL driver error:
      dplyr::relocate(Event_Notes, .after = last_col()) %>%
      dplyr::relocate(Transect_Notes, .after = last_col()) %>%
      dplyr::left_join(tbl_Sites_extra, by = "Site_ID") %>%
      #Move long text columns to end because of SQL driver error:
      dplyr::relocate(Event_Notes, .after = last_col()) %>%
      dplyr::relocate(Transect_Notes, .after = last_col()) %>%
      dplyr::select(Event_ID, Transect_ID, Start_Date, Unit_Code, Sampling_Frame, Zone, Management_Unit,
                    Transect_Number, Site_Name, Transect_Type, Transect_Number, Azimuth_Transect, Lat, Long,
                    GCS, Lat_Dir, Long_Dir, Entered_Date, Updated_Date, Verified, Verified_By, Verified_Date,
                    Certified, Certified_By, Certified_Date, Transect_Notes, Event_Notes)
    # Events_extra_QAQC
    Events_extra_QAQC_new <- Events_extra %>%
      dplyr::select(Start_Date, Unit_Code, Sampling_Frame, Transect_Type, Transect_Number,
                    Entered_Date, Updated_Date, Verified, Verified_By, Verified_Date,
                    Certified, Certified_By, Certified_Date, Transect_Notes, Event_Notes) %>%
      dplyr::collect()

    # Events_extra_xy
    Events_extra_xy_new <- Events_extra %>%
      dplyr::select(Start_Date, Unit_Code, Sampling_Frame, Transect_Number, Azimuth_Transect, Lat, Long, GCS, Lat_Dir, Long_Dir) %>%
      dplyr::collect()

    # Events_extra_other
    Events_extra_other_new <- Events_extra %>%
      dplyr::select(Start_Date, Unit_Code, Sampling_Frame, Zone, Management_Unit,
                    Transect_Number, Site_Name) %>%
      dplyr::collect()

    # Species w/nativity

    # Nativity - Short
    xref_Park_Species_Nativity_short <- dplyr::tbl(conn, "xref_Park_Species_Nativity") %>%
      dplyr::select(Species_ID, Park, Nativity = Nativeness)

    # Nativity - Extra
    xref_Park_Species_Nativity_extra <- dplyr::tbl(conn, "xref_Park_Species_Nativity") %>%
      dplyr::select(Species_ID, Park, Life_form, Nativity = Nativeness, Park_common_name,
                    Distribution, Conservation_status)

    # Species
    Species <- dplyr::tbl(conn, "tlu_Species") %>%
      dplyr::select(Species_ID, Scientific_name, Code, Life_form) %>%
      dplyr::right_join(xref_Park_Species_Nativity_short, by = "Species_ID")

    # Species_extra
    Species_extra_new <- dplyr::tbl(conn, "tlu_Species") %>%
      dplyr::select(Species_ID, Scientific_name, Code, Taxonomic_Order, Taxonomic_Family, Genus, Species,
                    Subdivision, Authority, Synonym, Authority_Source, Citation,
                    Common_name, Life_cycle, Complete, Update_date, Update_by,
                    Update_comments) %>%
      dplyr::right_join(xref_Park_Species_Nativity_extra, by = "Species_ID") %>%
      dplyr::collect() %>%
      dplyr::rename(Scientific_Name = Scientific_name, Life_Form = Life_form,
                    Park_Common_Name = Park_common_name, Conservation_Status = Conservation_status,
                    Common_Name = Common_name, Life_Cycle = Life_cycle, Update_Date = Update_date,
                    Update_By = Update_by, Update_Comments = Update_comments)

    # EIPS data
    xref_Cover_Class_Species <- dplyr::tbl(conn, "xref_Cover_Class_Species") %>%
      dplyr::select(Segment_ID, Event_ID, Species_ID, Cover_class, Dead)
    tbl_Segments <- dplyr::tbl(conn, "tbl_Segments") %>%
      dplyr::select(Segment_ID, Event_ID, No_Data, Segment_Notes)

    EIPS_data_new <- Events %>%
      dplyr::right_join(tbl_Segments, by = "Event_ID") %>%
      dplyr::left_join(xref_Cover_Class_Species, by = c("Segment_ID", "Event_ID")) %>%
      dplyr::left_join(Species, by = "Species_ID") %>%
      dplyr::select(Unit_Code, Community, Sampling_Frame, Start_Date, Transect_Number, Transect_Type, Species_ID, Cover_class, Dead, Code, Scientific_name, Life_form, Nativity) %>%
      dplyr::collect() %>%
      dplyr::rename(Cover_Class = Cover_class, Scientific_Name = Scientific_name, Life_Form = Life_form)

    Events_extra_QAQC_EIPS <- unique(rbind(Events_extra_QAQC_EIPS, Events_extra_QAQC_new))
    Events_extra_xy_EIPS <- unique(rbind(Events_extra_xy_EIPS, Events_extra_xy_new))
    Events_extra_other_EIPS <- unique(rbind(Events_extra_other_EIPS, Events_extra_other_new))
    Species_extra_EIPS <- unique(rbind(Species_extra_EIPS, Species_extra_new))
    EIPS_data <- unique(rbind(EIPS_data, EIPS_data_new))

    DBI::dbDisconnect(conn)
  } # End for loop

  data <- list(Events_extra_QAQC_EIPS = Events_extra_QAQC_EIPS,
               Events_extra_xy_EIPS = Events_extra_xy_EIPS,
               Events_extra_other_EIPS = Events_extra_other_EIPS,
               Species_extra_EIPS = Species_extra_EIPS,
               EIPS_data = EIPS_data)

  data <- lapply(data, function(df) {
    dplyr::mutate_if(df, is.character, iconv, "CP1252", "UTF-8")  # Convert to UTF-8 encoding
  })

  return(data)
}

#' Get column specifications
#'
#' @return A list of column specifications for each table of data.
#'
GetColSpec <- function() {
  time_format <- "%Y-%m-%dT%H:%M:%SZ"
  col.spec <- list(
    Events_extra_QAQC = readr::cols(Start_Date = readr::col_datetime(time_format),
                                    Plot_Number = readr::col_integer(),
                                    Entered_date = readr::col_datetime(time_format),
                                    Updated_date = readr::col_datetime(time_format),
                                    Verified = readr::col_logical(),
                                    Verified_date = readr::col_datetime(time_format),
                                    Certified = readr::col_logical(),
                                    Certified_date = readr::col_datetime(time_format),
                                    Completion_time = readr::col_double(),
                                    .default = readr::col_character()),
    Events_extra_xy = readr::cols(Start_Date = readr::col_datetime(time_format),
                                  Plot_Number = readr::col_integer(),
                                  Azimuth_Plot = readr::col_integer(),
                                  Start_Lat = readr::col_double(),
                                  Start_Long = readr::col_double(),
                                  Center_Lat = readr::col_double(),
                                  Center_Long = readr::col_double(),
                                  End_Lat = readr::col_double(),
                                  End_Long = readr::col_double(),
                                  .default = readr::col_character()),
    Events_extra_other = readr::cols(Start_Date = readr::col_datetime(time_format),
                                     Plot_Number = readr::col_integer(),
                                     Max_veg_ht = readr::col_double(),
                                     Images = readr::col_logical(),
                                     .default = readr::col_character()),
    Species_extra = readr::cols(Complete = readr::col_logical(),
                                Update_date = readr::col_datetime(time_format),
                                .default = readr::col_character()),
    LgTrees = readr::cols(Start_Date = readr::col_datetime(time_format),
                          Plot_Number = readr::col_integer(),
                          QA_Plot = readr::col_logical(),
                          Height = readr::col_double(),
                          Height_Dead = readr::col_double(),
                          Boles = readr::col_integer(),
                          DBH = readr::col_double(),
                          DBH_Other = readr::col_double(),
                          Fruit_Flower = readr::col_logical(),
                          Caudex_Length = readr::col_double(),
                          Shrublike_Growth = readr::col_logical(),
                          Resprouts = readr::col_logical(),
                          DBH_Bole = readr::col_double(),
                          .default = readr::col_character()),
    Canopy = readr::cols(Start_Date = readr::col_datetime(time_format),
                         Plot_Number = readr::col_integer(),
                         QA_Plot = readr::col_logical(),
                         Top = readr::col_integer(),
                         Base = readr::col_integer(),
                         Base_ht = readr::col_double(),
                         Distance = readr::col_double(),
                         Height = readr::col_double(),
                         DBH = readr::col_double(),
                         .default = readr::col_character()),
    Presence = readr::cols(Start_Date = readr::col_datetime(time_format),
                           Plot_Number = readr::col_integer(),
                           QA_Plot = readr::col_logical(),
                           Fruit_Flower = readr::col_logical(),
                           Dead = readr::col_logical(),
                           Outside_Plot = readr::col_logical(),
                           cf = readr::col_logical(),
                           .default = readr::col_character()),
    SmWoody = readr::cols(Start_Date = readr::col_datetime(time_format),
                          Plot_Number = readr::col_integer(),
                          QA_Plot = readr::col_logical(),
                          Count = readr::col_integer(),
                          .default = readr::col_character()),
    Understory = readr::cols(Start_Date = readr::col_datetime(time_format),
                             Plot_Number = readr::col_integer(),
                             QA_Plot = readr::col_logical(),
                             Point = readr::col_integer(),
                             Dead = readr::col_logical(),
                             .default = readr::col_character()),
    Debris = readr::cols(Start_Date = readr::col_datetime(time_format),
                         Plot_Number = readr::col_integer(),
                         QA_Plot = readr::col_logical(),
                         Diameter = readr::col_double(),
                         .default = readr::col_character()),
    Events_extra_QAQC_EIPS = readr::cols(Start_Date = readr::col_datetime(time_format),
                                         Entered_Date = readr::col_datetime(time_format),
                                         Updated_Date = readr::col_datetime(time_format),
                                         Verified = readr::col_logical(),
                                         Verified_Date = readr::col_datetime(time_format),
                                         Certified = readr::col_logical(),
                                         Certified_Date = readr::col_datetime(time_format),
                                         .default = readr::col_character()),
    Events_extra_xy_EIPS = readr::cols(Start_Date = readr::col_datetime(time_format),
                                       Azimuth_Transect = readr::col_integer(),
                                       Lat = readr::col_double(),
                                       Long = readr::col_double(),
                                       .default = readr::col_character()),
    Events_extra_other_EIPS = readr::cols(Start_Date = readr::col_datetime(time_format),
                                          .default = readr::col_character()),
    Species_extra_EIPS = readr::cols(Complete = readr::col_logical(),
                                     Update_Date = readr::col_datetime(),
                                     .default = readr::col_character()),
    EIPS_data = readr::cols(Start_Date = readr::col_datetime(time_format),
                            Dead = readr::col_logical(),
                            .default = readr::col_character())
  )

  return(col.spec)
}

#' Read data from a folder of csv files
#'
#' @param data_path A path to a folder containing the data in csv format
#'
#' @return A list of tibbles
#'
ReadCSV <- function(data_path) {
  data_path <- normalizePath(data_path)
  col.spec <- GetColSpec()
  data <- lapply(names(col.spec), function(data_name){
    file_path <- file.path(data_path, paste0(data_name, ".csv"))
    df <- readr::read_csv(file = file_path, col_types = col.spec[[data_name]])
    return(df)
  })

  names(data) <- names(col.spec)
  return(data)
}

FilterPACNVeg <- function(data_name, park, sample_frame, community, plot, plot_type, is_qa_plot, quad, transect, species_code, sci_name, nativity, live_dead, certified, verified) {
  data <- get_data(data_name)


  return(data)
}

#' Save PACN vegetation data as a set of .csv files
#'
#' @param dest.folder The folder in which to save the .csv files.
#' @param create.folders Should \code{dest.folder} be created automatically if it doesn't exist? Defaults to \code{FALSE}.
#' @param overwrite Should existing data be automatically overwritten? Defaults to \code{FALSE}.
#'
#' @return None.
#' @export
#'
#' @examples
#' \dontrun{
#' LoadPACNVeg("pacnveg", "path/to/access.mdb")
#' WritePACNVeg("folder/for/csv/data", create.folders = TRUE)
#' }
WritePACNVeg <- function(dest.folder, create.folders = FALSE, overwrite = FALSE, park, sample_frame, community, certified, verified, is_qa_plot) {
  data <- FilterPACNVeg()
  dest.folder <- normalizePath(dest.folder, mustWork = FALSE)
  col.spec <- GetColSpec()
  file.paths <- file.path(dest.folder, paste0(names(col.spec), ".csv"))

  if (!dir.exists(dest.folder) & create.folders) {
    dir.create(dest.folder)
  } else {
    stop("Destination folder does not exist. To create it automatically, set create.folders to TRUE.")
  }

  if (!overwrite & any(file.exists(file.paths))) {
    stop("Saving data in the folder provided would overwrite existing data. To automatically overwrite existing data, set overwrite to TRUE.")
  }

  invisible(
    lapply(names(col.spec), function(data_name) {
      full_path <- file.path(dest.folder, paste0(data_name, ".csv"))
      message(paste("Writing", full_path))
      readr::write_csv(data[[data_name]], full_path, na = "", append = FALSE, col_names = TRUE)
    })
  )
  message("Done writing to CSV")

}

#' Test for dataframe equivalence
#'
#' @param result Actual data frame
#' @param expected Expected data frame
#' @param ignore_col_order Ignore order of columns in dataframe? Defaults to FALSE.
#' @param ignore_row_order Ignore order of rows in dataframe? Defaults to TRUE.
#'
#' @return If test passes, nothing. If it fails, description of failure.
#'
#' @export
#'
expect_dataframe_equal <- function(result, expected, ignore_col_order = FALSE, ignore_row_order = TRUE) {
  # Check for same columns
  cols_match <- ifelse(ignore_col_order,
                       all(names(result) %in% names(expected)) & all(names(expected) %in% names(result)),
                       names(result) == names(expected))

  # Rearrange columns to match if ignoring column order
  if (cols_match & ignore_col_order) {
    result <- dplyr::select(result, names(expected))
  }
  # Rearrange row order to match if ignoring row order
  if (ignore_row_order) {
    result <- dplyr::arrange_at(result, names(result))
    expected <- dplyr::arrange_at(expected, names(expected))
  }
  # Compare dataframes
  test_result <- all.equal(result, expected, check.attributes = FALSE, use.names = TRUE, check.names = TRUE)

  return(testthat::expect_true(test_result, label = test_result))
}
