#' @importFrom magrittr %>% %<>%
#' @importFrom data.table ":="
#'
#' @export
magrittr::`%>%`
magrittr::`%<>%`

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

pkg_globals <- new.env(parent = emptyenv())

# Load data from global package environment
get_data <- function(data_name) {
  if (!missing(data_name)) {
    if (!(data_name %in% names(GetColSpec()))) {
      stop("Invalid data table name. Use names(pacnvegetation:::GetColSpec()) to see valid options for data_name.")
    }
    tryCatch({data <- get(data_name, pkg_globals)},
             error = function(e) {
               if (grepl(".*object.* not found.*", e$message, ignore.case = TRUE)) {
                 stop(paste0("Could not find data. Did you remember to call LoadPACNVeg?\n\tOriginal error: ", e$message))
               }
               else {e}
             })
  } else {
    tryCatch({
      data <- lapply(names(GetColSpec()), get, pkg_globals)
      names(data) <- names(GetColSpec())
    },
    error = function(e) {
      if (grepl(".*object.* not found.*", e$message, ignore.case = TRUE)) {
        stop(paste0("Could not find data. Did you remember to call LoadPACNVeg?\n\tOriginal error: ", e$message))
      }
      else {e}
    }
    )

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
  tbl_Locations <- dplyr::tbl(conn, "tbl_Locations") %>%
    dplyr::select(Location_ID, Site_ID, Community, Sampling_Frame)


  # . . 3. tbl_Plot----

  #Plots (e.g. Plot numbers, Plot type (Fixed vs. Rotational, Plot coordinates))

  #Short
  tbl_Plot_short <- dplyr::tbl(conn, "tbl_Plot") %>%
    dplyr::select(Plot_ID, Location_ID, Plot_Number, Plot_Type)

  #Extra
  tbl_Plot_extra <- dplyr::tbl(conn, "tbl_Plot") %>%
    dplyr::select(Plot_ID, Location_ID, Plot_Type, Plot_Number, Azimuth_Plot,
           Start_Lat, Start_Long, Center_Lat, Center_Long, End_Lat, End_Long,
           GCS, Lat_Dir, Long_Dir, Plot_Notes)

  # B. Temporal ----------------------------------------------------------------
  # . . 1. tbl_Events----

  #Events (e.g. The date the plot was sampled, QA/QC records)

  tbl_Events <- dplyr::tbl(conn, "tbl_Events") %>%
    dplyr::select(Event_ID, Plot_ID, Start_Date, QA_Plot, Images, Max_Veg_Ht = Max_veg_ht,
                  Entered_Date = Entered_date, Updated_Date = Updated_date, Verified, Verified_By = Verified_by, Verified_Date = Verified_date,
                  Certified, Certified_By = Certified_by, Certified_Date = Certified_date, Completion_Time = Completion_time,
                  Event_Notes, QA_notes) %>%
    # add "Year" (year sampled) and "Cycle" (sample cycle)
    dplyr::mutate(Year = YEAR(Start_Date)) %>%
    #supposedly SQL does not have translation for case_when so must use "ifelse" ??
    dplyr::mutate(Cycle = ifelse(Year <= 2014, 1, NA)) %>%
    dplyr::mutate(Cycle = ifelse(is.na(Cycle) & Year >= 2015 & Year <= 2020, 2, Cycle)) %>%
    dplyr::mutate(Cycle = ifelse(is.na(Cycle) & Year >= 2021 & Year <= 2027, 3, Cycle))
    #dplyr::mutate(Cycle = dplyr::case_when(Year <= 2014 ~ "1",
    #                         Year >= 2015 & Year <= 2020 ~ "2",
    #                         Year >= 2021 ~ "3"))

  #Short
  tbl_Events_short <- tbl_Events %>%
    dplyr::select(Event_ID, Plot_ID, Year, Cycle, QA_Plot, Certified, Verified)


  # . . . . **join** Spatial & Temp ---------------------------------------------------

  # . . . . Events ----
  Events <- tbl_Events_short %>%
    dplyr::left_join(tbl_Plot_short, by = "Plot_ID") %>%
    dplyr::left_join(tbl_Locations, by = "Location_ID") %>%
    dplyr::left_join(tbl_Sites_short, by = "Site_ID") %>%
    dplyr::select(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number,
           QA_Plot, Certified, Verified, Event_ID)


  # . . . . Events_extra ----
  Events_extra <- tbl_Events %>%
    dplyr::left_join(tbl_Plot_extra, by = "Plot_ID") %>%
    dplyr::left_join(tbl_Locations, by = "Location_ID") %>%
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
    dplyr::select(Unit_Code, Sampling_Frame, Start_Date, Year, Cycle, Plot_Type, Plot_Number,
           Entered_Date, Updated_Date, Verified, Verified_By, Verified_Date,
           Certified, Certified_By, Certified_Date, Completion_Time,
           Event_Notes, Plot_Notes, QA_notes) %>%
    dplyr::collect()

  # . . Events_extra_xy
  Events_extra_xy <- Events_extra %>%
    dplyr::select(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number,
           Azimuth_Plot, Start_Lat, Start_Long, Center_Lat, Center_Long,
           End_Lat, End_Long, GCS, Lat_Dir, Long_Dir, Certified, Verified) %>%
    dplyr::collect()

  # . . Events_extra_other
  Events_extra_other <- Events_extra %>%
    dplyr::select(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type,
                  Plot_Number, Max_Veg_Ht, Site_Name, Images, Certified, Verified) %>%
    dplyr::collect()


  # C. Species ----------------------------------------------------------------

  # . . 1. tlu_Species----

  #Short
  tlu_Species_short <- dplyr::tbl(conn, "tlu_Species") %>%
    dplyr::select(Species_ID, Scientific_Name = Scientific_name, Code, Life_Form = Life_form)

  #Extra
  tlu_Species_extra <- dplyr::tbl(conn, "tlu_Species") %>%
    dplyr::select(Species_ID, Scientific_Name = Scientific_name, Code, Taxonomic_Order, Taxonomic_Family, Genus, Species,
           Subdivision,
           Authority,
           #Synonym,
           Authority_Source,
           Citation,
           #Common_Name = Common_name,
           Life_Cycle = Life_cycle,
           #Complete,
           Update_Date = Update_date,
           Update_By = Update_by,
           Update_Comments = Update_comments)

  # . . 2. xref_Park_Species_Nativity----

  #Short
  xref_Park_Species_Nativity_short <- dplyr::tbl(conn, "xref_Park_Species_Nativity") %>%
    dplyr::select(Species_ID, Park, Nativity)

  #Extra
  xref_Park_Species_Nativity_extra <- dplyr::tbl(conn, "xref_Park_Species_Nativity") %>%
    dplyr::select(Species_ID, Park, Life_Form = Life_form, Nativity, Park_Common_Name = Park_common_name,
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
                  Height_Dead, Boles, DBH, Measurement_Type, Measurement, Vigor,
                  Fruit_Flower, Rooting, Foliar, Caudex_Length,
                  Shrublike_Growth, Resprouts)

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
    dplyr::collect() %>%
    dplyr::relocate(Certified, Verified, .after = last_col()) %>%
    dplyr::mutate(Cycle = as.integer(Cycle))


  # . . 2. tbl_Tree_Canopy_Height----
  # Height of canopy, sub-canopy, and emergent trees

  tbl_Tree_Canopy_Height <- dplyr::tbl(conn, "tbl_Tree_Canopy_Height") %>%
    dplyr::select(Event_ID, Species_ID, Quad, Status, Top, Base, Base_Ht = Base_ht,
                  Distance, Height, Method, DBH, Comments)
  # . . . . Canopy ----
  Canopy <- Events %>%
    dplyr::right_join(tbl_Tree_Canopy_Height, by = "Event_ID") %>%
    dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
    dplyr::select(-Event_ID, -Species_ID) %>%
    #Move long text columns to end because of SQL driver error:
    dplyr::relocate(Comments, .after = last_col()) %>%
    dplyr::collect() %>%
    dplyr::relocate(Certified, Verified, .after = last_col())


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
    dplyr::collect() %>%
    dplyr::relocate(Certified, Verified, .after = last_col())



  # . . 4. tbl_Sm_Woody_Tally----
  # Vines, seedlings, shrubs, small trees, and small tree ferns.
  tbl_Sm_Woody_Tally <- dplyr::tbl(conn, "tbl_Sm_Woody_Tally") %>%
    dplyr::select(Event_ID, Species_ID, Sample_Area, LF_Sm_Woody = Life_Form, DBH, Status,
                  Length, Rooting, Count, Comments)
  # . . . . SmWoody ----
  SmWoody <- Events %>%
    dplyr::right_join(tbl_Sm_Woody_Tally, by = "Event_ID") %>%
    dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
    dplyr::select(-Event_ID, -Species_ID) %>%
    #Move long text columns to end because of SQL driver error:
    dplyr::relocate(Comments, .after = last_col()) %>%
    dplyr::collect() %>%
    dplyr::relocate(Certified, Verified, .after = last_col())



  # . . 5. tbl_Understory_Cover----
  # point-intercept cover of  species and substrate
  # two stratum: 1)low [0-1m] 2) high [1-2m]
  UnderstoryCover <- dplyr::tbl(conn, "tbl_Understory_Cover") %>%
    dplyr::select(Event_ID, Point_ID, Point, Substrate)

  # . . . . xref_Understory_Low----
  UnderstoryLow <- dplyr::tbl(conn, "xref_Understory_Low") %>%
    dplyr::select(Event_ID, Point_ID, Species_ID, Dead) %>%
    #dplyr::mutate(Species_ID = ifelse(Dead == TRUE, NA, Species_ID)) %>%
    #All understory is Live (first year methods also collected data data which is dropped here)
    dplyr::filter(Dead == "FALSE") %>%
    dplyr::select(-Dead) %>%
    dplyr::mutate(Stratum = "Low")

  # . . . . xref_Understory_High----
  UnderstoryHigh <- dplyr::tbl(conn, "xref_Understory_High") %>%
    dplyr::select(Event_ID, Point_ID, Species_ID, Dead) %>%
    dplyr::filter(Dead == "FALSE") %>%
    dplyr::select(-Dead) %>%
    dplyr::mutate(Stratum = "High")

  UnderstorySpecies <- dplyr::union_all(UnderstoryLow, UnderstoryHigh)

  Understory <- Events %>%
    dplyr::right_join(UnderstoryCover, by = "Event_ID") %>%
    dplyr::left_join(UnderstorySpecies, by = c("Event_ID", "Point_ID")) %>%
    dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
    dplyr::select(-Event_ID, -Species_ID, -Point_ID) %>%
    dplyr::relocate(Certified, Verified, .after = last_col()) %>%
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
    dplyr::select(Woody_Debris_ID, Debris_Type = Debris_type, Diameter, Decay_Class, Comments)

  # . . . . Debris ----
  Debris <- Events %>%
    dplyr::right_join(tbl_Woody_Debris, by = "Event_ID") %>%
    dplyr::left_join(tbl_Debris_Species, by = "Woody_Debris_ID") %>%
    dplyr::select(-Woody_Debris_ID, -Event_ID) %>%
    dplyr::collect() %>%
    dplyr::relocate(Certified, Verified, .after = last_col())



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
  EIPS_image_pts <- tibble::tibble()
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
    tbl_Locations <- dplyr::tbl(conn, "tbl_Locations") %>%
      dplyr::select(Location_ID, Site_ID, Community = Plant_Community, Sampling_Frame)

    # Transects
    tbl_Transects_short <- dplyr::tbl(conn, "tbl_Transects") %>%
      dplyr::select(Transect_ID, Location_ID, Transect_Number, Transect_Type)
    tbl_Transects_extra <- dplyr::tbl(conn, "tbl_Transects") %>%
      dplyr::select(Transect_ID, Location_ID, Transect_Type, Transect_Number, Azimuth_Transect = Azimuth, Lat = Latitude, Lat_Dir = Latitude_Dir, Long = Longitude, Long_Dir = Longitude_Dir, GCS, Transect_Notes)

    # Events (e.g. The date the plot was sampled, QA/QC records)
    # Extra
    Events_extra <-  dplyr::tbl(conn, "tbl_Events") %>%
      # add "Year" (year sampled) and "Cycle" (sample cycle)
      dplyr::mutate(Year = YEAR(Start_Date)) %>%
      dplyr::mutate(Cycle = ifelse(Year <= 2014, 1,
                                     ifelse(Year >= 2015 & Year <= 2020, 2,
                                            ifelse(Year >= 2021, 3, NA)))) %>%
      dplyr::left_join(tbl_Transects_extra, by = "Transect_ID") %>%
      dplyr::left_join(tbl_Locations, by = "Location_ID") %>%
      #Move long text columns to end because of SQL driver error:
      dplyr::relocate(Event_Notes, .after = last_col()) %>%
      dplyr::relocate(Transect_Notes, .after = last_col()) %>%
      dplyr::left_join(tbl_Sites_extra, by = "Site_ID") %>%
      #Move long text columns to end because of SQL driver error:
      dplyr::relocate(Event_Notes, .after = last_col()) %>%
      dplyr::relocate(Transect_Notes, .after = last_col()) %>%
      dplyr::select(Event_ID, Transect_ID, Unit_Code, Community, Sampling_Frame, Start_Date, Year, Cycle,
                    Transect_Number, Site_Name, Transect_Type, Transect_Number, Azimuth_Transect, Lat, Long,
                    GCS, Lat_Dir, Long_Dir, Entered_Date, Updated_Date, Verified, Verified_By, Verified_Date,
                    Certified, Certified_By, Certified_Date, Transect_Notes, Event_Notes) #-Start_Date
    # Short
    Events <- Events_extra %>%
      dplyr::select(Event_ID, Transect_ID, Unit_Code, Community, Sampling_Frame, Year, Cycle, Transect_Type, Transect_Number, Certified, Verified)

    # Events_extra_QAQC
    Events_extra_QAQC_new <- Events_extra %>%
      dplyr::select(Unit_Code, Sampling_Frame, Start_Date, Year, Cycle, Transect_Type, Transect_Number,
                    Entered_Date, Updated_Date, Verified, Verified_By, Verified_Date,
                    Certified, Certified_By, Certified_Date, Transect_Notes, Event_Notes) %>% #-Start_date
      dplyr::collect()

    # Events_extra_xy
    Events_extra_xy_new <- Events_extra %>%
      dplyr::select(Unit_Code, Sampling_Frame, Year, Cycle, Transect_Type, Transect_Number, Azimuth_Transect, Lat, Long, GCS, Lat_Dir, Long_Dir, Certified, Verified) %>%
      dplyr::collect()

    # Events_extra_other
    Events_extra_other_new <- Events_extra %>%
      dplyr::select(Unit_Code, Sampling_Frame, Year, Cycle, Transect_Type,
                    Transect_Number, Site_Name, Certified, Verified) %>%
      dplyr::collect()

    # Image Points
    tbl_Image_Points <- dplyr::tbl(conn, "tbl_Image_Points")
    EIPS_image_pts_new <- Events %>%
      dplyr::right_join(tbl_Image_Points, by = "Event_ID") %>%
      dplyr::select(Unit_Code, Community, Sampling_Frame, Year, Cycle,
                    Transect_Type, Transect_Number, Image_Point,
                    Latitude, Latitude_Dir, Longitude, Longitude_Dir, GCS, GPS_Error) %>%
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
    tlu_Segment_Points <- dplyr::tbl(conn, "tlu_Segment_Points")

    EIPS_data_new <- Events %>%
      dplyr::right_join(tbl_Segments, by = "Event_ID") %>%
      dplyr::left_join(tlu_Segment_Points, by = "Segment_ID") %>%
      dplyr::left_join(xref_Cover_Class_Species, by = c("Segment_ID", "Event_ID")) %>%
      dplyr::left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
      dplyr::select(Unit_Code, Community, Sampling_Frame, Year, Cycle, Transect_Type, Transect_Number, Segment = Sort_Order, Species_ID, Cover_class, Dead, Code, Scientific_name, Life_form, Nativity, Certified, Verified) %>%
      dplyr::collect() %>%
      dplyr::rename(Cover_Class = Cover_class, Scientific_Name = Scientific_name, Life_Form = Life_form) %>%
      dplyr::relocate(Certified, Verified, .after = last_col())

    Events_extra_QAQC_EIPS <- unique(rbind(Events_extra_QAQC_EIPS, Events_extra_QAQC_new))
    Events_extra_xy_EIPS <- unique(rbind(Events_extra_xy_EIPS, Events_extra_xy_new))
    Events_extra_other_EIPS <- unique(rbind(Events_extra_other_EIPS, Events_extra_other_new))
    EIPS_image_pts <- unique(rbind(EIPS_image_pts, EIPS_image_pts_new))
    Species_extra_EIPS <- unique(rbind(Species_extra_EIPS, Species_extra_new))
    EIPS_data <- unique(rbind(EIPS_data, EIPS_data_new))

    DBI::dbDisconnect(conn)
  } # End for loop

  data <- list(Events_extra_QAQC_EIPS = Events_extra_QAQC_EIPS,
               Events_extra_xy_EIPS = Events_extra_xy_EIPS,
               Events_extra_other_EIPS = Events_extra_other_EIPS,
               EIPS_image_pts = EIPS_image_pts,
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
                                    Year = readr::col_integer(),
                                    Cycle = readr::col_integer(),
                                    Plot_Number = readr::col_integer(),
                                    Entered_Date = readr::col_datetime(time_format),
                                    Updated_Date = readr::col_datetime(time_format),
                                    Verified = readr::col_logical(),
                                    Verified_Date = readr::col_datetime(time_format),
                                    Certified = readr::col_logical(),
                                    Certified_Date = readr::col_datetime(time_format),
                                    Completion_Time = readr::col_double(),
                                    .default = readr::col_character()),
    Events_extra_xy = readr::cols(Year = readr::col_integer(),
                                  Cycle = readr::col_integer(),
                                  Plot_Number = readr::col_integer(),
                                  Azimuth_Plot = readr::col_integer(),
                                  Start_Lat = readr::col_double(),
                                  Start_Long = readr::col_double(),
                                  Center_Lat = readr::col_double(),
                                  Center_Long = readr::col_double(),
                                  End_Lat = readr::col_double(),
                                  End_Long = readr::col_double(),
                                  Certified = readr::col_logical(),
                                  Verified = readr::col_logical(),
                                  .default = readr::col_character()),
    Events_extra_other = readr::cols(Year = readr::col_integer(),
                                     Cycle = readr::col_integer(),
                                     Plot_Number = readr::col_integer(),
                                     Max_Veg_Ht = readr::col_double(),
                                     Images = readr::col_logical(),
                                     Certified = readr::col_logical(),
                                     Verified = readr::col_logical(),
                                     .default = readr::col_character()),
    Species_extra = readr::cols(Update_Date = readr::col_datetime(time_format),
                                .default = readr::col_character()),
    LgTrees = readr::cols(Year = readr::col_integer(),
                          Cycle = readr::col_integer(),
                          Plot_Number = readr::col_integer(),
                          QA_Plot = readr::col_logical(),
                          Height = readr::col_double(),
                          Height_Dead = readr::col_double(),
                          Boles = readr::col_integer(),
                          DBH = readr::col_double(),
                          Fruit_Flower = readr::col_logical(),
                          Caudex_Length = readr::col_double(),
                          Shrublike_Growth = readr::col_logical(),
                          Resprouts = readr::col_logical(),
                          DBH_Bole = readr::col_double(),
                          Measurement = readr::col_double(),
                          Certified = readr::col_logical(),
                          Verified = readr::col_logical(),
                          .default = readr::col_character()),
    Canopy = readr::cols(Year = readr::col_integer(),
                         Cycle = readr::col_integer(),
                         Plot_Number = readr::col_integer(),
                         QA_Plot = readr::col_logical(),
                         Top = readr::col_integer(),
                         Base = readr::col_integer(),
                         Base_Ht = readr::col_double(),
                         Distance = readr::col_double(),
                         Height = readr::col_double(),
                         DBH = readr::col_double(),
                         Certified = readr::col_logical(),
                         Verified = readr::col_logical(),
                         .default = readr::col_character()),
    Presence = readr::cols(Year = readr::col_integer(),
                           Cycle = readr::col_integer(),
                           Plot_Number = readr::col_integer(),
                           QA_Plot = readr::col_logical(),
                           Fruit_Flower = readr::col_logical(),
                           Dead = readr::col_logical(),
                           Outside_Plot = readr::col_logical(),
                           cf = readr::col_logical(),
                           Certified = readr::col_logical(),
                           Verified = readr::col_logical(),
                           .default = readr::col_character()),
    SmWoody = readr::cols(Year = readr::col_integer(),
                          Cycle = readr::col_integer(),
                          Plot_Number = readr::col_integer(),
                          QA_Plot = readr::col_logical(),
                          Count = readr::col_integer(),
                          Certified = readr::col_logical(),
                          Verified = readr::col_logical(),
                          .default = readr::col_character()),
    Understory = readr::cols(Year = readr::col_integer(),
                             Cycle = readr::col_integer(),
                             Plot_Number = readr::col_integer(),
                             QA_Plot = readr::col_logical(),
                             Point = readr::col_integer(),
                             Certified = readr::col_logical(),
                             Verified = readr::col_logical(),
                             .default = readr::col_character()),
    Debris = readr::cols(Year = readr::col_integer(),
                         Cycle = readr::col_integer(),
                         Plot_Number = readr::col_integer(),
                         QA_Plot = readr::col_logical(),
                         Diameter = readr::col_double(),
                         Certified = readr::col_logical(),
                         Verified = readr::col_logical(),
                         .default = readr::col_character()),
    Events_extra_QAQC_EIPS = readr::cols(Start_Date = readr::col_datetime(time_format),
                                         Year = readr::col_integer(),
                                         Cycle = readr::col_integer(),
                                         Entered_Date = readr::col_datetime(time_format),
                                         Updated_Date = readr::col_datetime(time_format),
                                         Verified = readr::col_logical(),
                                         Verified_Date = readr::col_datetime(time_format),
                                         Certified = readr::col_logical(),
                                         Certified_Date = readr::col_datetime(time_format),
                                         .default = readr::col_character()),
    Events_extra_xy_EIPS = readr::cols(Year = readr::col_integer(),
                                       Cycle = readr::col_integer(),
                                       Azimuth_Transect = readr::col_integer(),
                                       Lat = readr::col_double(),
                                       Long = readr::col_double(),
                                       Certified = readr::col_logical(),
                                       Verified = readr::col_logical(),
                                       .default = readr::col_character()),
    Events_extra_other_EIPS = readr::cols(Year = readr::col_integer(),
                                          Cycle = readr::col_integer(),
                                          Certified = readr::col_logical(),
                                          Verified = readr::col_logical(),
                                          .default = readr::col_character()),
    EIPS_image_pts = readr::cols(Year = readr::col_integer(),
                                 Cycle = readr::col_integer(),
                                 Latitude = readr::col_double(),
                                 Longitude = readr::col_double(),
                                 .default = readr::col_character()),
    Species_extra_EIPS = readr::cols(Complete = readr::col_logical(),
                                     Update_Date = readr::col_datetime(),
                                     .default = readr::col_character()),
    EIPS_data = readr::cols(Year = readr::col_integer(),
                            Cycle = readr::col_integer(),
                            Segment = readr::col_integer(),
                            Dead = readr::col_logical(),
                            Certified = readr::col_logical(),
                            Verified = readr::col_logical(),
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

#' Filter PACN data
#'
#' @details All filters are optional. To ignore a filter, omit it or set it to NA.
#'
#' @inheritParams FilterOne
#' @param park Four letter unit code of park(s)
#' @param sample_frame Name of sample frame
#' @param community Name of plant community
#' @param year Monitoring year
#' @param cycle Monitoring cycle
#' @param plot_type Type of plot (fixed vs. rotational)
#' @param is_qa_plot Whether the plots are QA plots or not (TRUE/FALSE)
#' @param transect_type Type of transect (fixed vs. rotational)
#' @param species_code 6 letter species code
#' @param sci_name Scientific name
#' @param nativity Whether species are native (TRUE/FALSE)
#' @param certified Whether data are certified (TRUE/FALSE)
#' @param verified Whether data are verified (TRUE/FALSE)
#'
#' @return A tibble (if data_name is provided) or a list of tibbles (if data_name is omitted)
#' @export
#'
#' @examples
#' \dontrun{
#' all_hawaii_data <- FilterPACNVeg(park = c("KALA", "KAHO", "HAVO", "HALE"))
#' all_certified_data <- FilterPACNVeg(certified = TRUE)
#' native_kaho_understory <- FilterPACNVeg("Understory", park = "KAHO", nativity = "Native")
#' }
FilterPACNVeg <- function(data_name, park, sample_frame, community, year, cycle, plot_type, plot_number, is_qa_plot = FALSE, transect_type, species_code, sci_name, nativity, certified, verified, case_sensitive = FALSE, silent = FALSE) {
  data <- get_data(data_name)

  all_filter_cols <- list(Unit_Code = ifelse(missing(park), NA, park),
                   Park = if (missing(park)) {NA} else {park},
                   Sampling_Frame = if (missing(sample_frame)) {NA} else {sample_frame},
                   Community = if(missing(community)) {NA} else {community},
                   Year = if(missing(year)) {NA} else {year},
                   Cycle = if (missing(cycle)) {NA} else {cycle},
                   Plot_Type = if (missing(plot_type)) {NA} else {plot_type},
                   Plot_Number = if (missing(plot_number)) {NA} else {plot_number},
                   QA_Plot = if (missing(is_qa_plot)) {NA} else {is_qa_plot},
                   Transect_Type = if (missing(transect_type)) {NA} else {transect_type},
                   Code = if (missing(species_code)) {NA} else {species_code},
                   Scientific_Name = if (missing(sci_name)) {NA} else {sci_name},
                   Nativity = if (missing(nativity)) {NA} else {nativity},
                   Certified = if (missing(certified)) {NA} else {certified},
                   Verified = if (missing(verified)) {NA} else {verified}
                   )

  filter_cols <- all_filter_cols[!is.na(all_filter_cols)]

  if (!is.data.frame(data)) {  # Note: a data frame/tibble is a list, but a list is not a data frame/tibble!
    data_names <- names(data)
    data <- lapply(names(data), function(data_name){
      df <- FilterOne(data[[data_name]], data_name, filter_cols = filter_cols, case_sensitive = case_sensitive, silent = silent)
      return(df)
    })
    names(data) <- data_names
  } else {
    data <- FilterOne(data, data_name, filter_cols, case_sensitive = case_sensitive, silent)
  }

  return(data)
}

#' Filter one dataframe
#' @description Helper function for FilterPACNVeg, not to be used outside that function
#'
#' @param data A tibble of PACN data
#' @param data_name The name of the data table (see `names(GetColSpec())` for valid options)
#' @param filter_cols Named vector where names are column names and values are values to filter on. This is created in the FilterPACNVeg function.
#' @param case_sensitive Should non-numeric filters be treated as case-sensitive?
#' @param silent Suppress informational messages?
#'
#' @return A tibble of filtered data
#'
FilterOne <- function(data, data_name, filter_cols, case_sensitive, silent) {
  # Iterate through each column to be filtered and filter the dataset on the value provided
  # Note: the !!as.symbol(col) syntax takes a character string (the column name) and causes it to be evaluated as a data variable that references a dataframe column
  cols_filtered <- c()
  for (col in names(filter_cols)) {
    if (col %in% names(data)) {  # Only filter if the column is present in the dataframe
      cols_filtered <- c(cols_filtered, col)  # Use this to keep track of which columns were actually filtered
      filter_value <- filter_cols[[col]]  # Value(s) to filter on
      if (is.character(data[[col]]) & !case_sensitive) {
        data <- dplyr::filter(data, tolower(!!as.symbol(col)) %in% tolower(filter_value))  # Case-insensitive filtering
      } else {
        data <- dplyr::filter(data, !!as.symbol(col) %in% filter_value)  # Case-sensitive and non-character filtering
      }
      if (nrow(data) == 0) {  # Stop filtering if we end up with an empty dataframe
        warning(paste("There are no data in", data_name, "that match all of the filters provided."))
        break
      }
    }
  }
  if (!silent & length(filter_cols) > 0) {
    col_list <- paste(cols_filtered, collapse = ", ")
    message(paste("Filtered", data_name, "on columns:", ifelse(col_list != "", col_list, "[none]")))
  }

  return(data)
}

#' Save PACN vegetation data as a set of .csv files
#'
#' @inheritParams FilterPACNVeg
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
WritePACNVeg <- function(dest.folder, create.folders = FALSE, overwrite = FALSE, park, sample_frame, community, certified, verified, is_qa_plot = FALSE) {
  data <- FilterPACNVeg(park = park, sample_frame = sample_frame, community = community, certified = certified, verified = verified, is_qa_plot = is_qa_plot)
  dest.folder <- normalizePath(dest.folder, mustWork = FALSE)
  col.spec <- GetColSpec()
  file.paths <- file.path(dest.folder, paste0(names(col.spec), ".csv"))

  if (!dir.exists(dest.folder)) {
    if (create.folders) {
      dir.create(dest.folder)
    } else {
      stop("Destination folder does not exist. To create it automatically, set create.folders to TRUE.")
    }
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

#' Remove data from plots with no revisits
#'
#' @param data A tibble/dataframe with columns Unit_Code, Sampling_Frame, Plot_Number, and Cycle
#'
#' @return The input data with  single-visit data removed
#
RemoveSingleVisits <- function(data) {
  dup_visits <- data %>%
    dplyr::select(Unit_Code, Sampling_Frame, Plot_Number, Cycle) %>%
    unique() %>%
    dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Number) %>%
    dplyr::summarize(Plot_Count = dplyr::n(), .groups = "keep") %>%
    dplyr::arrange(Unit_Code, Sampling_Frame, Plot_Number) %>%
    dplyr::filter(Plot_Count > 1) %>%
    dplyr::select(-Plot_Count)

  data <- data %>%
    dplyr::inner_join(dup_visits, by = c("Unit_Code", "Sampling_Frame", "Plot_Number"))

  return(data)
}

#' Apply some standard formatting to a ggplot object.
#'
#' @param plot.title The title of the plot.
#' @param sub.title Optional custom plot subtitle.
#' @param x.lab X axis label.
#' @param y.lab Y axis label.
#' @param rotate.x.labs Boolean indicating whether to rotate x axis labels 90 degrees.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#' @param data Data frame containing the data to be plotted.
#' @param x.col Column name of independent variable. If plot type only requires one variable (e.g. histogram), use only one of x.col or y.col.
#' @param y.col Column name of dependent variable. If plot type only requires one variable (e.g. histogram), use only one of x.col or y.col.
#' @param facet.col Column to facet on. If this results in only one facet, it will be used as a subtitle instead.
#' @param n.col.facet Number of columns of facet grid.
#' @param sample.size.col Column containing sample size labels.
#' @param sample.size.loc Either 'xaxis' or 'plot'. 'xaxis' will add sample size to each x axis label. 'plot' will add sample size to the facet label (or subtitle, if only one facet).
#' @param facet.as.subtitle If only one facet, use facet name as subtitle? Defaults to TRUE.
#' @param transform.x Optional x axis transformation. One of 'log10', 'sqrt', or 'reverse'.
#' @param transform.y Optional y axis transformation. One of 'log10', 'sqrt', or 'reverse'.
#'
#' @return A ggplot object.
#'
#' @export
#'
FormatPlot <- function(data, x.col, y.col, facet.col, n.col.facet = 2, sample.size.col, sample.size.loc, plot.title = '', sub.title = '', facet.as.subtitle = TRUE, x.lab = '', y.lab = '', rotate.x.labs = FALSE, ymax, ymin, xmax, xmin, transform.x, transform.y) {

  # Allow for 1 or 2 variables
  if (!missing(y.col) & !missing(x.col)) {
    y.col <- dplyr::enquo(y.col)
    x.col <- dplyr::enquo(x.col)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x.col, y = !!y.col))
  } else if (!missing(x.col)) {
    x.col <- dplyr::enquo(x.col)
    p <- ggplot2::ggplot(data, ggplot2::aes(!!x.col))
  } else if (!missing(y.col)) {
    y.col <- dplyr::enquo(y.col)
    p <- ggplot2::ggplot(data, ggplot2::aes(!!y.col))
  }


  # Create facets if >1 event group, otherwise create subtitle
  if (!missing(facet.col)) {
    facet.col <- dplyr::enquo(facet.col)
    facets <- unique(dplyr::select(data, !!facet.col))
    if (nrow(facets) > 1) {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(!!facet.col), ncol = n.col.facet, scales = 'free_y')
    } else if (sub.title == '' & facet.as.subtitle) {
      sub.title <- facets
    }
  }

  # Add sample size information to either x axis labels or facet/subtitle
  if (!missing(sample.size.col) & !missing(sample.size.loc)) {
    sample.size.col <- dplyr::enquo(sample.size.col)
    if (sample.size.loc == 'xaxis') {
      data %<>% dplyr::mutate(!!x.col := paste0(!!x.col, '\n', !!sample.size.col))
    } else if (sample.size.loc == 'plot' & !missing(facet.col)) {
      data %<>% dplyr::mutate(!!facet.col := paste0(!!facet.col, ' (', !!sample.size.col, ')'))
    } else {
      facet.col <- sample.size.col
    }
  }

  # Add title and subtitle if not blank
  if (!missing(plot.title) & plot.title != '') {
    p <- p + ggplot2::labs(title = plot.title)
  }
  if (!missing(sub.title) & sub.title != '') {
    p <- p + ggplot2::labs(subtitle = sub.title)
  }

  # Add x and y axis titles if not blank
  if (x.lab != "") {
    p <- p + ggplot2::xlab(x.lab)
  } else {
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  if (y.lab != "") {
    p <- p + ggplot2::ylab(y.lab)
  } else {
    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }

  # Rotate x labels 90 degrees if rotate.x.labs is TRUE
  if (!missing(rotate.x.labs)) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }

  # Set ymin and ymax if provided
  if (!missing(ymin) & !missing(ymax)) {
    p <- p + ggplot2::expand_limits(y = c(ymin, ymax))
  } else if (!missing(ymax)) {
    p <- p + ggplot2::expand_limits(y = ymax)
  } else if (!missing(ymin)) {
    p <- p + ggplot2::expand_limits(y = ymin)
  }

  # Set xmin and xmax if provided
  if (!missing(xmin) & !missing(xmax)) {
    p <- p + ggplot2::expand_limits(x = c(xmin, xmax))
  } else if (!missing(xmax)) {
    p <- p + ggplot2::expand_limits(x = xmax)
  } else if (!missing(xmin)) {
    p <- p + ggplot2::expand_limits(x = xmin)
  }

  # Tranform x axis, if transformation specified
  if (!missing(transform.x)) {
    if (transform.x == 'log10') {
      p <- p + ggplot2::scale_x_log10()
    } else if (transform.x == 'sqrt') {
      p <- p + ggplot2::scale_x_sqrt()
    } else if (transform.x == 'reverse') {
      p <- p + ggplot2::scale_x_reverse()
    } else {
      stop(paste0("The x transformation specified, '", transform.x, "' is not a valid option."))
    }
  }

  # Transform y axis, if transformation specified
  if (!missing(transform.y)) {
    if (transform.y == 'log10') {
      p <- p + ggplot2::scale_y_log10()
    } else if (transform.y == 'sqrt') {
      p <- p + ggplot2::scale_y_sqrt()
    } else if (transform.y == 'reverse') {
      p <- p + ggplot2::scale_y_reverse()
    } else {
      stop(paste0("The y transformation specified, '", transform.y, "' is not a valid option."))
    }
  }

  return(p)
}
