#' Combine species data from Veg Map Databases
#'
#' @param db_paths Database path or list of database paths pointing to
#'
#' @return A tibble with one row per location and columns Protocol, Unit_Code, Sampling_Frame, Sample_Unit_Number, Lat, Long, Cycles, Years
#' @export
#'
#' @examples
#' \dontrun{
#'
#' hi_vegmap_db_paths <- c("C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/havodata.accdb",
#'                        "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/haledata.accdb",
#'                        "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/kahodata.mdb",
#'                        "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/kaladata.mdb",
#'                        "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/puhedata.mdb",
#'                        "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/puhodata.mdb")
#'
#' # Veg map & species locations ----
#'
#' Hawaii_vegmap_data <- read_vegmap_db(hi_vegmap_db_paths)
#' }
read_vegmap_db <- function(db_paths) {
  conn_strings <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", db_paths)

  # Tables
  Veg_Map_Data <- tibble::tibble()

  for (conn_string in conn_strings) {
    conn <- DBI::dbConnect(odbc::odbc(), .connection_string = conn_string)
    #Sites
    tbl_Sites <- dplyr::tbl(conn, "tPlotEvents") %>%
      dplyr::select(Plot_Event, Plot_Code, Event_Date, Field_X, Field_Y, GPS_Error, UTM_Zone,
                    Plot_Shape, Plot_Radius, Plot_Width, Plot_Length, Observation_pt) %>%
      dplyr::collect()
    names(tbl_Sites)

    #Species Code
    tbl_Species_Event_Code <- dplyr::tbl(conn, "tPlotEventSpecies") %>%
      dplyr::select(Plot_Event, Spp_Code) %>%
      dplyr::collect()
    names(tbl_Species_Event_Code)

    # Species
    tbl_Species <- dplyr::tbl(conn, "tSpecies") %>%
      dplyr::select(Spp_Code, Field_Name, PLANTS_Symbol) %>%
      dplyr::collect()
    names(tbl_Species)

    # Species
    tbl_Family <- dplyr::tbl(conn, "xPLANTS_lu") %>%
      dplyr::collect()
    names(tbl_Family)

    spp_sites <- tbl_Species_Event_Code %>%
      dplyr::left_join(tbl_Species, by = c("Spp_Code" = "Spp_Code")) %>%
      dplyr::left_join(tbl_Sites, by = "Plot_Event")

    if ("Species_Code" %in% names(tbl_Family)) {
      tbl_Family <- tbl_Family %>%
        dplyr::select(Species_Code, Sci_Name, ComName, SciName_w_Auth, Family, ITIS_TSN)
      spp_sites <- spp_sites %>%
        dplyr::left_join(tbl_Family, by = c("Spp_Code" = "Species_Code"))
    } else {
      spp_sites <- spp_sites %>%
        dplyr::left_join(tbl_Family, by = c("PLANTS_Symbol" = "Plants_Symbol"))
    }

    # Transform from UTM NAD83 to WGS84 lat long

    if (length(unique(spp_sites$UTM_Zone)) != 1) {
      stop("Zero UTM zones present, or more than one UTM Zone present in dataset")
    } else if (unique(spp_sites$UTM_Zone) == "5N") {
      crs_var <- terra::crs("+init=epsg:26905")
    } else if (unique(spp_sites$UTM_Zone) == "4N") {
      crs_var <- terra::crs("+init=epsg:26904")
    } else if (unique(spp_sites$UTM_Zone) == "55N") {
      crs_var <- terra::crs("+init=epsg:8693")
    } else {stop("UTM to latlong transformation not defined in function")}

    x <- spp_sites$Field_X
    y <- spp_sites$Field_Y
    points <- cbind(x,y)
    points_utm <- terra::vect(points, crs=crs_var)
    points_lat_long <- terra::project(points_utm, "+proj=longlat +datum=WGS84")
    xy <- terra::geom(points_lat_long)[, c("y", "x")]
    latlong <- dplyr::as_tibble(xy) %>%
      dplyr::rename(lat = y, long = x)

    # Bind new lat long columns to
    spp_sites_latlong <- cbind(spp_sites, latlong)

    Veg_Map_Data <- unique(rbind(Veg_Map_Data, spp_sites_latlong))

    DBI::dbDisconnect(conn)

  } # End for loop

  data <- Veg_Map_Data

  return(data)

}
