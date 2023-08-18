

dbpath <- "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db/PACN_veg_species_list_20230810.accdb"
pacnveg_master_spp_list <- read_vegspp_db(dbpath)



#' Load and Query The PACN Veg Species Database
#' Vital Signs > 05_focal_terr_plant_communities > Data > Database > Veg_species_db
#'
#' @param db_paths Database path (downloaded from sharepoint to location on computer)
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' dbpath <- "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db/PACN_veg_species_list_20230810.accdb"
#' pacnveg_master_spp_list <- read_vegspp_db(dbpath)
#'
#' }
read_vegspp_db <- function(db_paths) {
  conn_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", db_paths)

  # Establish connection with access database
  conn <- DBI::dbConnect(odbc::odbc(), .connection_string = conn_string)

  # Download Species info table
  tlu_Species <- dplyr::tbl(conn, "tlu_Species") %>%
    dplyr::collect()
  names(tlu_Species)

  # Download park specific Species checklists
  xref_Park_Species <- dplyr::tbl(conn, "xref_Park_Species") %>%
    dplyr::collect()

  # Join Park Species checklists to with the additional species information
  pacnveg_master_spp_list <- tlu_Species %>%
    dplyr::right_join(xref_Park_Species, by = c("Species_ID", "TSN"), suffix = c("","_park"))

  return(pacnveg_master_spp_list)

}



