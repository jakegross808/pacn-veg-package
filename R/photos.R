
#' Watermark and process FTPC, EIPS, & Plant Photos
#'
#' @param AGOL_Layer "FTPC" "EIPS" or "Plants"
#' @param gdb_name Name of the geodatabase (Example: "EIPS_Olaa_Nahuku_20220323.gdb")
#' @param gdb_location File path to the geodatabase (Example: "C:/Users/JJGross/Documents/RData/PROJECTS/pacnvegetation/geodatabase")
#' @param gdb_layer The layer file inside the geodatabase (Example: "EIPS_Olaa_Nahuku_20220323")
#' @param return_first_table `default = FALSE` do not return table, process photos as normal; `return_table = TRUE` do not process photos, and return table instead.
#' @param insert_first_table `default = NULL` insert first table created in function. If R object supplied to `insert_first_table = `, then user supplied table will be utilized in function instead of initial geodatabase.
#' @param add_watermark `default = FALSE` no watermark added
#' @param return_last_table `default = FALSE` do not return table, process photos as normal; `return_table = TRUE` do not process photos, and return table instead.
#' @param insert_last_table `default = NULL` insert final table created in function. If R object supplied to `insert_last_table = `, then user supplied table will be used utilized in function instead of geodatabase.
#'
#'
#' @return watermarked photos are saved to a folder "watermarked" inside the working directory. Use here() to determine current working directory.
#' @export
#'
#' @examples
#' \dontrun{
#' process_photos(AGOL_Layer = "EIPS", gdb_name = "EIPS_Olaa_Nahuku_20220323.gdb", gdb_location = "C:/Users/JJGross/Documents/RData/PROJECTS/pacnvegetation/geodatabase", gdb_layer = "EIPS_Olaa_Nahuku_20220323")
#' }

process_photos <- function(AGOL_Layer, gdb_name, gdb_location, gdb_layer,
                           return_first_table = FALSE,
                           insert_first_table = NULL,
                           add_watermark = FALSE,
                           return_last_table = FALSE,
                           insert_last_table = NULL) {

  if(!is.null(insert_first_table)){

    GIS_Table <- insert_first_table

  } else {

    # Function to get Date and Time from exif within photo
    exif_datetime <- function(x){
      x_magick <- magick::image_read(x)
      x_att <- magick::image_attributes(x_magick)
      x_date <- x_att$value[x_att$property == "exif:DateTime"]
      return(x_date)
    }

    # Get joined GIS table + attachment table:
    gdb <- paste0(gdb_location, "/", gdb_name)
    # Layer File with attributes
    attributes <- sf::read_sf(gdb, gdb_layer)

    # Attachments Table containing photos
    layer_attach <- paste0(gdb_layer,"__ATTACH")
    attachments <- sf::read_sf(gdb, layer_attach)
    # The 'DATA' column of the attachments table contains the photos. Each row in
    # the attachments table corresponds to one photo.
    # photo_in_first_row <- attachments$DATA[[1]]

    # (optional) If you tell R that the field is a BLOB, then you can view table in Rstudio
    attachments$DATA <- blob::as_blob(attachments$DATA)
    # ... However, if using automation the apply() function doesn't like blobs
    # so make sure to change column back to a list before feeding into apply()
    # attachments$DATA <- as.list(attachments$DATA)

    # Add date time column to attachments table
    attachments <- attachments |>
      mutate(exif_date_time = purrr::map(DATA, ~exif_datetime(.)))

    GIS_Table <- attachments %>%
      dplyr::left_join(attributes, by = c("REL_GLOBALID" = "GlobalID"))
    pts_with_no_photo <- nrow(dplyr::anti_join(x = attributes, y = attachments, by = c("GlobalID" = "REL_GLOBALID")))
    message(paste(pts_with_no_photo, "points dropped because no photo associated with point"))
  }


  if(return_first_table == TRUE){
    return(GIS_Table)
  }



  # Hawaiian Diacritics
  A. <- "\U0100"
  E. <- "\U0112"
  I. <- "\U012A"
  O. <- "\U014C"
  U. <- "\U016A"
  okina <- "\U02BB"
  a. <- "\U0101"
  e. <- "\U0113"
  i. <- "\U012B"
  o. <- "\U014D"
  u. <- "\U016B"

  Olaa. <- paste0(okina, O., "la", okina, "a")
  Nahuku. <- paste0("N", a., "huku / East Rift")
  Kaloko. <- paste0("Kaloko-Honok", o., "hau")
  Kipahulu. <- paste0("K", i., "pahulu")
  Haleakala. <- paste0("Haleakal", a.)
  Puu_Alii. <- paste0("Pu", okina, "u Ali", okina, "i")
  Hoolehua. <- paste0("Ho", okina, "olehua")
  Tau. <- paste0("Ta", okina, u.)

  GIS_Table1 <- GIS_Table %>%
    dplyr::mutate(Protocol = AGOL_Layer) %>%
    # For some reason 2021 Field maps spells out Hawaii Volcanoes instead
    # of using correct Unit_Code
    dplyr::mutate(Unit_Code = dplyr::case_when(
      Unit_Code == "Hawaii Volcanoes National Park" ~ paste0("HAVO"))) %>%
    dplyr::mutate(Unit_Name = dplyr::case_when(
      Unit_Code == "HAVO" ~ paste0("Hawai", okina, "i Volcanoes National Park"),
      Unit_Code == "KAHO" ~ paste0(Kaloko., " National Historical Park"),
      Unit_Code == "HALE" ~ paste0(Haleakala., " National Historical Park"),
      #Samp_Frame == "KH" ~ paste0(Kaloko., " National Historical Park"),
      #Samp_Frame == "ML" ~ paste0("Hawai", okina, "i Volcanoes National Park"),
      #Samp_Frame == "KU" ~ paste0("Hawai", okina, "i Volcanoes National Park")
      )) %>%
    dplyr::mutate(Sampling_Frame_DB = dplyr::case_when(is.na(Samp_Frame) ~ "NA",
                                                    Samp_Frame == "OL" ~ "Olaa",
                                                    Samp_Frame == "ER" ~ "Nahuku/East Rift",
                                                    Samp_Frame == "KU" ~ "Kahuku",
                                                    Samp_Frame == "ML" ~ "Mauna Loa",
                                                    Samp_Frame == "KH" ~ "Kaloko-Honokohau",
                                                    Samp_Frame == "KD" ~ "Kipahulu District",
                                                    Samp_Frame == "HA" ~ "Haleakala",
                                                    Samp_Frame == "PA" ~ "Puu Alii",
                                                    Samp_Frame == "HO" ~ "Hoolehua",
                                                    Samp_Frame == "KW" ~ "Kalawao",
                                                    Samp_Frame == "TT" ~ "Tutuila",
                                                    Samp_Frame == "TA" ~ "Tau",
                                                    Samp_Frame == "GU" ~ "Guam",
                                                    Samp_Frame == "MU" ~ "Muchot",

    )) %>%
    dplyr::mutate(Sampling_Frame = dplyr::case_when(is.na(Samp_Frame) ~ "NA",
                                                       Samp_Frame == "OL" ~ Olaa.,
                                                       Samp_Frame == "ER" ~ Nahuku.,
                                                       Samp_Frame == "KU" ~ "Kahuku",
                                                       Samp_Frame == "ML" ~ "Mauna Loa",
                                                       Samp_Frame == "KH" ~ Kaloko.,
                                                       Samp_Frame == "KD" ~ Kipahulu.,
                                                       Samp_Frame == "HA" ~ Haleakala.,
                                                       Samp_Frame == "PA" ~ Puu_Alii.,
                                                       Samp_Frame == "HO" ~ Hoolehua.,
                                                       Samp_Frame == "KW" ~ "Kalawao",
                                                       Samp_Frame == "TT" ~ "Tutuila",
                                                       Samp_Frame == "TA" ~ Tau.,
                                                       Samp_Frame == "GU" ~ "Guam",
                                                       Samp_Frame == "MU" ~ "Muchot",
    )) %>%
    dplyr::mutate(Community = dplyr::case_when(is.na(Community) ~ "NA",
                                               Community == "WF" | Community == "W" ~ "Wet Forest",
                                               Community == "SS" | Community == "S" ~ "Subalpine Shrubland",
                                               Community == "LF" | Community == "L" ~ "Limestone Forest",
                                               Community == "MF" | Community == "M" ~ "Mangrove Forest",
                                               Community == "CS" | Community == "C" ~ "Coastal Strand",
                                               TRUE ~ as.character(Community))) %>%

    dplyr::mutate(Comm = dplyr::case_when(is.na(Community) ~ "NA",
                                          Community == "Wet Forest" ~ "W",
                                          Community == "Subalpine Shrubland" ~ "S",
                                          Community == "Limestone Forest" ~ "L",
                                          Community == "Mangrove Forest" ~ "M",
                                          Community == "Coastal Strand" ~ "C")) %>%


    dplyr::mutate(Site_Number = readr::parse_number(Site_numb, na = c("Misc Other", "EIPS no transect", "EIPS other", "FTPC No Plot", "FTPC Other"))) %>%
    dplyr::mutate(Site_Type = dplyr::case_when(Protocol == "FTPC" & Site_Number <= 15 & Community == "Wet Forest" ~ "Fixed",
                                               Protocol == "FTPC" & Site_Number <= 15 & Community == "Subalpine Shrubland" ~ "Fixed",
                                               Protocol == "FTPC" & Site_Number <= 10 & Community == "Coastal Strand" ~ "Fixed",
                                               Protocol == "FTPC" & Site_Number <= 10 & Community == "Mangrove Wetland" ~ "Fixed",
                                               Protocol == "FTPC" & Site_Number <= 10 & Community == "Limestone Forest" ~ "Fixed",
                                               Protocol == "EIPS" & Unit_Code == "AMME" ~ "Fixed",
                                               Protocol == "EIPS" & Unit_Code != "AMME" & Site_Number <= 15 & Sampling_Frame == "Kahuku" ~ "Fixed",
                                               Protocol == "EIPS" & Unit_Code != "AMME" & Site_Number <= 10 & Sampling_Frame != "Kahuku" ~ "Fixed",
                                               TRUE ~ "Rotational"))


  if(AGOL_Layer == "FTPC"){
    GIS_Table1 <- GIS_Table1 %>%
      dplyr::mutate(Subject1 = Subject_FTPC)}

  if(AGOL_Layer == "EIPS"){
    GIS_Table1 <- GIS_Table1 %>%
      dplyr::mutate(Subject1 = Subject_EIPS)}

  if(AGOL_Layer == "Plants"){
    GIS_Table1 <- GIS_Table1 %>%
      tidyr::separate(Photo_Taxon, c("A", "B"), ";") %>%
      tidyr::separate(B, c("common", "lifeform", "nativity"), " / ") %>%
      tidyr::separate(A, c("species", "C"), " \\(") %>%
      tidyr::separate(C, c("code", "family"), "\\)") %>%
      dplyr::mutate(family = trimws(family)) %>%
      dplyr::mutate(Subject1 = species)
  }

  if (any(GIS_Table1$Subject1 == "Other", na.rm = TRUE)) {
    GIS_Table1 <- GIS_Table1 %>%
      dplyr::mutate(Subject2 = dplyr::case_when(Subject1 == "Other" ~ paste(Subject1, Subject_other, sep = "_"),
                                                TRUE ~ as.character(Subject1)))
  } else {
    GIS_Table1 <- GIS_Table1 %>%
      dplyr::mutate(Subject2 = Subject1)
  }

  # Date/Time-----------------------------------------------------

  # New system uses the photo's exif date/time (instead of field maps point)
  GIS_Table2 <- GIS_Table1 |>
    # replace colons with dashes between year-month and month-date
    mutate(exif_formatted = stringr::str_replace(exif_date_time,":", "-")) |>
    mutate(exif_formatted = stringr::str_replace(exif_formatted,":", "-")) |>
    # remove colons completely for file naming
    mutate(exif_date_time2 = stringr::str_remove_all(exif_date_time, ":")) |>
    separate(exif_date_time2, into = c("File_Date", "File_Time"), sep = " (?=[^ ]+$)") |>
    # create column File_DT for file date+time -> YYYYMMDD_HHMMSS
    dplyr::mutate(File_DT = paste(File_Date, File_Time, sep = "_")) |>
    dplyr::rename(field_maps_created_date = created_date)

  # Old system (below) used 'created_date' from Field Maps, but this caused problems,
  # because whenever GIS Specialist saved new copy of AGOL layer, this date would change.

  # GIS_Table2 <- GIS_Table1 %>%
  #   # Parse first "word" (ie date) from created_date
  #   dplyr::mutate(File_Date = stringr::word(created_date, 1)) %>%
  #   # Remove dashes "-" between YYYY-MM-DD
  #   dplyr::mutate(File_Date = stringr::str_replace_all(File_Date, "-", "")) %>%
  #   # Parse second "word" (ie time) from created_date
  #   dplyr::mutate(File_Time = stringr::word(created_date, 2)) %>%
  #   # Remove dashes "-" between YYYY-MM-DD
  #   dplyr::mutate(File_Time = stringr::str_replace_all(File_Time, ":", "")) %>%
  #   # create column File_DT for file date+time -> YYYYMMDD_HHMMSS
  #   dplyr::mutate(File_DT = paste(File_Date, File_Time, sep = "_"))

  # Naming Abbreviations------------------------------------
  # Used in folder and file names (ex. FT_L_LF_F01_20190614)
  GIS_Table3 <- GIS_Table2 %>%
    dplyr::mutate(Prot_2 = stringr::str_sub(Protocol, end = 2)) %>%
    dplyr::mutate(Comm_1 = Comm) %>%
    dplyr::mutate(Samp_2 = Samp_Frame) %>%
    dplyr::mutate(Type_1 = stringr::str_sub(Site_Type, end = 1)) %>%
    dplyr::mutate(Num_2 = stringr::str_pad(Site_Number, 2, pad = "0")) %>%
    dplyr::mutate(TNum_3 = paste0(Type_1, Num_2)) %>%
    dplyr::mutate(Site_Name = paste(Prot_2, Comm_1,
                                    Samp_2, TNum_3, sep = "_"))

  # handle QAQC plots
  GIS_Table3 <- GIS_Table3 |>
    mutate(as_date = lubridate::as_datetime(exif_formatted)) |>
    group_by(Site_Name) |>
    mutate(first_photo = min(as_date)) |>
    mutate(time_span = lubridate::seconds_to_period(as_date-first_photo)) |>
    mutate(more_than_14_day_span = case_when(time_span > days(14) ~ TRUE,
                                             .default = FALSE)) |>
    mutate(likely_QA_Plot = more_than_14_day_span) |>
    ungroup()

  if (any(GIS_Table3$likely_QA_Plot == TRUE)) {
    message(paste("dataset contains photos located in same plot spanning more than 14 days.
                These photos were marked as likely QA plots"))
  }

  # If plot/transect takes more than 1 day to complete, use first date
  # Create table that shows one date per plot/transect:
  first_date <- GIS_Table3 %>%
    dplyr::select(Site_Name, likely_QA_Plot, File_Date) %>%
    dplyr::group_by(Site_Name, likely_QA_Plot) %>%
    dplyr::arrange(File_Date) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(Folder_Name = paste(Site_Name, File_Date, sep = "_")) %>%
    dplyr::select(-File_Date)


  # File Naming ------------------------------------
  # Create Input and Output File Names
  if(AGOL_Layer == "FTPC" | AGOL_Layer == "EIPS"){
    GIS_Table4 <- GIS_Table3 %>%
      dplyr::left_join(first_date, by = join_by(Site_Name, likely_QA_Plot)) %>%
      # Out_Name
      dplyr::mutate(Out_Name = paste(File_Date, TNum_3, Subject2, sep = "_")) %>%
      dplyr::group_by(Out_Name) %>%
      dplyr::mutate(Out_Name = if(dplyr::n() > 1) {paste(Out_Name, stringr::str_pad(dplyr::row_number(), 3, pad = "0"), sep = "_")}
                    else {paste0(Out_Name)}) %>%
      dplyr::mutate(Out_Name = paste0(Out_Name, ".jpg")) %>%
      dplyr::ungroup()

  }

  if(AGOL_Layer == "Plants"){
    # If ID_final contains NA this will be TRUE
    #final_sp_id_missing <- sum(is.na(GIS_Table3$ID_final)) > 0


    GIS_Table4 <- GIS_Table3 %>%
      dplyr::left_join(first_date, by = "Site_Name") %>%
      # Out_Name for processed photos
      #dplyr::mutate(Out_Name = stringr::str_remove(Subject2, "\\.")) %>%
      #dplyr::mutate(Out_Name = ifelse(is.na(Out_Name), REL_GLOBALID,
      #                                ifelse(Out_Name >= 0, Out_Name, "error"))) %>%
      # If ID_final contains NA use field ID (i.e. "species") (from Subject2) instead
      #dplyr::mutate(Out_Name = if(final_sp_id_missing) {paste(File_Time, Out_Name, sep = "_")}
      #              else {paste(File_Time, ID_final, sep = "_")}) %>%
      dplyr::mutate(Subject2 = stringr::str_remove(Subject2, "\\.")) %>%
      dplyr::mutate(ID_final2 = stringr::str_remove(ID_final, "\\.")) |>
      dplyr::mutate(Out_Name = ID_final2) |>
      dplyr::mutate(Out_Name = dplyr::case_when(is.na(ID_final2) ~ Subject2,
                                                .default = ID_final2)) |>
      dplyr::mutate(Out_Name = paste(File_Time, Out_Name, sep = "_")) |>
      dplyr::arrange(exif_formatted, ATT_NAME) %>%
      dplyr::group_by(Out_Name) %>%
      dplyr::mutate(Out_Name = if(dplyr::n() > 1) {paste(Out_Name, str_pad(row_number(), 2, pad = "0"), sep = "_")}
                    else {paste0(Out_Name)}) %>%
      dplyr::mutate(Out_Name = paste0(Out_Name, ".jpg")) %>%
      dplyr::mutate(Folder_Name = File_Date) %>%
      dplyr::ungroup()

  }

  GIS_Table4$exif_formatted <- as.character(GIS_Table4$exif_formatted)

  if(return_last_table == TRUE){
    return(GIS_Table4)
  }

  if(!is.null(insert_last_table)){
    GIS_Table4 <- insert_last_table
  }


  # Fix additional photos using changes here:
  #GIS_Table4 <- GIS_Table4 %>%
  #  filter(Site_Number == 54) %>%
  #  mutate(DT_HST = "2021-04-16")

  # apply() function doesn't like blobs so change to list before running apply()
  GIS_Table4$DATA <- as.list(GIS_Table4$DATA)

  # applyr the "watermark" function to each record (ie photo)
  if(add_watermark == TRUE){
    apply(X = GIS_Table4, MARGIN = 1, FUN = watermark, new_folder = "watermarked")
  }

  if(add_watermark == FALSE){
    apply(X = GIS_Table4, MARGIN = 1, FUN = watermark_no, new_folder = "no_watermark")
  }

  #print warning message to remind user to move photos to sharepoint
  print("Remember to copy images from local folder to vital signs folders: 1) images folder and 2) database folder")

}



#' Watermark and process FTPC, EIPS, & Plant Photos
#'
#' @param x GIS table
#' @param new_folder folder where watermarked photos are saved
#'
#' @return an exported watermarked photo
#' @export
#'
#' @examples
#' \dontrun{
#' watermark(GIS_Table, "watermarked")
#' }

watermark <- function(x, new_folder) {
  # Function to apply to each row (i.e. photo) in the GIS_Table:
  # Get data from GIS table (x)
  p.date <- x["exif_formatted"]
  p.unit <- x["Unit_Name"]
  p.protocol <- x["Protocol"]
  p.community <- x["Community"]
  p.sf <- x["Sampling_Frame"]
  if (p.sf == "Kaloko-Honok\U014Dhau") {
    p.sf <- p.community
  }
  p.tran <- x["Trans_Num"]
  p.site_folder <- x["Folder_Name"]
  p.type_num <- x["TNum_3"]
  p.subject <- x["Subject2"]
  p.prot_type_num <- paste0(p.protocol, " - ", p.type_num)

  # Create paths and folders to save each photo
  dir.create(here::here(new_folder, p.site_folder), recursive = TRUE, showWarnings = FALSE )
  out.path <- here::here(new_folder, p.site_folder)
  out.name <- file.path(out.path, x["Out_Name"])
  print(out.name)

  # Load photo
  image.x <- x["DATA"] %>%
    purrr::pluck(1)
  #print(paste("structure of photo = ", print(str(image.x))))

  img.x1 <- magick::image_read(image.x)
  # Apply auto-orientation "image_orient()" which tries to infer the correct orientation
  #' from the Exif data.
  img.x2 <- magick::image_orient(img.x1)
  #print(image_attributes(img.x))


  # ---- Watermark photo -----

  # northwest corner
  nw1 <- p.unit
  nw2 <- p.sf
  nw <- paste(nw1, nw2, sep = "\n")
  img.x3 <- magick::image_annotate(img.x2, nw,
                          size = 25,
                          gravity = "northwest",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  # northeast corner
  ne <- paste(p.prot_type_num, p.subject, sep = "\n")
  img.x4 <- magick::image_annotate(img.x3, ne,
                          size = 25,
                          gravity = "northeast",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  # southwest corner
  sw <- paste(p.date)
  img.x5 <- magick::image_annotate(img.x4, sw,
                          size = 25,
                          gravity = "southwest",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)

  # Save photo
  magick::image_write(img.x5, path = out.name, format = "jpg")
}

#' Process FTPC, EIPS, & Plant Photos without a watermark
#'
#' @param x GIS table
#' @param new_folder folder where watermarked photos are saved
#'
#' @return an exported watermarked photo
#' @export
#'
#' @examples
#' \dontrun{
#' watermark_no(GIS_Table, "new_folder")
#' }

watermark_no <- function(x, new_folder) {
  # Function to apply to each row (i.e. photo) in the GIS_Table:
  # Get data from GIS table (x)

  p.date <- x["exif_formatted"]
  #p.unit <- x["Unit_Name"]
  #p.protocol <- x["Protocol"]
  #p.community <- x["Community"]
  #p.sf <- x["Sampling_Frame"]
  #p.tran <- x["Trans_Num"]
  p.site_folder <- x["Folder_Name"]
  #p.type_num <- x["TNum_3"]
  p.subject <- x["Subject2"]
  #p.prot_type_num <- paste0(p.protocol, " - ", p.type_num)

  # Create paths and folders to save each photo
  dir.create(here::here(new_folder, p.site_folder), recursive = TRUE, showWarnings = FALSE )
  out.path <- here::here(new_folder, p.site_folder)
  out.name <- file.path(out.path, x["Out_Name"])
  print(out.name)

  # Load photo
  image.x <- x["DATA"] %>%
    purrr::pluck(1)
  #print(paste("structure of photo = ", print(str(image.x))))

  img.x1 <- magick::image_read(image.x)
  # Apply auto-orientation "image_orient()" which tries to infer the correct orientation
  #' from the Exif data.
  img.x2 <- magick::image_orient(img.x1)
  #print(image_attributes(img.x))

  # Save photo
  magick::image_write(img.x2, path = out.name, format = "jpg")
}
