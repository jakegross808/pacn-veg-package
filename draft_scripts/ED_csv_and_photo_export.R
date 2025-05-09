Park <- "HAVO"
ED_Database <- "C:/Users/JJGross/Downloads/ED_2022.gdb"
AGOL_Layer <- "ED"

gdb_name <- "ED_2022.gdb"
gdb_location <- "C:/Users/JJGross/Downloads/"
gdb_layer <- "ED_HAVO_2022"

Species_Database_Folder <- "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"

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

GIS_Table <- attachments %>%
  dplyr::left_join(attributes, by = c("REL_GLOBALID" = "GlobalID"))

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

GIS_Table1 <- GIS_Table

if(AGOL_Layer == "ED"){
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

# Make new date and time columns that can be used in file names

GIS_Table2 <- GIS_Table1 %>%
  # Parse first "word" (ie date) from created_date
  dplyr::mutate(File_Date = stringr::word(created_date, 1)) %>%
  # Remove dashes "-" between YYYY-MM-DD
  dplyr::mutate(File_Date = stringr::str_replace_all(File_Date, "-", "")) %>%
  # Parse second "word" (ie time) from created_date
  dplyr::mutate(File_Time = stringr::word(created_date, 2)) %>%
  # Remove dashes "-" between YYYY-MM-DD
  dplyr::mutate(File_Time = stringr::str_replace_all(File_Time, ":", "")) %>%
  # create column File_DT for file date+time -> YYYYMMDD_HHMMSS
  dplyr::mutate(File_DT = paste(File_Date, File_Time, sep = "_"))

# Naming Abbreviations------------------------------------
# Used in folder and file names (ex. FT_L_LF_F01_20190614)
GIS_Table3 <- GIS_Table2 %>%
  dplyr::mutate(Prot_2 = stringr::str_sub(Protocol, end = 2))

# If plot/transect takes more than 1 day to complete, use first date
# Create table that shows one date per plot/transect:
first_date <- GIS_Table3 %>%
  dplyr::select(File_Date) %>%
  dplyr::arrange(File_Date) %>%
  dplyr::slice(1) %>%
  dplyr::mutate(Folder_Name = paste(File_Date, sep = "_")) %>%
  dplyr::select(-File_Date)


# File Naming ------------------------------------
# Create Input and Output File Names

if(AGOL_Layer == "ED"){
  GIS_Table4 <- GIS_Table3 %>%
    # If'New_sp_name' is blank & full species name is entered in Photo_Taxon
    # with a level of confidence is "5", then, use that as species name:
    mutate(Subject3 = case_when(is.na(New_sp_name) ~ Subject2,
                                   .default = as.character(New_sp_name))) %>%
    #dplyr::left_join(first_date, by = "Site_Name") %>%
    # Out_Name for processed photos
    dplyr::mutate(Out_Name = stringr::str_remove(Subject3, "\\.")) %>%
    dplyr::mutate(Out_Name = ifelse(is.na(Out_Name), REL_GLOBALID,
                                    ifelse(Out_Name >= 0, Out_Name, "error"))) %>%
    #dplyr::mutate(Out_Name = paste(File_Time, Out_Name, sep = "_")) %>%
    #Changed here so that file name uses species from Final ID instead of species from Field ID
    dplyr::mutate(Out_Name = paste(File_Date, File_Time, Out_Name, sep = "_")) %>%
    dplyr::arrange(created_date, ATT_NAME) %>%
    dplyr::group_by(Out_Name) %>%
    dplyr::mutate(Out_Name = if(dplyr::n() > 1) {paste(Out_Name, str_pad(row_number(), 2, pad = "0"), sep = "_")}
                  else {paste0(Out_Name)}) %>%
    dplyr::mutate(Out_Name = paste0(Out_Name, ".jpg")) %>%
    dplyr::mutate(Folder_Name = "ED_Photos") %>%
    dplyr::mutate(Photo_path = paste(Folder_Name, Out_Name, sep = "\\"))
    dplyr::ungroup()

}

GIS_Table4$created_date <- as.character(GIS_Table4$created_date)

GIS_Table_Final <- GIS_Table4 %>%
  select(-Subject1, -Subject2, -File_Date, -File_Time, -File_DT, -Prot_2)

write_csv(GIS_Table_Final, "Early_Detection.csv")

#if(return_table == TRUE){
#  return(GIS_Table4)
#}


# Fix additional photos using changes here:
#GIS_Table4 <- GIS_Table4 %>%
#  filter(Site_Number == 54) %>%
#  mutate(DT_HST = "2021-04-16")

# apply() function doesn't like blobs so change to list before running apply()
GIS_Table4$DATA <- as.list(GIS_Table4$DATA)

# applyr the "watermark" function to each record (ie photo)
if(AGOL_Layer != "ED"){
  apply(X = GIS_Table4, MARGIN = 1, FUN = watermark, new_folder = "watermarked")
}

if(AGOL_Layer == "ED"){
  apply(X = GIS_Table4, MARGIN = 1, FUN = watermark_no, new_folder = "no_watermark")
}

#print warning message to remind user to move photos to sharepoint
print("Remember to copy images from local folder to vital signs folders: 1) images folder and 2) database folder")

}



#' Process ED Photos without a watermark
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

  p.date <- x["created_date"]
  #p.unit <- x["Unit_Name"]
  #p.protocol <- x["Protocol"]
  #p.community <- x["Community"]
  #p.sf <- x["Sampling_Frame"]
  #p.tran <- x["Trans_Num"]
  p.site_folder <- x["Folder_Name"]
  #p.type_num <- x["TNum_3"]
  p.subject <- x["Subject3"]
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

