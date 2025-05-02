library(pacnvegetation)
library(tidyverse)

#access_dbs <- here(params$data_folder, params$access_dbs)
#LoadPACNVeg(ftpc_params = "pacnveg", eips_paths = access_dbs,
#            cache = TRUE, force_refresh = FALSE)

#--- 1. Read latest cache ----

LoadPACNVeg(force_refresh = FALSE, eips_paths = "foo")



# FTPC user inputs -------------------------------------------------------------

# AGOL downloaded geodatabase info: Kahuku
#var_AGOL_Layer <- "FTPC"
#gdb_name_var <- "FTPC_Kahuku_Maunaloa_20240910.gdb"
#gdb_location_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Parks/HAVO/HAVO"
#gdb_layer_var <- "FTPC_Kahuku_Maunaloa_20240910"

# AGOL downloaded geodatabase info:
var_AGOL_Layer <- "FTPC"
gdb_name_var <- "FTPC_KAHO_20250415.gdb"
gdb_location_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/ArcGIS/Projects/KAHO"
gdb_layer_var <- "FTPC_KAHO_20250415"
gdb_layer_var2 <- "FTPC_KAHO_20250415_PA11"

# temp save location locally
temp_root <- "C:/Users/JJGross/Downloads/Images"

# presumed A drive (Reston):
reston_root <- "A:/Files/FTPC/Database_Images"
#reston_test <- "A:/Files/FTPC/Database_Images/2021/HAVO/Wet_Forest/FT_W_ER_F01_20211115/20211115_F01_CenterT1_CenterT2.jpg"
#reston_root <- "C:/Users/JJGross/OneDrive - DOI/VS_EIPS_Database_images"

# check table ------------------------------------------------------------------
photos_table <- process_photos(AGOL_Layer = var_AGOL_Layer,
                       gdb_name = gdb_name_var,
                       gdb_location = gdb_location_var,
                       gdb_layer = gdb_layer_var2,
                       #test_n_rows = 30,
                       #add_watermark = TRUE
                       return_last_table = TRUE
)

look <- photos_table |>
  select(Sampling_Frame_DB, Site_numb, Subject_FTPC, #Subject_EIPS,
         exif_formatted, field_maps_created_date)

# Number of points in dataset:
length(unique(photos_table$REL_GLOBALID))
length(unique(photos_table$REL_GLOBALID))+ 3#add number dropped here to check
# that all points are present

# Process Photos ---------------------------------------------------------------

process_photos(AGOL_Layer = var_AGOL_Layer,
              gdb_name = gdb_name_var,
              gdb_location = gdb_location_var,
              gdb_layer = gdb_layer_var2,
              add_watermark = TRUE,
              return_last_table = FALSE)

# FIX --------------

# Fix needed in AGOL table?
# Use code below
fix_table <- process_photos(AGOL_Layer = var_AGOL_Layer,
                            gdb_name = gdb_name_var,
                            gdb_location = gdb_location_var,
                            gdb_layer = gdb_layer_var,
                            add_watermark = FALSE,
                            return_first_table = TRUE)

fix_table_subject_first_table <- fix_table |>
  # fix space in subject name (issue with Field Maps schema)
  dplyr::mutate(Subject_FTPC = dplyr::case_when(Subject_FTPC == "StartT3_ CenterT3" ~ "StartT3_CenterT3",
                          .default = as.character(Subject_FTPC)))

fix_table_subject_last_table <- process_photos(AGOL_Layer = var_AGOL_Layer,
                            gdb_name = gdb_name_var,
                            gdb_location = gdb_location_var,
                            gdb_layer = gdb_layer_var,
                            add_watermark = FALSE,
                            insert_first_table = fix_table_subject_first_table,
                            return_last_table = TRUE)

process_photos(AGOL_Layer = var_AGOL_Layer,
               gdb_name = gdb_name_var,
               gdb_location = gdb_location_var,
               gdb_layer = gdb_layer_var,
               add_watermark = TRUE,
               return_last_table = FALSE,
               insert_last_table = fix_table_subject_last_table)


# Quick fix needed in final R labeling table? (consider fixing in R function if possible)
# Use code below
# fix_table <- process_photos(AGOL_Layer = var_AGOL_Layer,
#                gdb_name = gdb_name_var,
#                gdb_location = gdb_location_var,
#                gdb_layer = gdb_layer_var,
#                add_watermark = FALSE,
#                return_last_table = TRUE)
# fix_table_filter <- fix_table |>
#   filter(Site_Name == "EI_W_ER_R33")
# process_photos(AGOL_Layer = var_AGOL_Layer,
#                gdb_name = gdb_name_var,
#                gdb_location = gdb_location_var,
#                gdb_layer = gdb_layer_var,
#                add_watermark = TRUE,
#                return_last_table = FALSE,
#                insert_last_table = fix_table_filter)




# Spreadsheet ------------------------------------------------------------------
photos_table_final <- process_photos(
  AGOL_Layer = var_AGOL_Layer,
  gdb_name = gdb_name_var,
  gdb_location = gdb_location_var,
  gdb_layer = gdb_layer_var,
  add_watermark = FALSE,
  return_last_table = TRUE)

photos_table_final2 <- process_photos(
  AGOL_Layer = var_AGOL_Layer,
  gdb_name = gdb_name_var,
  gdb_location = gdb_location_var,
  gdb_layer = gdb_layer_var2,
  add_watermark = FALSE,
  return_last_table = TRUE)

photos_table_final3 <- photos_table_final |>
  dplyr::bind_rows(photos_table_final2)

photos_table_final <- photos_table_final3

# Get Event ID lookup table ----------------------------------------------------
#** Download latest FTPC and EIPS data first!*

LoadPACNVeg(force_refresh = FALSE, eips_paths = "foo")

# FTPC -------------------------------------------------------------------------

# get events lookup from Events_extra_other table
Events_extra_other_noQA <- FilterPACNVeg(data_name = "Events_extra_other", is_qa_plot = FALSE)
Events_extra_other_QA <- FilterPACNVeg(data_name = "Events_extra_other", is_qa_plot = TRUE)
Events_extra_other <- bind_rows(Events_extra_other_noQA, Events_extra_other_QA)

event_ID_lookup <- Events_extra_other |>
  select(Event_ID, Unit_Code, Community, Sampling_Frame, Cycle, Year, Plot_Number, QA_Plot) |>
  #group_by(Event_ID, Unit_Code, Community, Sampling_Frame, Cycle, Year, Plot_Number, QA_Plot) |>
  dplyr::group_by(Sampling_Frame, Cycle) |>
  dplyr::mutate(Year_Cycle = min(Year)) |>
  dplyr::mutate(QA_Plot = as.logical(QA_Plot)) |>
  dplyr::ungroup()

# Select column names to be in final table

# If table required fix above, insert here:
photos_table_final <- fix_table_subject_last_table

table_out <- photos_table_final |>
  dplyr::mutate(Num_2 = as.integer(Num_2)) |>
  dplyr::mutate(Camera_Type = "Field Maps") |>
  dplyr::mutate(Comments = "") |>
  dplyr::mutate(Community_underscore = stringr::str_replace(Community, " ", "_")) |>
  dplyr::left_join(y = event_ID_lookup, by = dplyr::join_by(Unit_Code, Num_2 == Plot_Number,
                                              Samp_Year == Year_Cycle,
                                              Sampling_Frame_DB == Sampling_Frame,
                                              Community, likely_QA_Plot == QA_Plot)) |>
  dplyr::mutate(reston_link = paste(
    reston_root, Samp_Year, Unit_Code, Community_underscore,
    Folder_Name, Out_Name,  sep = "/"))

table_out_final <- table_out |>
  dplyr::mutate(lub = lubridate::as_datetime(table_out$exif_formatted)) |>
  dplyr::mutate(Date = format(as.POSIXct(lub), format = "%Y-%m-%d")) |>
  dplyr::mutate(Time = format(as.POSIXct(lub), format = "%H:%M")) |>
  select(Sampling_Frame_DB, Samp_Year, File_Name = Out_Name, Subject = Subject2, Date , Time,
         Camera_Type, Comments, Image_Project_Path = reston_link, Event_ID)

# get date and other info for spreadsheet name
today <- lubridate::ymd(today())
today_no_dash <- gsub("-", "", as.character(today))
table_park <- table_out$Unit_Code[1]
table_year <- table_out$Samp_Year[1]
comm_underscore <- table_out$Community_underscore[1]
table_out_name <- paste0(temp_root, "/FTPC_", table_year, "_",
                         table_park, "_", comm_underscore , "_photo_list_", today_no_dash, ".csv")
table_out_name

# create temp local folder and save spreadsheet there
dir.create(temp_root)
write_excel_csv(table_out_final, file = table_out_name)





# EIPS -------------------------------------------------------------------------

# EIPS user inputs -------------------------------------------------------------

# AGOL downloaded geodatabase info:
var_AGOL_Layer <- "EIPS"
gdb_name_var <- "EIPS_Kahuku_Maunaloa_20240909.gdb"
gdb_location_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Parks/HAVO/HAVO"
gdb_layer_var <- "EIPS_Kahuku_Maunaloa_20240909"

# temp save location locally
temp_root <- "C:/Users/JJGross/Downloads/Images"

# presumed A drive (Reston):
reston_root <- "A:/Files/FTPC/Database_Images"



# get events lookup from Events_extra_other table

#** No QA transects to date*
Events_extra_other <- FilterPACNVeg(data_name = "Events_extra_other_EIPS")
EIPS_image_pts <- FilterPACNVeg(data_name = "EIPS_image_pts")

event_ID_lookup <- Events_extra_other |>
  dplyr::right_join(EIPS_image_pts) |>
  select(Event_ID, Image_Point_ID, Image_Point, Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Number) |>
  dplyr::group_by(Sampling_Frame, Cycle) |>
  filter(Cycle == 3) |>
  dplyr::mutate(Year_Cycle = min(Year)) |>
  dplyr::mutate(Transect_Number = as.integer(Transect_Number)) |>
  #mutate(QA_Plot = as.logical(QA_Plot)) |>
  dplyr::ungroup()

# Select column names to be in final table
table_out <- photos_table_final |>
  dplyr::mutate(Num_2 = as.integer(Num_2)) |>
  dplyr::mutate(Camera_Type = "Field Maps") |>
  dplyr::mutate(Comments = "") |>
  dplyr::mutate(Community_underscore = stringr::str_replace(Community, " ", "_")) |>
  dplyr::mutate(Subject_Point = str_extract(Subject_EIPS, "(\\d)+")) |>
  # "Staff" photos do not have a specific "Image_Point" so assign them to Image_Point 0 (ie Start point of transect)
  dplyr::mutate(Subject_Point = dplyr::case_when(stringr::str_detect(Subject_EIPS, pattern = "Staff") ~ "0",
         .default = as.character(Subject_Point))) |>
  dplyr::left_join(y = event_ID_lookup, by = dplyr::join_by(Unit_Code,
                                              Num_2 == Transect_Number,
                                              Subject_Point == Image_Point,
                                              Samp_Year == Year_Cycle,
                                              Sampling_Frame_DB == Sampling_Frame,
                                              Community)) |>
  dplyr::mutate(reston_link = paste(
    reston_root, Samp_Year, Unit_Code, Community_underscore,
    Folder_Name, Out_Name,  sep = "/")) |>
  # Change value back from "0" to "Staff" (after joining) to make clear it is staff photo
  dplyr::mutate(Subject_Point = dplyr::case_when(stringr::str_detect(Subject_EIPS, pattern = "Staff") ~ "Staff",
                                   .default = as.character(Subject_Point)))


table_out_final <- table_out |>
  dplyr::mutate(lub = lubridate::as_datetime(table_out$exif_formatted)) |>
  dplyr::mutate(Date = format(as.POSIXct(lub), format = "%Y-%m-%d")) |>
  dplyr::mutate(Time = format(as.POSIXct(lub), format = "%H:%M")) |>
  select(Sampling_Frame_DB, Samp_Year, File_Name = Out_Name, Transect_Number = TNum_3, Subject = Subject2, Subject_Point, Date , Time,
         Camera_Type, Comments, Image_Project_Path = reston_link, Event_ID, Image_Point_ID)

# get date and other info for spreadsheet name
today <- lubridate::ymd(today())
today_no_dash <- gsub("-", "", as.character(today))
table_park <- table_out$Unit_Code[1]
table_year <- table_out$Samp_Year[1]
comm_underscore <- table_out$Community_underscore[1]
table_out_name <- paste0(temp_root, "/EIPS_", table_year, "_",
                         table_park, "_", comm_underscore , "_photo_list_", today_no_dash, ".csv")
table_out_name

# create temp local folder and save spreadsheet there
dir.create(temp_root)
write_excel_csv(table_out_final, file = table_out_name)


# -----------------------------------------------------------------------------#

