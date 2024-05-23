library(pacnvegetation)
library(tidyverse)


# FTPC user inputs -------------------------------------------------------------

# AGOL downloaded geodatabase info:
gdb_name_var <- "FTPC_Olaa_Nahuku_20240521_B.gdb"
gdb_location_var <- "C:/Users/JJGross/Downloads/Temp_Geodatabase"
gdb_layer_var <- "FTPC_Olaa_Nahuku_20240521_B"

# temp save location locally
temp_root <- "C:/Users/JJGross/Downloads/Images"

# presumed A drive (Reston):
reston_root <- "A:/Files/FTPC/Database_Images"
reston_test <- "A:/Files/FTPC/Database_Images/2021/HAVO/Wet_Forest/FT_W_ER_F01_20211115/20211115_F01_CenterT1_CenterT2.jpg"

# check table ------------------------------------------------------------------
photos_table <- process_photos(AGOL_Layer = "FTPC",
                       gdb_name = gdb_name_var,
                       gdb_location = gdb_location_var,
                       gdb_layer = gdb_layer_var,
                       #test_n_rows = 30,
                       #add_watermark = TRUE
                       return_last_table = TRUE
)

look <- photos_table |>
  select(Sampling_Frame_DB, Site_numb, Subject_FTPC, exif_formatted, field_maps_created_date)

# Number of points in dataset:
length(unique(photos_table$REL_GLOBALID))
length(unique(photos_table$REL_GLOBALID))+ 17 #add number dropped here to check
# that all points are present

# Process Photos ---------------------------------------------------------------

#process_photos(AGOL_Layer = "FTPC",
#               gdb_name = gdb_name_var,
#               gdb_location = gdb_location_var,
#               gdb_layer = gdb_layer_var,
#               add_watermark = TRUE,
#               return_last_table = FALSE)

# Spreadsheet ------------------------------------------------------------------
photos_table_final <- process_photos(
  AGOL_Layer = "FTPC",
  gdb_name = gdb_name_var,
  gdb_location = gdb_location_var,
  gdb_layer = gdb_layer_var,
  add_watermark = FALSE,
  return_last_table = TRUE)

# Get Event ID lookup table ----------------------------------------------------
#** Download latest FTPC and EIPS data first!*

# Write/Read csv from pacnvegetation package:
pacnveg_cache_path <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/R_WritePACNVeg"

# Read
path_file_info <- file.info(list.files(pacnveg_cache_path, full.names = T))
latest_folder <- rownames(path_file_info)[which.max(path_file_info$mtime)]

LoadPACNVeg(data_path = latest_folder,
            data_source = "file")

# get events lookup from Events_extra_other table
Events_extra_other <- FilterPACNVeg(data_name = "Events_extra_other")
event_ID_lookup <- Events_extra_other |>
  select(Event_ID, Unit_Code, Community, Sampling_Frame, Cycle, Year, Plot_Number) |>
  group_by(Event_ID, Unit_Code, Community, Sampling_Frame, Cycle, Year, Plot_Number) |>
  group_by(Sampling_Frame, Cycle) |>
  mutate(Year = min(Year)) |>
  ungroup()

# Select column names to be in final table
table_out <- photos_table_final |>
  mutate(Num_2 = as.integer(Num_2)) |>
  mutate(Camera_Type = "Field Maps") |>
  mutate(Comments = "") |>
  mutate(Community_underscore = str_replace(Community, " ", "_")) |>
  left_join(y = event_ID_lookup, by = join_by(Unit_Code, Num_2 == Plot_Number,
                                              Samp_Year == Year,
                                              Sampling_Frame_DB == Sampling_Frame,
                                              Community)) |>
  mutate(reston_link = paste(
    reston_root, Samp_Year, Unit_Code, Community_underscore,
    Folder_Name, Out_Name,  sep = "/"))

table_out_final <- table_out |>
  select(Sampling_Frame_DB, Samp_Year, File_Name = Out_Name, Subject = Subject2, Time = File_Time,
         Camera_Type, Comments, Image_Project_Path = reston_link, Event_ID)

# get date and other info for spreadsheet name
today <- lubridate::ymd(today())
today_no_dash <- gsub("-", "", as.character(today))
table_park <- table_out$Unit_Code[1]
table_year <- table_out$Samp_Year[1]
table_out_name <- paste0(temp_root, "/FTPC_", table_park, "_",
                         table_year, "_photo_list_", today_no_dash, ".csv")
table_out_name

# create temp local folder and save spreadsheet there
dir.create(temp_root)
write_excel_csv(table_out_final, file = table_out_name)


# -----------------------------------------------------------------------------#

