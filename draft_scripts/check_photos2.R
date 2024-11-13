
#install.packages("arcgisbinding")
install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")

# Load local geodatabase
#chk <- process_photos(AGOL_Layer = "FTPC",
#                      gdb_name = "FTPC_OL_ER_20220503.gdb",
#                      gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
#                      gdb_layer = "FTPC_OL_ER_20220503",
#                      return_table = TRUE)

#### load packages ----
library(tidyverse)
library(arcgisbinding)
library(sf)
library(pacnvegetation)


# Check photos directly from AGOL ----------------------------------------------


# Open ArcGIS Pro to establish valid product
arc.check_product()
#check portal connections and active portal login
arc.check_portal()
#NOTE: you will need to be logged in to nps.maps.arcgis.com in ArcGIS Pro for the bridge to function, but you do not have to keep Pro open

url_FTPC_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/FTPC_Points_Photos_HAVO_2021/FeatureServer/1"

url_FTPC_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_FTPC_Sampling_Points_Photos/FeatureServer/30"

url_Plants_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/FTPC_Points_Photos_HAVO_2021/FeatureServer/1"



# Get agol layers as sf objects:
FTPC_2021 <- arc.data2sf(arc.select(arc.open(url_FTPC_2021)))
FTPC_2022 <- arc.data2sf(arc.select(arc.open(url_FTPC_2022)))

Plants_2021 <- arc.data2sf(arc.select(arc.open(url_Plants_2021)))

chk <- FTPC_photos

# FTPC photo Check ------------------------------------------------------------
# Get list of missing fixed photos
chk_fixed <- chk %>%
  filter(Site_Type == "Fixed") %>%
  filter(Subject1 != "Staff_Photo") %>%
  filter(Subject1 != "Other") %>%
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::ungroup() %>%
  complete(tidyr::nesting(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type), Subject1) %>%
  filter(is.na(n))

# Get list of missing rotational photos
chk_rotational <- chk %>%
  filter(Site_Type == "Rotational") %>%
  filter(Subject1 != "Staff_Photo") %>%
  filter(Subject1 != "Other") %>%
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::ungroup() %>%
  complete(tidyr::nesting(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type), Subject1) %>%
  filter(is.na(n))

# Combine fixed and rotational into one table
chk_missing <- bind_rows(chk_fixed, chk_rotational)


# Create a table of duplicate points
# missing photos may be hiding as a mislabeled point,
# if so, it would likely be a duplicate

chk_dupes <- chk %>%
  # Count number of photos per subject
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, REL_GLOBALID) %>%
  dplyr::summarize(n_photos = n()) %>%
  # Count number of points per subject (disregards multiple photos at one point)
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  dplyr::summarize(n_points = n()) %>%
  filter(n_points > 1) %>%
  filter(Subject1 != "Other")

# join with original data to check created date, etc.
dupes <- chk_dupes %>%
  dplyr::left_join(chk) %>%
  select(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, n_points, Staff_list, created_date, last_edited_date, last_edited_user)

# EIPS photo Check ------------------------------------------------------------

# Load EIPS data
EIPS_chk <- process_photos(AGOL_Layer = "EIPS",
                           gdb_name = "EIPS_OL_ER_20220502.gdb",
                           gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                           gdb_layer = "EIPS_OL_ER_20220502",
                           return_table = TRUE)

# Look for missing points
EIPS_missing <- EIPS_chk %>%
  # remove subjects that are not photo points
  filter(!Subject_EIPS == "Staff" & !Subject_EIPS == "Other") %>%
  tidyr::separate(Subject_EIPS, sep = "_", into = c("distance", "direction"), remove = FALSE) %>%
  dplyr::group_by(Sampling_Frame, Site_numb, Site_Type, distance) %>%
  summarise(n_direct = n_distinct(direction)) %>%
  filter(n_direct != 3 & Site_Type == "Fixed" |
           n_direct != 2 & Site_Type == "Rotational" )

EIPS_chk_dupes <- EIPS_chk %>%
  # Count number of photos per subject
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, REL_GLOBALID) %>%
  dplyr::summarize(n_photos = n()) %>%
  # Count number of points per subject (disregards multiple photos at one point)
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  dplyr::summarize(n_points = n()) %>%
  filter(n_points > 1)

# join with original data to check created date, etc.
EIPS_dupes <- EIPS_chk_dupes %>%
  dplyr::left_join(EIPS_chk) %>%
  select(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, n_points, Staff_list, created_date, last_edited_date, last_edited_user)



# Code to Rotate an upsidedown image ------------------------------------------

upsidedown <- chk %>%
  filter(Samp_Frame == "OL",
         Site_numb == "EIPS 05",
         Subject_EIPS == "600m_Post")

# apply() function doesn't like blobs so change to list before running apply()
upsidedown$DATA <- as.list(upsidedown$DATA)
# Load photo
library(magick)
image_r <- image_read(upsidedown$DATA[[1]])
image_r
image_r <- image_flip(image_r)
upsidedown$DATA[[1]] <- image_write(image_r)
image_r <- image_read(upsidedown$DATA[[1]])
image_r
# applyr the "watermark" function to each record (ie photo)
apply(X = upsidedown, MARGIN = 1, FUN = watermark, new_folder = "upsidedown")

# Process images --------------------------------------------------------------

# example of function parameters to process photos

#FTPC_2022_KAHO_20240328
#FTPC_2022_KAHO_PA11_20240328
#EIPS_2022_HAVO
#FTPC_2022_HAVO
#FTPC_2021_HAVO_ER14_OL01reread
#EIPS_2021_HAVO_ER_TR3

# Get spreadsheets
FTPC_2022_HAVO <- process_photos(AGOL_Layer = "FTPC",
               gdb_name = "Process_Photos.gdb",
               gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/ArcGIS/Projects/Process_Photos",
               gdb_layer = "FTPC_2022_HAVO",
               return_last_table = TRUE)

FTPC_2022_HAVO_ML <- FTPC_2022_HAVO %>%
  dplyr::filter(Samp_Frame == "ML")
length(unique(FTPC_2022_HAVO_ML$Folder_Name))
readr::write_csv(FTPC_2022_HAVO_ML, "watermarked/FTPC_2022_HAVO_ML_20240328.csv")

FTPC_2022_HAVO_KU <- FTPC_2022_HAVO |>
  dplyr::filter(Samp_Frame == "KU") |>
  dplyr::filter(!is.na(Site_Number))
length(unique(FTPC_2022_HAVO_KU$Folder_Name))
readr::write_csv(FTPC_2022_HAVO_KU, "watermarked/FTPC_2022_HAVO_KU_20240328.csv")

EIPS_2022_HAVO <- process_photos(AGOL_Layer = "EIPS",
                                 gdb_name = "Process_Photos.gdb",
                                 gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/ArcGIS/Projects/Process_Photos",
                                 gdb_layer = "EIPS_2022_HAVO",
                                 return_last_table = TRUE)

EIPS_2022_HAVO_ML <- EIPS_2022_HAVO %>%
  dplyr::filter(Samp_Frame == "ML")
length(unique(EIPS_2022_HAVO_ML$Folder_Name))
readr::write_csv(EIPS_2022_HAVO_ML, "watermarked/EIPS_2022_HAVO_ML_20240328.csv")

EIPS_2022_HAVO_KU <- EIPS_2022_HAVO |>
  dplyr::filter(Samp_Frame == "KU") |>
  dplyr::filter(!is.na(Site_Number))
length(unique(EIPS_2022_HAVO_KU$Folder_Name))
readr::write_csv(EIPS_2022_HAVO_KU, "watermarked/EIPS_2022_HAVO_KU_20240328.csv")

unique(EIPS_2022_HAVO_KU$Folder_Name)


# Fix specific photos (plots) --------------------------------------------------

library(pacnvegetation)

KAHO <- process_photos(AGOL_Layer = "FTPC",
               gdb_name = "Process_Photos.gdb",
               gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/ArcGIS/Projects/Process_Photos",
               gdb_layer = "FTPC_2022_KAHO_20240328",
               #test_n_rows = 30,
               #add_watermark = TRUE
               return_last_table = TRUE
               )

# Example Problem - 'Subject_other' comments need to be used as site numbers for KAHO rotationals outside the park proper

# Get first table
KAHO_first_table <- process_photos(AGOL_Layer = "FTPC",
                           gdb_name = "Process_Photos.gdb",
                           gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/ArcGIS/Projects/Process_Photos",
                           gdb_layer = "FTPC_2022_KAHO_20240328",
                           return_first_table = TRUE
)

# Make edits
KAHO_first_table_edits <- KAHO_first_table |>
  dplyr::filter(Site_numb == "FTPC Other") |>
  dplyr::mutate(Site_numb = Subject_other)

# Insert first table and check to make sure all good by checking last table
look <- process_photos(AGOL_Layer = "FTPC",
                       insert_first_table = KAHO_first_table_edits,
                       return_last_table = TRUE)

# Insert first table and run
process_photos(
  AGOL_Layer = "FTPC",
  insert_first_table = KAHO_first_table_edits,
  add_watermark = TRUE
)

# Create table for paths
KAHO_all <- KAHO |>
  dplyr::filter(Site_numb != "FTPC Other") |>
  dplyr::rows_append(look)

readr::write_csv(KAHO_all, "watermarked/FTPC_2022_KAHO_20240328.csv")





# Fix specific photos (transects) OLD ------------------------------------------

update_photos <- process_photos(AGOL_Layer = "EIPS",
                                gdb_name = "EIPS_OL_ER_20220502.gdb",
                                gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                                gdb_layer = "EIPS_OL_ER_20220502",
                                return_table = TRUE)

update_photos <- update_photos %>%
  mutate(samp_plot = paste(Samp_Frame,Site_Number, sep = "_"))

plots2update <- c("OL_2",
                  "OL_3",
                  "OL_4",
                  "OL_35")

#plots2update <- "OL_47"

update_photos2 <- update_photos %>%
  filter(samp_plot %in% plots2update) %>%
  mutate(DATA = as.list(DATA))

update_photos2$Site_numb

apply(X = update_photos2, MARGIN = 1, FUN = watermark, new_folder = "watermark_EIPS__20220922")
