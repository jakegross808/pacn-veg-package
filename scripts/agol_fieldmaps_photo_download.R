# Load libraries:
library(pacnvegetation)
#LoadPACNVeg(force_refresh = FALSE, eips_paths = "foo")
#library(tidyverse)
# Field Maps ----

# Set download folder location (local workspace): ----
# Can be any folder on local machine
var_temp_dest <- "C:/Users/JJGross/Downloads/Check_Photos_KALA_20250916"




## AGOL urls: ------------------------------------------------------------------

# Provide URLs to the AGOL rest service that is being downloaded:

# Type in new AGOL layer rest services here:
#FTPC_KAHO_2022_PA11 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KAHO_2022_FTPC_Sampling_Points_PA11/FeatureServer/43"
#FTPC_KAHO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KAHO_2022_FTPC_Sampling_Points_Photos/FeatureServer/30"

#var_photo_layers <- c("FTPC_KAHO_2022_PA11")
#var_sharepoint_dest <- "https://doimspp.sharepoint.com/:f:/r/sites/nps-PWR-PACNIM/vital_signs/05_focal_terr_plant_communities/Images/2022/KAHO/Originals/FTPC"
#master_spreadsheet_folder <- "C:/Users/JJGross/OneDrive - DOI/2024/KALA/Originals/FTPC/master_spreadsheet"


# KALA EIPS
#EIPS_KALA_2024 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KALA_2024_EIPS_Sampling_Points_Photos/FeatureServer/29"
#var_photo_layers <- c("EIPS_KALA_2024")

# KALA FTPC
FTPC_KALA_2024 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KALA_2024_FTPC_Sampling_Points_Photos/FeatureServer/3"
var_photo_layers <- c("FTPC_KALA_2024")
var_sharepoint_dest <- "https://doimspp.sharepoint.com/:f:/r/sites/nps-PWR-PACNIM/vital_signs/05_focal_terr_plant_communities/Images/2024/KALA/Originals/FTPC"
master_spreadsheet_folder <- "C:/Users/JJGross/OneDrive - DOI/2024/KALA/Originals/FTPC/master_spreadsheet"
#master_spreadsheet_folder <-"C:/Users/JJGross/Downloads/Check_Photos/test_master"

# KALA Plants
Plants_KALA_2024v2 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KALA_2024_VEG_Sampling_Plant_Photos_v2/FeatureServer/2"
var_photo_layers <- c("Plants_KALA_2024v2")
var_sharepoint_dest <- "https://doimspp.sharepoint.com/:f:/r/sites/nps-PWR-PACNIM/vital_signs/05_focal_terr_plant_communities/Images/2024/KALA/Plants/Plants_photos"
master_spreadsheet_folder <- "C:/Users/JJGross/OneDrive - DOI/2024/KALA/Plants/Plants_KALA_2024_Spreadsheet"

## First Download -----------------------------------------------------------

# Test Run
Plants_KALA_2024v2 <- download_agol(
  photo_layers = var_photo_layers,
  temp_dest = var_temp_dest,
  sharepoint_dest = var_sharepoint_dest,
  #only_staff = FALSE,
  test_run = TRUE
)

# First Full Download
field_maps_data <- download_agol(
  photo_layers = var_photo_layers,
  temp_dest = var_temp_dest,
  sharepoint_dest = var_sharepoint_dest,
  test_run = FALSE
)

## 2nd-n Downloads -----------------------------------------------------------

#this needs additional testing with KALA plants, when auto_date_filter = TRUE I'm getting error: Error in `dplyr::bind_rows()`:
#! Can't combine `..1$ID_1_relate` <character> and `..2$ID_1_relate` <logical>.
# I think it is not returning records from AGOL when auto_date_filter = TRUE
# Oddly it seems to work fine with FTPC

# 2nd Download Test Run
Plants_KALA_2024v2_run <- download_agol(
  photo_layers = var_photo_layers,
  temp_dest = var_temp_dest,
  sharepoint_dest = var_sharepoint_dest,
  master_spreadsheet_folder = master_spreadsheet_folder,
  after_date_filter = lubridate::mdy_hms("05/07/2025 18:00:00", tz = "HST"),
  #auto_date_filter = FALSE,
  test_run = TRUE
)
readr::write_csv(Plants_KALA_2024v2_run, "C:/Users/JJGross/Downloads/Plants_KALA_2024v2_run.csv")

# 2nd Download new photos
field_maps_data <- download_agol(
  photo_layers = var_photo_layers,
  temp_dest = var_temp_dest,
  sharepoint_dest = var_sharepoint_dest,
  master_spreadsheet_folder = master_spreadsheet_folder,
  #auto_date_filter = TRUE,
  after_date_filter = lubridate::mdy_hms("05/07/2025 18:00:00", tz = "HST"),
  test_run = FALSE
)
#2025-04-24 06:47:32

# Staff photos
field_maps_data <- download_agol(
  photo_layers = var_photo_layers,
  temp_dest = var_temp_dest,
  sharepoint_dest = var_sharepoint_dest,
  test_run = FALSE,
  only_staff = TRUE
)
