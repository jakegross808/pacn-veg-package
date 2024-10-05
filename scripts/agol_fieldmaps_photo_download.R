library(pacnvegetation)

# Field Maps ----


## AGOL urls: --------------------------------------------------------------------

#FTPC_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_FTPC_Sampling_Points_Photos/FeatureServer/30"
Plants_KALA_2024v2 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KALA_2024_VEG_Sampling_Plant_Photos_v2/FeatureServer/2"
var_photo_layers <- c("Plants_KALA_2024v2")

FTPC_KALA_2024 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KALA_2024_FTPC_Sampling_Points_Photos/FeatureServer/3"
var_photo_layers <- c("FTPC_KALA_2024")

EIPS_KALA_2024 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KALA_2024_EIPS_Sampling_Points_Photos/FeatureServer/29"
var_photo_layers <- c("EIPS_KALA_2024")

var_temp_dest <- "C:/Users/JJGross/Downloads/Check_Photos"
#var_sharepoint_dest <- "https://doimspp.sharepoint.com/:f:/r/sites/nps-PWR-PACNIM/vital_signs/05_focal_terr_plant_communities/Images/2024/KALA/Plants/Plants_photos"
var_sharepoint_dest <- "https://doimspp.sharepoint.com/:f:/r/sites/nps-PWR-PACNIM/vital_signs/05_focal_terr_plant_communities/Images/2024/KALA/Originals/FTPC"



## First Download -----------------------------------------------------------

# Test Run
field_maps_data <- download_agol(
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

# 2nd Download Test Run
field_maps_data <- download_agol(
  photo_layers = var_photo_layers,
  temp_dest = var_temp_dest,
  sharepoint_dest = var_sharepoint_dest,
  master_spreadsheet_folder = "C:/Users/JJGross/OneDrive - DOI/2024/KALA/Originals/FTPC/master_spreadsheet",
  test_run = TRUE
)

# 2nd Download new photos
field_maps_data <- download_agol(
  photo_layers = var_photo_layers,
  temp_dest = var_temp_dest,
  sharepoint_dest = var_sharepoint_dest,
  master_spreadsheet_folder = "C:/Users/JJGross/OneDrive - DOI/2024/KALA/Originals/FTPC/master_spreadsheet",
  #master_spreadsheet_folder = "C:/Users/JJGross/Downloads/master",
  #after_date_filter = lubridate::mdy_hms("09/12/2024 18:00:00", tz = "HST"),
  test_run = FALSE
)
