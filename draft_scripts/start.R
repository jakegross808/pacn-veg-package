# Startup scripts for using pacnvegetation.

library(pacnvegetation)
library(tidyverse)
#library(magrittr)
# if need to install packages while on network:
#options(download.file.method = "wininet")

#--- 1. Read latest cache ----

# Write/Read csv from pacnvegetation package:
pacnveg_cache_path <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/R_WritePACNVeg"

# Read
path_file_info <- file.info(list.files(pacnveg_cache_path, full.names = T))
latest_folder <- rownames(path_file_info)[which.max(path_file_info$mtime)]

LoadPACNVeg(data_path = latest_folder,
            data_source = "file")

#--- 2. Update and Write Data ----

# ..........Update Data ----
eips_database_folder_path <- "C:/Users/JJGross/Documents/Databases_copied_local/EIPS"
eips_databases <- list.files(eips_database_folder_path,full.names = TRUE)
eips_databases

LoadPACNVeg(ftpc_params = "pacnveg",
            eips_paths = eips_databases,
            cache = TRUE,
            expire_interval_days = 30,
            force_refresh = FALSE)

# ..........Write Data ----
write_folder <- paste0(pacnveg_cache_path, "/", Sys.Date())
write_folder
WritePACNVeg(dest.folder = write_folder, create.folders = TRUE)

#--- 3. Optional Loads ----

# ..........Veg Map Data ----
hi_vegmap_db_paths <- c("C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/havodata.accdb",
                     "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/haledata.accdb",
                     "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/kahodata.mdb",
                     "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/kaladata.mdb",
                     "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/puhedata.mdb",
                     "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/puhodata.mdb")

hi_vegmap_data <- pacnvegetation::read_vegmap_db(hi_vegmap_db_paths)

hi_vegmap_grass_example <- hi_vegmap_data %>%
  filter(!is.na(lat), !is.na(long)) %>%
  filter(Family == "Poaceae")

# ..........Veg Species Database ----
veg_species_db_path <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
veg_species_db <- list.files(veg_species_db_path,full.names = TRUE)
veg_species_db

pacnveg_master_spp_list <- read_vegspp_db(veg_species_db)

# Veg map & species locations ----

Hawaii_vegmap_data <- read_vegmap_db(vegmap_db_paths)

hi_poaceae_vegmap <- Hawaii_vegmap_data %>%
  filter(!is.na(lat), !is.na(long)) %>%
  filter(Family == "Poaceae")

names(FilterPACNVeg())

look <- FilterPACNVeg(data_name = "Understory")

read_vegmap_db()

# ..........AGOL Data ----

# AGOL layer service urls
FTPC_HAVO_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/FTPC_Points_Photos_HAVO_2021/FeatureServer/1"
FTPC_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_FTPC_Sampling_Points_Photos/FeatureServer/30"
FTPC_KAHO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KAHO_2022_FTPC_Sampling_Points_Photos/FeatureServer/30"
FTPC_HALE_2023 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_FTPC_Sampling_Points_Photos/FeatureServer/89"

EIPS_HAVO_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/EIPS_Points_Photos_HAVO_2021/FeatureServer/1"
EIPS_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_EIPS_Sampling_Points_Photos/FeatureServer/32"
EIPS_HALE_2023 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_EIPS_Sampling_Points_Photos/FeatureServer/93"

Plants_HAVO_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_Vegetation_Sampling_Plant_Photos_HAVO_2021/FeatureServer/1"
Plants_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_VEG_Sampling_Plant_Photos/FeatureServer/31"
Plants_KAHO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KAHO_2022_VEG_Sampling_Plant_Photos/FeatureServer/41"
Plants_HALE_2023v1 <-"https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_VEG_Sampling_Plant_Photos/FeatureServer/91"
Plants_HALE_2023v2 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_VEG_Sampling_Plant_Photos_v2/FeatureServer/91"

# Get Data For All Layers
all_photos_layers <- c("FTPC_HAVO_2021", "FTPC_HAVO_2022", "FTPC_KAHO_2022", "FTPC_HALE_2023",
                       "EIPS_HAVO_2021", "EIPS_HAVO_2022", "EIPS_HALE_2023",
                       "Plants_HAVO_2021", "Plants_HAVO_2022", "Plants_KAHO_2022", "Plants_HALE_2023v1", "Plants_HALE_2023v2")

subset_photos_layers <- c("Plants_HALE_2023v2")

temp_dest <- "C:/Users/JJGross/Downloads/"

for (layer in subset_photos_layers){
  print(paste("starting download for", layer))

  file_date <- gsub("-", "", as.character(Sys.Date()))

  layer_dest <- paste0(temp_dest, as.character(layer), "_", file_date)

  layer_df <- DownloadAGOLAttachments(
    feature_layer_url = get(layer),
    custom_name = TRUE,
    append_id = FALSE,
    agol_username = "pacn_gis",
    agol_password = keyring::key_get(service = "AGOL", username = "pacn_gis"),
    test_run = FALSE,
    dest_folder = layer_dest)

  csv_location <- paste0(layer_dest, "/", as.character(layer), "_", file_date, ".csv")

  readr::write_csv(layer_df, csv_location)
  #assign(paste0("look_", as.character(layer)), x)


  print(layer_dest)
  print(csv_location)
  print(paste(layer, "complete"))

}

temp_dest <- "C:/Users/JJGross/Downloads/HALE_Plants_test"

# Get Specific Layer
x <- DownloadAGOLAttachments(
  feature_layer_url = EIPS_HALE_2023,
  custom_name = TRUE,
  append_id = FALSE,
  agol_username = "pacn_gis",
  agol_password = keyring::key_get(service = "AGOL", username = "pacn_gis"),
  test_run = TRUE,
  dest_folder = "Plants_HALE_2023")

readr::write_csv(x,  "C:/Users/JJGross/Downloads/EIPS_HALE_test/test_csv.csv")
