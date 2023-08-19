# Startup script to get going on pacnvegetation package.

library(pacnvegetation)
#library(tidyverse)
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
            force_refresh = TRUE)

# ..........Write Data ----
path_file_info <- file.info(list.files(pacnveg_cache_path, full.names = T))
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

pacnveg_master_spp_list <- read_spp_db(veg_species_db)

pacnveg_master_spp_list <- pacnveg_master_spp_list %>%
  dplyr::arrange(Taxonomic_Family, Genus, Species)

readr::write_csv(pacnveg_master_spp_list, paste0("C:/Users/JJGross/Downloads/pacnveg_master_spp_list_", Sys.Date(), ".csv"))

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
#FTPC_HAVO_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/FTPC_Points_Photos_HAVO_2021/FeatureServer/1"
#FTPC_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_FTPC_Sampling_Points_Photos/FeatureServer/30"
#FTPC_KAHO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KAHO_2022_FTPC_Sampling_Points_Photos/FeatureServer/30"
FTPC_HALE_2023 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_FTPC_Sampling_Points_Photos/FeatureServer/89"

#EIPS_HAVO_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/EIPS_Points_Photos_HAVO_2021/FeatureServer/1"
#EIPS_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_EIPS_Sampling_Points_Photos/FeatureServer/32"
EIPS_HALE_2023 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_EIPS_Sampling_Points_Photos/FeatureServer/93"

#Plants_HAVO_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_Vegetation_Sampling_Plant_Photos_HAVO_2021/FeatureServer/1"
#Plants_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_VEG_Sampling_Plant_Photos/FeatureServer/31"
#Plants_KAHO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KAHO_2022_VEG_Sampling_Plant_Photos/FeatureServer/41"
Plants_HALE_2023v1 <-"https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_VEG_Sampling_Plant_Photos/FeatureServer/91"
Plants_HALE_2023v2 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_VEG_Sampling_Plant_Photos_v2/FeatureServer/91"

# Get Data For All Layers
all_photos_layers <- c("FTPC_HAVO_2021", "FTPC_HAVO_2022", "FTPC_KAHO_2022", "FTPC_HALE_2023",
                       "EIPS_HAVO_2021", "EIPS_HAVO_2022", "EIPS_HALE_2023",
                       "Plants_HAVO_2021", "Plants_HAVO_2022", "Plants_KAHO_2022", "Plants_HALE_2023v1", "Plants_HALE_2023v2")

subset_photos_layers <- c("EIPS_HALE_2023", "Plants_HALE_2023v2")

temp_dest <- "C:/Users/JJGross/Downloads/"

download_agol(subset_photos_layers, temp_dest)










