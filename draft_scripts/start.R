# Startup script to get going on pacnvegetation package.
#install.packages("devtools")
#devtools::install_github("jakegross808/pacn-veg-package")

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

names(FilterPACNVeg())

select_dataset <- FilterPACNVeg(data_name = "Enter Dataset Name Here")

Events_FTPC <- FilterPACNVeg(data_name = "Events_extra_xy")
Events_EIPS <- FilterPACNVeg(data_name = "Events_extra_other_EIPS")
Species_FTPC <- FilterPACNVeg(data_name = "Presence") #%>%
  select(Scientific_Name) %>%
  distinct()



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

WAPA_vegmap_db_paths <- c("C:/Users/JJGross/Documents/Veg_Map_Data/wapadata.mdb")


# Veg map & species locations ----

Hawaii_vegmap_data2 <- read_vegmap_db(hi_vegmap_db_paths)

WAPA_vegmap_data <- read_vegmap_db(WAPA_vegmap_db_paths)

leaflet::addCircleMarkers()

leaflet::leaflet(data = WAPA_vegmap_data) %>%
  leaflet::addTiles() %>%
  leaflet::addMarkers(~long, ~lat, popup = ~as.character(Plot_Code), label = ~as.character(Sci_Name))



hi_poaceae_vegmap <- Hawaii_vegmap_data %>%
  filter(!is.na(lat), !is.na(long)) %>%
  filter(Family == "Poaceae")

names(FilterPACNVeg())

look <- FilterPACNVeg(data_name = "Understory")

read_vegmap_db()

# Most recent query for Kevin:

hi_vegmap_data <- pacnvegetation::read_vegmap_db(hi_vegmap_db_paths)

selected_spp <- c(
  "Tibouchina herbacea","Leptospermum scoparium","Toona ciliata",
  "Pinus luchuensis","Pinus spp.", "Angiopteris evecta","Sphaeropteris cooperi = (Cyathea cooperi)",
  "Pterolepis glomerata","Elephantopus mollis","Melochia umbellata",
  "Spathodea campanulata","Rhodomyrtus tomentosa",
  "Erigeron karvinskianus","Hedychium gardnerianum","Oxyspora paniculata",
  "Alstonia macrophylla")

# Not_found <- "Leptospermum scoparium" ,"Toona ciliata", "Pinus luchuensis",
# "Angiopteris evecta", "Pterolepis glomerata", "Melochia umbellata", "Rhodomyrtus tomentosa",
# "Oxyspora paniculata", "Alstonia macrophylla"

# 93 species occurrences from veg map:
hi_vegmap_spp <- hi_vegmap_data %>%
  filter(!is.na(lat), !is.na(long)) %>%
  filter(Sci_Name %in% c(selected_spp))




# ..........Veg Species Database ----


# Local Path to Veg Spp database
veg_species_db_folder <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
# If only one database in folder, this will grab full path:
veg_species_db_full_path <- list.files(veg_species_db_folder,full.names = TRUE)
veg_species_db_full_path
# Get raw data from Veg Species Database:
raw_spp_data <- read_spp_db(veg_species_db_full_path)

# Get master species list for a park (with ID_Field for field maps):
spp_list_master <- master_spp_list(veg_species_db_full_path, park = 'All')

spp_list_KALA <- master_spp_list(veg_species_db_full_path, park = "KALA")
readr::write_excel_csv(spp_list_KALA, paste0("C:/Users/JJGross/Downloads/spp_list_KALA_", Sys.Date(), ".csv"))

spp_list_KALA_coast <- master_spp_list(veg_species_db_full_path, park = "KALA", sample_frame = c("Hoolehua", "Kalawao"))

spp_list_KALA_coast_EIPS <- spp_list_KALA_coast %>%
  filter(Nativeness != "Native")

readr::write_excel_csv(spp_list_KALA_coast_EIPS, paste0("C:/Users/JJGross/Downloads/spp_list_KALA_coast_EIPS_", Sys.Date(), ".csv"))



# This is pain, not going to use it:
spp_list_KALA_SF <- spp_list_KALA %>%
  separate_wider_delim(FTPC_pres, "(", names = c(NA, "SF")) %>%
  mutate(SF = str_sub(SF, end = -2)) %>%
  separate_wider_delim(SF, ",", names = c("HO", "KW", "PA")) %>%
  mutate(HO = as.numeric(str_sub(HO, 4, -1))) %>%
  mutate(KW = as.numeric(str_sub(KW, 5, -1))) %>%
  mutate(PA = as.numeric(str_sub(PA, 5, -1)))

readr::write_excel_csv(spp_list_KALA_SF, paste0("C:/Users/JJGross/Downloads/spp_list_KALA_SF", Sys.Date(), ".csv"))

drop_these_rows <- spp_list_KALA_remove_puu_only %>%
  filter(HO == 0, KW == 0, PA > 0)

spp_list_KALA_remove_puu_only <- spp_list_KALA_SF %>%
  filter(!Scientific_name %in% drop_these_rows)





# Write master species list
readr::write_csv(raw_spp_data, paste0("C:/Users/JJGross/Downloads/raw_spp_data", Sys.Date(), ".csv"))



ECHCOL_list <- master_spp_list(veg_species_db_full_path, park = "HAVO") |>
  filter(Scientific_name == "Echinochloa colona")

ECHCOL <- FilterPACNVeg(data_name = "Presence") |>
  filter(Unit_Code == "HAVO") |>
  filter(Scientific_Name == "Echinochloa colona")

ECHCOL_und <- FilterPACNVeg(data_name = "Understory") |>
  filter(Unit_Code == "HAVO") |>
  filter(Scientific_Name == "Echinochloa colona")

# ..........AGOL Data ----

# AGOL layer service urls
#FTPC_HAVO_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/FTPC_Points_Photos_HAVO_2021/FeatureServer/1"
#FTPC_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_FTPC_Sampling_Points_Photos/FeatureServer/30"
FTPC_KAHO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KAHO_2022_FTPC_Sampling_Points_Photos/FeatureServer/30"
FTPC_KAHO_2022pa11 <-"https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KAHO_2022_FTPC_Sampling_Points_PA11/FeatureServer/43"
FTPC_HALE_2023 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_FTPC_Sampling_Points_Photos/FeatureServer/89"

#EIPS_HAVO_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/EIPS_Points_Photos_HAVO_2021/FeatureServer/1"
#EIPS_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_EIPS_Sampling_Points_Photos/FeatureServer/32"
EIPS_HALE_2023 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_EIPS_Sampling_Points_Photos/FeatureServer/93"

#Plants_HAVO_2021 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_Vegetation_Sampling_Plant_Photos_HAVO_2021/FeatureServer/1"
#Plants_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_VEG_Sampling_Plant_Photos/FeatureServer/31"
Plants_KAHO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KAHO_2022_VEG_Sampling_Plant_Photos/FeatureServer/41"
Plants_HALE_2023v1 <-"https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_VEG_Sampling_Plant_Photos/FeatureServer/91"
Plants_HALE_2023v2 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_VEG_Sampling_Plant_Photos_v2/FeatureServer/91"

# Get Data For All Layers
all_photos_layers <- c("FTPC_HAVO_2021", "FTPC_HAVO_2022", "FTPC_KAHO_2022", "FTPC_HALE_2023",
                       "EIPS_HAVO_2021", "EIPS_HAVO_2022", "EIPS_HALE_2023",
                       "Plants_HAVO_2021", "Plants_HAVO_2022", "Plants_KAHO_2022", "Plants_HALE_2023v1", "Plants_HALE_2023v2")

subset_photos_layers1 <- c("FTPC_KAHO_2022")
subset_photos_layers2 <- c("FTPC_KAHO_2022pa11")

# Test download_agol2
look1 <- download_agol2(photo_layers = subset_photos_layers1,
               temp_dest = "C:/Users/JJGross/Downloads/KAHO_FTPC/",
               test_run = TRUE)

download_agol2(photo_layers = subset_photos_layers1,
               temp_dest = "C:/Users/JJGross/Downloads/KAHO_FTPC/")

look2 <- download_agol2(photo_layers = subset_photos_layers2,
               temp_dest = "C:/Users/JJGross/Downloads/KAHO_FTPC/",
               test_run = TRUE)

download_agol2(photo_layers = subset_photos_layers2,
               temp_dest = "C:/Users/JJGross/Downloads/KAHO_FTPC/")



# ..........Local Geodatabase Data ----
gdb <- "C:/Users/JJGross/Downloads/ED_HAVO_2022_new/ED_HAVO_2022_new.gdb"
gdb_layer <- "ED_HAVO_2022"
ED_HAVO_2022 <- sf::read_sf(gdb, gdb_layer)

gdb <- "C:/Users/JJGross/Downloads/KAHO_2022_FTPC_Sampling_Points_Photos/KAHO_2022_FTPC_Sampling_Points_Photos.gdb"
sf::st_layers(dsn = gdb)

look <- process_photos(AGOL_Layer = "FTPC",
               gdb_name = "KAHO_2022_FTPC_Sampling_Points_Photos.gdb",
               gdb_location = "C:/Users/JJGross/Downloads/KAHO_2022_FTPC_Sampling_Points_Photos",
               gdb_layer = "KAHO_2022_FTPC_Sampling_Points_Photos",
               return_table = TRUE)

gdb_layer <- "ED_HAVO_2022"
ED_HAVO_2022 <- sf::read_sf(gdb, gdb_layer)







