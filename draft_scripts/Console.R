# Packages:

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
            force_refresh = FALSE)

# ..........Write Data ----
write_folder <- paste0(pacnveg_cache_path, "/", Sys.Date())
write_folder
WritePACNVeg(dest.folder = write_folder, create.folders = TRUE)

#--- 3. Optional Loads ----

# ..........Veg Map Data ----
vegmap_db_paths <- c("C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/havodata.accdb",
                     "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/haledata.accdb",
                     "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/kahodata.mdb",
                     "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/kaladata.mdb",
                     "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/puhedata.mdb",
                     "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/puhodata.mdb")

# ..........Veg Species Database ----
veg_species_db_path <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
veg_species_db <- list.files(veg_species_db_path,full.names = TRUE)
veg_species_db


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

# ---Experimenting with Download AGOL Attachments ----

a <- c("20230814_143023", "20230814_143023")
b <- c("Bob", "Schtu")
df <- data.frame(a,b)
str(df)
#df$x <- substr(df$x, 1, nchar(x)-2)

df %>%
  dplyr::mutate(a = stringr::str_sub(a, end = -3))




test_FTPC_HAVO_att <- DownloadAGOLAttachments(
  feature_layer_url = FTPC_HAVO_2022,
  custom_name = TRUE,
  append_id = FALSE,
  agol_username = "pacn_gis",
  agol_password = keyring::key_get(service = "AGOL", username = "pacn_gis"),
  test_run = TRUE,
  dest_folder = "C:/Users/JJGross/Downloads/EIPS_HALE_test"
)

test_EIPS_HALE_att <- DownloadAGOLAttachments(
  feature_layer_url = EIPS_HALE_2023,
  custom_name = TRUE,
  append_id = TRUE,
  agol_username = "pacn_gis",
  agol_password = keyring::key_get(service = "AGOL", username = "pacn_gis"),
  test_run = TRUE,
  dest_folder = "C:/Users/JJGross/Downloads/EIPS_HALE_test"
)





test_FTPC_HALE_att <- DownloadAGOLAttachments(
  feature_layer_url = FTPC_HALE_2023,
  agol_username = "pacn_gis",
  agol_password = keyring::key_get(service = "AGOL", username = "pacn_gis"),
  test_run = TRUE,
  dest_folder = "C:/Users/JJGross/Downloads/EIPS_HALE_test"
)

test_Plants_HALE_att <- DownloadAGOLAttachments(
  feature_layer_url = Plants_HALE_2023, #FTPC_HALE_2023, #EIPS_HALE_2023,
  agol_username = "pacn_gis",
  agol_password = keyring::key_get(service = "AGOL", username = "pacn_gis"),
  test_run = TRUE,
  dest_folder = "C:/Users/JJGross/Downloads/EIPS_HALE_test",
  custom_name = TRUE,
  prefix = paste(photo_day_time, "EI", Samp_Frame, Site_numb, Subject_FTPC, sep = "_"),
  sep = "_",
  append_id = FALSE
  )

test_x <- test_Plants_HALE_att
test_x <- test_FTPC_HALE_att
test_x <- test_EIPS_HALE_att


# utilize date column from point created in field maps
test_x <- test_x %>%
  dplyr::mutate(pt_date_new = stringr::str_remove_all(string = pt_date,
                                                       pattern = "[-:]")) %>%
  dplyr::mutate(pt_date_new = stringr::str_replace(string = pt_date_new,
                                                   pattern = " ",
                                                   replacement = "_"))

# Make Subject column compatible for all layers

# Change Subject_FTPC and Subject_EIPS to just "Subject"
lookup <- c(Subject = "Subject_FTPC", Subject = "Subject_EIPS")
test_x <- dplyr::rename(test_x, tidyselect::any_of(lookup))

# If "Plant" layer use Code from Photo_Taxon as "Subject"
if (any(names(test_x) %in% c("Photo_Taxon"))) {
  test_x$Subject <- stringr::str_extract(test_x$Photo_Taxon, "(?<=\\().*?(?=\\))")
}

# remove all special characters from Subject_other & append to any Subject == other
if (any(names(test_x) %in% c("Subject_other"))) {
  test_x <- test_x %>%
    dplyr::mutate(Sub_other = stringr::str_remove(Subject_other, "[[:punct:]]")) %>%
    dplyr::mutate(Sub_other = stringr::str_replace_all(Sub_other," ", "_")) %>%
    dplyr::mutate(Subject = dplyr::case_when(Subject == "Other" ~ paste(Subject, Sub_other, sep = "_"),
                                             .default = as.character(Subject)))
}

# utilize numeric count of photos from Field Maps
test_x$photo_cnt <- stringr::str_extract(string = test_x$name, pattern = "(\\d)+")

# utilize date column from point created in field maps
test_x <- test_x %>%
  dplyr::mutate(pt_date = as.character(as.POSIXct(created_date/1000,
                                                  origin="1970-01-01"))) %>%
  tidyr::separate_wider_delim(pt_date, " ",
                              names = c("pt_date_file", NA),
                              cols_remove = FALSE) %>%
  dplyr::mutate(pt_date_file = stringr::str_remove_all(pt_date_file,
                                                       pattern = "-")) %>%
  tidyr::separate_wider_delim(Site_numb, " ",
                              names = c("protocol", "site_typenum"),
                              cols_remove = FALSE,
                              too_many = "merge")

# Create new name utilizing columns above
test_x <- test_x %>%
  dplyr::mutate(new_name = paste(pt_date_file, site_typenum, Subject, photo_cnt, sep = "_"))






test_x %>%
  dplyr::mutate(Sub_other = stringr::str_remove(Subject_other, "[[:punct:]]")) %>%
  dplyr::mutate(Sub_other = stringr::str_replace_all(Sub_other," ", "_"))

stringr::str_remove_all(test_x$Subject_other, "[[:punct:]]") %>%
  stringr::str_replace_all(" ", "_")

lookup <- c(Subject = "Subject_FTPC", Subject = "Subject_EIPS")
test_x <- dplyr::rename(test_x, tidyselect::any_of(lookup))

any(names(test_x) %in% c("Photo_Taxon"))

if (any(names(test_x) %in% c("Photo_Taxon"))) {
  test_x$Subject <- stringr::str_extract(test_x$Photo_Taxon, "(?<=\\().*?(?=\\))")
}








# includes parentheses:
stringr::str_extract(test_x$Photo_Taxon, "\\((.*?)\\)")

# excludes parentheses:
stringr::str_extract(test_x$Photo_Taxon, "(?<=\\().*?(?=\\))")



test_x %>%
  dplyr::select(Photo_Taxon) %>%
  dplyr::pull() %>%
  #stringr::str_extract(
  stringr::str_extract("")


any(names(test_x) %in% c("Subject_FTPC", "Subject_EIPS"))

test_x %>%
  dplyr::select(tidyselect::starts_with("Subject") & !tidyselect::ends_with("other"))

lookup <- c(Subject = "Subject_FTPC", Subject = "Subject_EIPS")
test_x <- dplyr::rename(test_x, tidyselect::any_of(lookup))

dplyr::rename(test_x,
              .cols = tidyselect::starts_with("Subject") & !tidyselect::ends_with("other"),
              ~ "Subject")

any(names(test_FTPC_HALE_att) == "Subject_FTPC" |
      any(names(test_FTPC_HALE_att) == "Subject_EIPS"))

any(names(test_FTPC_HALE_att) %in% c("Subject_FTPC", "Subject_EIPS"))

any(names(test_EIPS_HALE_att) == "Subject_FTPC" |
      any(names(test_EIPS_HALE_att) == "Subject_EIPS"))

any(names(test_Plants_HALE_att) == "Subject_FTPC" |
      any(names(test_Plants_HALE_att) == "Subject_EIPS"))



test_Plants <- test_Plants_HALE_att %>%
  dplyr::mutate(pt_date = as.character(as.POSIXct(created_date/1000,
                                                  origin="1970-01-01"))) %>%
  tidyr::separate_wider_delim(pt_date, " ",
                              names = c("pt_date_file", NA),
                              cols_remove = FALSE) %>%
  dplyr::mutate(pt_date_file = stringr::str_remove_all(pt_date_file,
                                                       pattern = "-")) %>%
  tidyr::separate_wider_delim(Site_numb, " ",
                              names = c("protocol", "site_typenum"),
                              cols_remove = FALSE,
                              too_many = "merge") %>%
  dplyr::mutate(paste_name = paste(pt_date_file, site_typenum, Subject_FTPC, sep = "_"))





test_FTPC <- test_FTPC_HALE_att %>%
    dplyr::mutate(pt_date = as.character(as.POSIXct(created_date/1000, origin="1970-01-01"))) %>%
    tidyr::separate_wider_delim(pt_date, " ", names = c("pt_date_file", NA), cols_remove = FALSE) %>%
    dplyr::mutate(pt_date_file = stringr::str_remove_all(pt_date_file, pattern = "-")) %>%
    tidyr::separate_wider_delim(Site_numb, " ", names = c("protocol", "site_typenum"), cols_remove = FALSE,too_many = ) #%>%
  #dplyr::mutate(paste_name = paste(pt_date_file, "EI", Samp_Frame, Site_numb, Subject_FTPC, sep = "_"))





 #Get Exif Date/Time - problem is some photos don't have this! (weird!)
test <- test_Plants_HALE_att[1,] %>%
  select(exifInfo) %>%
  unnest(cols = c(exifInfo)) %>%
  #filter(name %in% c("Exif IFD0", "Exif SubIFD")) %>%
  mutate(tags = map(tags, ~ mutate(.x, value = as.character(value)))) %>%
  unnest(cols = c(tags), names_sep = "_", keep_empty = TRUE) %>%
  filter(tags_name == "Date/Time")

chk_date1 <- test_Plants_HALE_att[1,]
chk_date <- test_Plants_HALE_att$created_date[1]

chk_date2 <- test_Plants_HALE_att %>%
  filter(Samp_Frame == "HA") #%>%
  pull(created_date)

chk2 <- chk_date2[1]


format(chk_date, scientific = FALSE)

as.POSIXct(chk2/1000, origin="1970-01-01")


format(round(chk_date, 2), nsmall = 2)
date
  mutate(tags = map(tags, mutate(.x, value = as.charcter(value))))
  unnest(cols = c(tags))
test[[2]]$value
x.1Plants_HAVO21[1,][[2]]$value


look <- test %>%
  mutate(tags = map(tags, ~ mutate(.x, value = as.character(value)))) %>%
  unnest(cols = c(tags), names_sep = "_")

#%>%
test
  unlist()
str(test)
test

dat <- c("Clyde" = "1", "Susy" = "2", "Frank" = "3", "John" = "4")
str(dat)

x.3Plants_HAVO21 <- x.1Plants_HAVO21 %>%
  #mutate(data = map(data, ~ .x %>% mutate_all(as.character))) #%>%
  select(exifInfo) %>%
  unnest(cols = c(exifInfo)) %>%
  #map(~filter(.,name=="Date/Time"))
  filter(name == "Exif IFD0",
         name == "Edif SubiFD") %>%
  select(tags) %>%
  unlist() #%>%
  #unnest(cols = c(tags)) #%>%

x.3Plants_HAVO21

  filter(name == "Date/Time") %>%
    select(value) %>%


  #mutate(tags = as.character(tags)) #%>%
  #mutate(across(everything(), as.character)) #%>%
  #mutate(data = map(data,~
  #                    .x %>%
  #                    mutate(across(everything(), as.character)))) %>%
  unnest(cols = c(tags)) #%>%
  filter(name == "Date/Time") %>%
  select(value) %>%
  mutate(value2 = as.character(str_replace_all(value, ":", ""))) %>%
  mutate(photo_day_time = as.character(str_replace_all(value2, " ", "_"))) %>%
  select(photo_day_time) %>%
  bind_cols(x.1Plants_HALE)
look <- x.1Plants_HALE2

x.1 %>%
  select(exifInfo) %>%
  slice(1) %>%
  pull(1) %>%
  pluck(1)

x.1$exifInfo %>% dplyr::slice(1)

%>% purrr::pluck('Exif IFD0') #%>% purrr::map_df(.f = as_tibble)


purrr::pluck(x.1$exifInfo, name=="Exif IFD0")

purrr::pluck(x.1$exifInfo, 2)

x$exifInfo %>%
  purrr::pluc

x$exifInfo[[1]][[2]]

x$exifInfo[][[2]]

str(x$exifInfo)



t <- (x$exifInfo[[1]][[2]])[[1]]$value[8]
t

gsub(" ", "_", gsub(':',"", t))

gsub(" ", "_", gsub(':',"", (x$exifInfo[[2]][[2]])[[1]]$value[8]))

gsub(" ", "_", gsub(':',"", (x$exifInfo[[2]])[[1]]$value[8]))

# ---- HALE Climate chg meeting ------------------------------------------------
pacnvegetation::v_cover_bar_stats(combine_strata = TRUE,
                                  plant_grouping = "Species",
                                  sample_frame = "Haleakala",
                                  paired_change = TRUE,
                                  measurement = "Chg_Prior"
                                  )

zones <- read.csv(file = "R/Events_extra_xy_mgmt.csv") %>%
  select(Sampling_Frame, Plot_Number, Zone) %>%
  dplyr::distinct()

spp_info <- FilterPACNVeg(data = "Species_extra")

All_spp <- FilterPACNVeg(data_name = "Presence") %>%
  dplyr::left_join(zones, by = dplyr::join_by("Sampling_Frame", "Plot_Number")) %>%
  #dplyr::filter(Zone == "NW Kahuku") %>%
  dplyr::left_join(spp_info, by = dplyr::join_by("Code", "Unit_Code" == "Park"))

HALE_spp <- All_spp %>%
  dplyr::group_by(Sampling_Frame, Species) %>%
  filter(Unit_Code == "HALE")

HALE_spp_unique <- HALE_spp %>%
  dplyr::group_by(Sampling_Frame, Nativity.x) %>%
  dplyr::summarize(n = n())

write_csv(NW_Kahuku_spp, "Downloads/test.csv")
look <- read_csv("Downloads/test.csv")
look <- read_csv("Downloads/NW_Kahuku_Species_PACN_Veg_monitoring_plots.csv")

# ----Certification----

install.packages("devtools")
devtools::install_github("matthewjwhittle/getarc")
library(getarc)

paste_layer <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/FTPC_Points_Photos_HAVO_2021/FeatureServer"

# Set Credentials (once)
set_credentials(client_id = "xxxx", client_secret = "xxxx", app_name = "My App")
my_token <- get_token()

## Or:
generate_token(endpoint = paste_layer, username = "NPS/JJGross", password = "test", expiration = 60)

my_token <- get_token(client_id = "xxxx", client_secret = "xxxx", app_name = "My App")

data <-
  query_layer(endpoint = private_endpoint,
              # Pass in token for a secured service
              my_token = my_token
  )

head(getarc::endpoints)
query_layer(endpoint = endpoints$us_fire_occurrence)



test <- arc.data2sf(arc.select(arc.open(paste_prop_layer)))


look <- pacnvegetation::qc_presence_complete(sample_frame = "Mauna Loa")

# ----Get Species in NW Kahuku Zone----

zones <- read.csv(file = "R/Events_extra_xy_mgmt.csv") %>%
  select(Sampling_Frame, Plot_Number, Zone) %>%
  dplyr::distinct()

names(pacnvegetation:::GetColSpec())
install.packages("tidyverse")
dplyr::join_by

pacnvegetation::add_mgmt_unit("All")

zones <- read.csv(file = "R/Events_extra_xy_mgmt.csv") %>%
  select(Sampling_Frame, Plot_Number, Zone) %>%
  dplyr::distinct()

names(pacnvegetation:::GetColSpec())

spp_info <- FilterPACNVeg(data = "Species_extra")

NW_Kahuku_spp <- FilterPACNVeg(data_name = "Presence") %>%
  dplyr::left_join(zones, by = dplyr::join_by("Sampling_Frame", "Plot_Number")) %>%
  dplyr::filter(Zone == "NW Kahuku") %>%
  dplyr::left_join(spp_info, by = dplyr::join_by("Code", "Unit_Code" == "Park"))

write_csv(NW_Kahuku_spp, "Downloads/test.csv")
look <- read_csv("Downloads/test.csv")
look <- read_csv("Downloads/NW_Kahuku_Species_PACN_Veg_monitoring_plots.csv")

#----- Large Trees - Basal Area ------------------------------------------------


BRUGYM <- FilterPACNVeg(data_name = "LgTrees", sample_frame = "Muchot", is_qa_plot = FALSE, sci_name = "Bruguiera gymnorrhiza")

SmWoody <- FilterPACNVeg(data_name = "SmWoody", sample_frame = "Muchot", is_qa_plot = FALSE)

BRUGYM_plt_tot_DBH <- BRUGYM %>%
  mutate(DBH_coalesce = dplyr::coalesce(DBH_Bole, DBH)) %>%
  dplyr::group_by(Sampling_Frame, Year, Plot_Number, Status) %>%
  dplyr::summarise(DBH_sum = sum(DBH_coalesce))



# ----Kipahulu Zone----

MapPACNVeg2(sample_frame = "Kipahulu District", protocol = c("FTPC", "EIPS"))


# ----Get Species For Charlie & Palikea----
v_cover_bar_stats(sample_frame = "Kipahulu District",
                  combine_strata = TRUE,
                  plant_grouping = "Species",
                  paired_change = TRUE,
                  measurement = "Chg_Prior", plot_number = c(2,3,6,7,11,12,15)
                  )

# ----troubleshoot Nahuku/East Rift Rmd----
test_eips_paths = c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20220503.mdb")
test_conn_strings <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", test_eips_paths)
test_conn <- DBI::dbConnect(odbc::odbc(), .connection_string = test_conn_strings)


tbl_Locations <- dplyr::tbl(test_conn, "tbl_Locations") %>%
  dplyr::select(Location_ID, Site_ID, Community = Plant_Community, Sampling_Frame) %>%
  # Remove white space " / " in "Nahuku / East Rift" to match FTPC

  # This replaces all records (does not match pattern)
  #dplyr::mutate(Sampling_Frame = REPLACE("Sampling_Frame", 'Nahuku / East Rift', 'Nahuku/East Rift')) %>%
  #dplyr::mutate(REPLACE("Sampling_Frame", 'Nahuku / East Rift', 'Nahuku/East Rift')) %>%
  #dplyr::mutate(Sampling_Frame = REPLACE('Nahuku / East Rift', 'Nahuku/East Rift')) %>%
  #dplyr::mutate(Sampling_Frame = REPLACE(" / ", "/")) %>%
  dplyr::collect() %>%
  dplyr::mutate(Sampling_Frame = stringr::str_replace_all(Sampling_Frame, 'Nahuku / East Rift', 'Nahuku/East Rift'))

MapPACNVeg2(sample_frame = "Nahuku/East Rift", protocol = "EIPS")
#Error in df$Transect_Line : $ operator is invalid for atomic vectors
# No EIPS data showing up

look_eips_pts <- FilterPACNVeg(data_name = "Events_extra_xy_EIPS", park = "HAVO")
look_nahu <- unique(look_eips_pts$Sampling_Frame)[2]
look_nahu
sub(x = look_eips_pts$Sampling_Frame, pattern = " / ", replacement = "/")
# "Nahuku / East Rift" has space on both sides of "/" in EIPS
# In FTPC it is "Nahuku/East Rift"

# ----v_cover_bar_stats----
UnderNativityCover.plot.nat_v_non(sample_frame = "Olaa",
                                         cycle = 2,
                                         paired_cycle = 1,
                                         paired_change = TRUE,
                                         combine_strata = TRUE,
                                         crosstalk = TRUE,
                                         crosstalk_group = "fake_grp",
                                         interactive = TRUE)

look <- summarize_understory(sample_frame = "Olaa",
                     combine_strata = TRUE,
                     plant_grouping = "Species",
                     paired_change = TRUE)

v_cover_bar_stats(sample_frame = "Olaa",
                  combine_strata = TRUE,
                  plant_grouping = "Species",
                  paired_change = TRUE,
                  measurement = "Cycle3vs1")

# ----MapPACNVeg2----
sample_frame <- "Mauna Loa"
add_mgmt_unit_test2(sample_frame = "Mauna Loa")

url <- httr::parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/")
url$path <- paste(url$path, "PACN_Vegetation_Sampling_Frames_vlyr/FeatureServer/0/query", sep = "/")
url$query <- list(where = paste0("Sampling_Frame = Sampling_Frame"),
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- httr::build_url(url)
request #print url request

zones <- sf::st_read(request)

factpal <- leaflet::colorFactor(topo.colors(5), zones$Zone)

leaflet::leaflet(zones) %>%
  leaflet::addProviderTiles("CartoDB.Positron") %>%
  leaflet::addPolygons(color =  ~factpal(Zone), label = ~Zone)



url <- httr::parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/")
url$path <- paste(url$path, "PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0/query", sep = "/")
url$query <- list(where = paste0("Sampling_Frame = Sampling_Frame"),
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- httr::build_url(url)
mgmt_unit <- sf::st_read(request, quiet = TRUE)

MapPACNVeg2(protocol = c("FTPC"),
            crosstalk = FALSE,
            crosstalk_group = "map",
            sample_frame = "Mauna Loa",
            is_qa_plot = FALSE)


# ----sunburst labels----
nativity_colors <- c("Native" = "#1b9e77", "No Veg" = "grey", "Non-Native" = "#d95f02", "Unknown" = "#7570b3")
mgmt_unit_colors <- c("#F8573A", "#F4C47B", "#28468B", "#AED5CB")

understorySunburst(sample_frame = "Mauna Loa", cycle = "3", mgmt_unit = FALSE, colors = nativity_colors)




# ----Clidemia in NPSA----
spp_chg_und <- summarize_understory(combine_strata = TRUE,
                     plant_grouping = "Species",
                     paired_change = TRUE,
                     park = "NPSA")

clihir_chg_und <- spp_chg_und %>%
  filter(Scientific_Name == "Clidemia hirta")

presence <- clihir_chg_und %>%
  dplyr::group_by(Year, Sampling_Frame) %>%
  dplyr::mutate(sp_present = dplyr::case_when(
    Cover > 0 ~ TRUE,
    Cover <= 0 ~ FALSE)) %>%
  summarise(plots_present = sum(sp_present),
            total_plots = n(),
            presence = plots_present/total_plots)

# Nativity discrete scale Colors:
param <- "Cover"

nativity_colors <- c("Native" = "#1b9e77",
                     "No_Veg" = "grey",
                     "Non-Native" = "#d95f02",
                     "Unknown" = "#7570b3")

# add stats
clihir_chg_und_stats <- add_stats(clihir_chg_und, Unit_Code, Sampling_Frame,
                              Cycle, Year, Stratum, Nativity, Scientific_Name)

# sample size calculation for text (output is on graph caption)
sample_size <- clihir_chg_und_stats %>%
  dplyr::filter(NPLOTS != 0) %>%
  dplyr::filter(Parameter == param) %>%
  dplyr::select(Sampling_Frame, Year, NPLOTS) %>%
  dplyr::distinct() %>%
  dplyr::group_by(Sampling_Frame) %>%
  dplyr::mutate(count_cycles = match(Year, unique(Year))) %>%
  dplyr::mutate(SF = paste0("; ", Sampling_Frame, ": ")) %>%
  dplyr::mutate(SF = dplyr::case_when(count_cycles == 1 ~ SF,
                                      TRUE ~ "")) %>%
  dplyr::mutate(Text = paste0(SF, Year, " [n = ", NPLOTS, "]")) %>%
  dplyr::pull(Text) %>%
  paste(collapse = ", ") %>%
  stringr::str_sub(3) %>%
  stringr::str_replace_all(", ;", ";")
sample_size

#........BAR YEARLY MEANS
label_param <- stringr::str_replace_all(param, "_", " ")

plot <- clihir_chg_und_stats %>%
  dplyr::mutate(SF_no_space = stringr::str_replace_all(Sampling_Frame, " ", "_")) %>%
  dplyr::filter(NPLOTS != 0) %>%
  dplyr::filter(Parameter == param) %>%
  ggplot2::ggplot(ggplot2::aes(x = Year, y = MEAN, fill = Nativity)) +
  ggplot2::geom_col(position = ggplot2::position_dodge()) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=L, ymax=R), width=.2,
                         position=ggplot2::position_dodge(.9)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(y = paste(label_param, "(% Cover)")) +
  #ggh4x package allows nested facets:
  #ggplot2::facet_grid(Stratum ~ SF_no_space + Nativity,
  #                    labeller = ggplot2::label_parsed,
  #                    scales = "free_x") +
  ggplot2::facet_grid(Scientific_Name ~ Sampling_Frame) +
  ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
  ggplot2::xlab("Year") +
  ggplot2::theme(legend.position="none") +
  ggplot2::labs(caption = sample_size) +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5))

plot

data_table <- clihir_chg_und
paired_change <- TRUE
interactive <- TRUE

# Get max value for plotting data
#toplot.max <- data_table %>%
#  dplyr::ungroup() %>%
#  dplyr::select(dplyr::starts_with(c("Cover")))
#toplot.max <- max(c(abs(max(toplot.max, na.rm = TRUE)), abs(min(toplot.max, na.rm = TRUE))))

input <- data_table %>%
  filter(Sampling_Frame == "Tau") %>%
  drop_na(Chg_Prior) %>%
  dplyr::arrange(-Chg_Prior)


param_cols <- input %>%
  dplyr::pull(Chg_Prior)

input <- dplyr::mutate(input, key = paste0(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle)) #crosstalk recommends using unique key
input <- crosstalk::SharedData$new(input, group = "ctg", key = ~key)
input_table <- input$data()


plt3 <- plotly::plot_ly(data = input,
                       x = ~Plot_Number,
                       y = ~Chg_Prior,
                       type = "bar",
                       colors = pal) %>%
  layout(title = 'Change in Clidemia cover - Tau',
         xaxis = list(categoryorder = "trace",
                      title = "Plot Number"),
         yaxis = list(title = '% Cover'))

plt3

map_input_dataset <- input_table %>%
  mutate(Plot_Number = as.integer(Plot_Number))

map3 <- MapCoverTotal3(sample_frame = "Tau", map_dataset = map_input_dataset, crosstalk = TRUE, crosstalk_group = "ctg")
map3
bscols(widths = c(6, NA), plt3, map3)














input <- data_table %>%
  filter(Sampling_Frame == "Tau")

plt <- plotly::plot_ly(colors = pal) %>%
  #plotly::highlight_key(~Plot_Number) %>%
  plotly::plot_ly(data = input,
                      x = ~ Plot_Number,
                      y = ~ Cover,
                      hoverinfo = "text",
                      #groups = ~Plot_Number,
                      color = ~ Cover,
                      type = "scatter",
                      marker = list(line = list(color = "black"), width = 2, size = ~ Cover*.1),
                      text = ~paste('</br> Plot: ', Plot_Number,
                                    '</br> Year: ', Year,
                                    '</br> Cover: ', round(Cover, 1)),
                      showlegend = TRUE,
                      name = "Plot") %>%
  plotly::highlight(on = "plotly_hover", off = "plotly_doubleclick") %>%
  plotly::layout(xaxis = list(title = "Native cover"), #, range = lims
                 yaxis = list(title = "Non-native cover")) %>% #, range = lims
  plotly::colorbar(title = "% Cover", limits = c(0,100))
plt


# If 'year_filter == TRUE' then add filter checkbox:
if (year_filter) {
  box_filter <- crosstalk::filter_checkbox("html_year_id", "Year", data, ~Year, inline = TRUE)

  plt <- crosstalk::bscols(
    widths = c(12, 12),
    plt, box_filter)
}

return(plt)

# Total cover plots
if (!paired_change) {
  if (interactive) {
    plot.nat_v_non <- pacnvegetation:::totalCover_plotly(data, toplot.max, year_filter = year_filter)  # Plot total cover in plotly
  } else {
    plot.nat_v_non <- totalCover_ggplot(data, toplot.max)  # Plot total cover in ggplot
  }
}

# Paired change plots
if (paired_change) {
  if (interactive) {
    plot.nat_v_non <- changeInCover_plotly(data, toplot.max)  # Plot paired change in plotly
  } else {
    plot.nat_v_non <- changeInCover_ggplot(data, toplot.max)  # Plot paired change in ggplot
  }
}

return(plot.nat_v_non)

v_cover_plot_bar_nativity()

MapPACNVeg2(protocol = "FTPC", sample_frame = "Tutuila", cycle = 1)

Plot_Presence <- FilterPACNVeg("Presence", is_qa_plot = FALSE) %>%
  select(Year, Sampling_Frame, Plot_Number, Scientific_Name)

sp_Plot_Presence <- Plot_Presence %>%
  filter(Scientific_Name == "Clidemia hirta") %>%
  dplyr::group_by(Year, Sampling_Frame) %>%
  summarise(n = n())


# ----Test why MapPACNVeg2 does not work with null transects----

# Because you have to specify no EIPS protocol

# this works:
MapPACNVeg2(protocol = "FTPC", sample_frame = "Tau")
# this does not:
MapPACNVeg2(sample_frame = "Tau")
# because default is:
MapPACNVeg2(protocol = c("FTPC", "EIPS"), sample_frame = "Tau")

names(FilterPACNVeg())

EIPS_in_park <- FilterPACNVeg("EIPS_data", is_qa_plot = FALSE) %>%
  select(Sampling_Frame) %>%
  dplyr::distinct() %>%
  drop_na()

FTPC_in_park <- FilterPACNVeg("Presence", is_qa_plot = FALSE) %>%
  select(Sampling_Frame) %>%
  dplyr::distinct() %>%
  drop_na()

anti_join(FTPC_in_park, EIPS_in_park)

no_eips <- c("Guam", "Tau", "Tutuila", "Puu Alii", "Kaloko-Honokohau")

if (sf %in% no_eips){
  protocol <- "FTPC"
  } else {
    protocol <- c("FTPC", "EIPS")
}

protocol

# Protocols per Park

# ----Plot Presence of a Species ----

Plot_Presence <- FilterPACNVeg("Presence", is_qa_plot = FALSE) %>%
  dplyr::select(Year, Sampling_Frame, Plot_Number, Scientific_Name)

sp_Plot_Presence <- Plot_Presence %>%
  dplyr::filter(Scientific_Name == "Argyroxiphium sandwicense ssp. macrocephalum") %>%
  dplyr::group_by(Year, Sampling_Frame) %>%
  dplyr::summarise(n = dplyr::n())

Species_per_park <- FilterPACNVeg("Presence") %>%
  select(Year, Scientific_Name) %>%
  dplyr::distinct() %>%
  dplyr::group_by(Year) %>%
  summarise(n = n())

# FM Species List - HALE ----

HALE_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/NPSpecies_FullListWithDetails_HALE_20230419182817.xlsx")
KALA_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/NPSpecies_FullListWithDetails_KALA_20230419182842.xlsx")

MAUI_NUI_veg <- bind_rows(HALE_veg, KALA_veg)

names(MAUI_NUI_veg)
head(MAUI_NUI_veg)

MAUI_NUI_veg2 <- MAUI_NUI_veg %>%
  dplyr::select(Unit = "Park Code", Family, Scientific_Name = "Scientific Name", Common_Names = "Common Names", Occurrence, Nativeness, Abundance) %>%
  dplyr::group_by(Scientific_Name) %>%
  dplyr::mutate(Units = paste(Unit, collapse = ", "),
                Occurrence = paste(Occurrence, collapse = ", "),
                Abundance = paste(Abundance, collapse = ", ")) %>%
  tidyr::separate(Common_Names, c("Common_Name", NA), sep = ",", remove = FALSE) %>%
  dplyr::select(Family, Scientific_Name, Nativeness, Common_Name, Units, Occurrence, Abundance) %>%
  dplyr::distinct(Scientific_Name, .keep_all = TRUE) %>%
  dplyr::arrange(Family, Scientific_Name)

unique(MAUI_NUI_veg2$Abundance)

# use write_excel_csv to preserve hawaiian diacriticals
readr::write_excel_csv(MAUI_NUI_veg2, "C:/Users/JJGross/Downloads/NPSpecies_Mauinui_20230419.csv")

# Look at shapefile ----
library(sf)
library(leaflet)
folder <- "C:/Users/JJGross/Downloads/FinalManagementUnitClips/FinalManagementUnitClips/"
shps <- list.files(path = folder, pattern = "\\.shp$")
shps

for (i in shps) {

  print(i)

  if (exists("sf_objects") == FALSE) {
    sf_objects <- sf::st_read(paste0(folder, i)) %>%
      sf::st_transform('+proj=longlat +datum=WGS84')
    print(sf::st_crs(sf_objects))
    count <- 1
  }

  if (exists("sf_objects") == TRUE) {
  sf_object <- sf::st_read(paste0(folder, i)) %>%
    sf::st_transform('+proj=longlat +datum=WGS84')

  print(sf::st_crs(sf_object))

  sf_objects <- rbind(sf_objects, sf_object)

  count <- count + 1
  }

  print(paste("count = ", count))

  }

shps



!exists("blah")
st_crs(HALE_mgmt_layer)
st_bbox(HALE_mgmt_layer)

leaflet(HALE_mgmt_layer_wgs84) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons()

sample_frame <- "Kipahulu"
map <- leaflet::leaflet(pts) %>%
  leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
  leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
  leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
  leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
  leaflet.esri::addEsriFeatureLayer(options = leaflet.esri::featureLayerOptions(where = paste0("Sampling_Frame = '", sample_frame, "'")),
                                    group = "Sampling Frame",
                                    url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0",
                                    useServiceSymbology = TRUE,
                                    labelProperty = "Sampling_Frame") %>%
  leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                            overlayGroups = c("Sampling Frame", grps),
                            options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
  leaflet::addMarkers(lng = ~Long,
                      lat = ~Lat,
                      icon = ~leaflet::icons(iconUrl = custom_icons,
                                             iconWidth = symb_w,
                                             iconHeight = symb_h),
                      group = ~paste(Protocol, "points"),
                      label = ~Sample_Unit_Number,
                      labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold")),
                      popup = ~paste0("<strong>", Protocol, " ", Sample_Unit, ":</strong> ", Sample_Unit_Number,
                                      "<br><strong>", Sample_Unit, " Type:</strong> ", Sample_Unit_Type,
                                      "<br><strong>Sampling Frame:</strong> ", Sampling_Frame,
                                      "<br><strong>Cycle:</strong> ", Cycle,
                                      "<br><strong>Year:</strong> ", Year))





# Test KAHO brief

MapPACNVeg2(sample_frame = "Kaloko-Honokohau")

# List all sampling frames----
all_samp_frames <- FilterPACNVeg("Presence") %>%
  pull(Sampling_Frame) %>%
  unique()
all_samp_frames

# check Albizia for Dave ----
detections <- FilterPACNVeg("Presence") %>%
  filter(Sampling_Frame == "Puu Alii") %>%
  #select(Scientific_Name, Code, Life_Form, Nativity, Family) %>%
  dplyr::distinct()

# ----test GROUP ----
mgmt_test <- readr::read_csv(file = paste0(getwd(),"/R/Events_extra_xy_mgmt.csv"))
und_test <- FilterPACNVeg("Understory", sample_frame = "Mauna Loa")

und_test2 <- und_test %>%
  dplyr::left_join(select(mgmt_test, Zone, Unit_Code, Sampling_Frame, Cycle, Plot_Number),
                   by = c("Unit_Code", "Sampling_Frame", "Cycle", "Plot_Number"))





und_new_out_test <- understory_spp_cover(sample_frame = "Mauna Loa")
understoryBarCover("Mauna Loa")

und_out_test <- understorySpeciesCover2(sample_frame = "Mauna Loa")

# ----test mgmt layer function ----
add.mgmt.unit(sample_frame = "All")
look <- readr::read_csv(file = paste0(getwd(),"/R/Events_extra_xy_mgmt.csv"))

add.mgmt.unit(sample_frame = "Nahuku/East Rift")
add.mgmt.unit(sample_frame = "Mauna Loa")

# Read/Load AGOL layer:
url <- httr::parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/")
url$path <- paste(url$path, "PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0/query", sep = "/")
url$query <- list(where = paste0("Sampling_Frame = Sampling_Frame"),
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- httr::build_url(url)
request #print url request
# Convert AGOL layer into a simple features object
mgmt_unit <- sf::st_read(request)

# Add line to make geometry valid - otherwise was receiving following error:
# Error: "Edge 270 has duplicate vertex with edge 273"
# see following for more information: https://r-spatial.org/r/2017/03/19/invalid.html
mgmt_unit_valid <- sf::st_make_valid(mgmt_unit)


url <- httr::parse_url("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services")
url$path <- paste(url$path, "USA_Railroads_1/FeatureServer/0/query", sep = "/")
url$query <- list(where = "STATE = STATE",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- httr::build_url(url)

Florida_Railroads2 <- sf::st_read(request)



all_sample_frames = "*"
url_test <- httr::parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0/")
url_test$query <- list(outFields = "*",
                       returnGeometry = "true",
                       f = "geojson")
request <- httr::build_url(url_test)
request #print url request
# Convert AGOL layer into a simple features object
mgmt_unit_test <- sf::st_read(request)

writeLines(agol_sample_frame)
url_test <- httr::parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services")
url_test$path <- paste(url_test$path, "PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0/?f=pjson", sep = "/")
url_test$query <- list(returnGeometry = "true",
                  f = "geojson")
request <- httr::build_url(url_test)
request
mgmt_unit_test <- sf::st_read(request)


request <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0/?f=pjson"
request #print url request
# Convert AGOL layer into a simple features object
mgmt_unit_test <- sf::st_read(request)




check_events <- FilterPACNVeg("Events_extra_xy" ,
                              is_qa_plot = FALSE,
                              sample_frame = "Nahuku/East Rift")

add.mgmt.unit(sample_frame = "Nahuku/East Rift")
add.mgmt.unit(sample_frame = "Mauna Loa")

look <- readr::read_csv(file = paste0(getwd(),"/R/Events_extra_xy_mgmt.csv"))


# ----quick lookup of names ----
names(FilterPACNVeg())
"Events_extra_xy"
sfs <- FilterPACNVeg("Events_extra_xy" , is_qa_plot = FALSE) %>%
  dplyr::pull(Sampling_Frame) %>%
  unique()
sfs

Events_filtered <- Events_extra_xy %>%
  select(Year, Cycle, Sampling_Frame, Plot_Number, Plot_Type, Center_Lat, Center_Long)


# ----Species per park ----
"SmWoody"

SmWoody <- FilterPACNVeg("SmWoody", is_qa_plot = FALSE) #%>%
  select(Year, Sampling_Frame, Plot_Number, Scientific_Name)

SmWoody_count <- SmWoody %>%
  filter(LF_Sm_Woody == "Shrub") %>%
  dplyr::group_by(Year, Sampling_Frame, Plot_Number) %>%
  summarise(all_count = sum(Count))
# ----Species per park ----

Species_per_plot <- FilterPACNVeg("Presence", is_qa_plot = FALSE) %>%
  select(Year, Sampling_Frame, Plot_Number, Scientific_Name)

spp_plot_count <- Species_per_plot %>%
  dplyr::group_by(Year, Sampling_Frame, Plot_Number) %>%
  summarise(n = n())

Species_per_park <- FilterPACNVeg("Presence") %>%
  select(Year, Scientific_Name) %>%
  dplyr::distinct() %>%
  dplyr::group_by(Year) %>%
  summarise(n = n())

Species_total <- FilterPACNVeg("Presence") %>%
    select(Scientific_Name) %>%
    dplyr::distinct()

g <- ggplot2::ggplot(Species_per_park, aes(Year)) +
  geom_bar()
g


# ----Look at Mgmt Layer ----
leaflet::leaflet() %>%
  leaflet.esri::addEsriBasemapLayer(leaflet.esri::esriBasemapLayers$Streets) %>%
  leaflet.esri::addEsriFeatureLayer(options = leaflet.esri::featureLayerOptions(where = paste0("Sampling_Frame == '", sample_frame, "'")),
                                    group = "Sampling Frame",
                                    url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0")

leaflet::leaflet() %>%
  leaflet.esri::addEsriBasemapLayer(leaflet.esri::esriBasemapLayers$Streets) %>%
  leaflet.esri::addEsriFeatureLayer(url = paste0("https://services1.arcgis.com/fBc8EJBxQRMcHlei/ArcGIS/rest/services/",
                                                 "PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0"),
                                    useServiceSymbology = TRUE,)

sample_frame = "Hoolehua"

MapPACNVeg2(sample_frame = sample_frame)

sample_frame = "KALA Coast"

leaflet::leaflet() %>%
  leaflet.esri::addEsriBasemapLayer(leaflet.esri::esriBasemapLayers$Streets) %>%
  leaflet.esri::addEsriFeatureLayer(options = leaflet.esri::featureLayerOptions(where = paste0("Sampling_Frame = '", sample_frame, "'")),
                                    url = paste0("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/",
                                                 "PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0"),
                                    useServiceSymbology = TRUE,
                                    labelProperty = "Zone")

leaflet::leaflet() %>%
  leaflet.esri::addEsriBasemapLayer(leaflet.esri::esriBasemapLayers$Streets) %>%
  leaflet.esri::addEsriFeatureLayer(url = paste0("https://services.arcgis.com/rOo16HdIMeOBI4Mb/arcgis/rest/services/",
                                                 "Heritage_Trees_Portland/FeatureServer/0"),
                                    useServiceSymbology = TRUE,)

leaflet::leaflet() %>%
  leaflet.esri::addEsriBasemapLayer(leaflet.esri::esriBasemapLayers$Streets) %>%
  leaflet.esri::addEsriFeatureLayer(options = leaflet.esri::featureLayerOptions(where = paste0("Sampling_Frame == '", sample_frame, "'")),
                                    group = "Sampling Frame",
                                    url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0")

leaflet::leaflet() %>%
  leaflet.esri::addEsriBasemapLayer(leaflet.esri::esriBasemapLayers$Streets) %>%
  leaflet::setView(-122.667, 45.526, 13) %>%
  leaflet.esri::addEsriFeatureLayer(
    url = paste0("https://services.arcgis.com/rOo16HdIMeOBI4Mb/arcgis/rest/services/",
                 "Heritage_Trees_Portland/FeatureServer/0"),
    useServiceSymbology = TRUE,
    labelProperty = "COMMON_NAM", labelOptions = leaflet::labelOptions(textsize = "12px"),
    popupProperty = leaflet::JS(paste0(
      "function(feature) {",
      "  return L.Util.template(",
      "    \"<h3>{COMMON_NAM}</h3><hr />",
      "      <p>This tree is located at {ADDRESS} and its scientific name is {SCIENTIFIC}.</p>",
      "    \",",
      "    feature.properties",
      "  );",
      "}"
    )))


# ----quick lookup of names ----
names(FilterPACNVeg())

all_samp_frames <- FilterPACNVeg("Presence") %>%
  pull(Sampling_Frame) %>%
  unique()
all_samp_frames

all_samp_frames_plots <- FilterPACNVeg("Presence") %>%
  select(Sampling_Frame, Plot_Number, Plot_Type) %>%
  unique()
all_samp_frames_plots

all_samp_frames_trans <- FilterPACNVeg("EIPS_data") %>%
  select(Unit_Code, Year, Sampling_Frame, Transect_Number, Transect_Type) %>%
  mutate(Transect_Number = as.numeric(Transect_Number)) %>%
  unique()
all_samp_frames_trans

# Export Plant photos -----------------------------------------------------------------

chk <- process_photos(AGOL_Layer = "Plants",
                      gdb_name = "generic_delete.gdb",
                      gdb_location = "C:/Users/JJGross/Documents/ArcGIS/Projects/generic_delete",
                      gdb_layer = "Plants_OL_ER_20230124",
                      return_table = TRUE)

write_csv(chk, file = "C:/Users/JJGross/Downloads/HAVO_2021_Plants_20230124.csv")

process_photos(AGOL_Layer = "Plants",
               gdb_name = "generic_delete.gdb",
               gdb_location = "C:/Users/JJGross/Documents/ArcGIS/Projects/generic_delete",
               gdb_layer = "Plants_OL_ER_20230124",
               return_table = FALSE)

names(chk)


write_csv(chk1, file = "C:/Users/JJGross/Downloads/HAVO_2021.csv")

process_photos(AGOL_Layer = "FTPC",
               gdb_name = "FTPC_Olaa_Nahuku_20220323.gdb",
               gdb_location = "C:/Users/JJGross/Downloads/FTPC_Olaa_Nahuku_20220323",
               gdb_layer = "FTPC_Olaa_Nahuku_20220323",
               return_table = FALSE)

chk1 <- chk %>%
  select(created_date, Unit_Code, Samp_Year, Samp_Frame, Site_numb, Staff_List,
         species, code, family, common, lifeform, nativity,
         Taxon_relate, Taxon_comment, Specimen, ID_notes,
         ID_1, ID_1_relate, ID_1_staff,
         ID_2, ID_2_relate, ID_2_staff,
         ID_3, ID_3_relate, ID_3_staff,
         ID_final, ID_final_relate,
         ESRIGNSS_LATITUDE, ESRIGNSS_LONGITUDE)




write_csv(chk1, file = "C:/Users/JJGross/Downloads/Plants_20230120.csv")

chk_2022 <- process_photos(AGOL_Layer = "Plants",
               gdb_name = "AGOL_Backups_20221103.gdb",
               gdb_location = "C:/Users/JJGross/Downloads/AGOL_Backups_20221103",
               gdb_layer = "Plants_KU_ML_20221103",
               return_table = TRUE)

process_photos(AGOL_Layer = "Plants",
                      gdb_name = "AGOL_Backups_20221103.gdb",
                      gdb_location = "C:/Users/JJGross/Downloads/AGOL_Backups_20221103",
                      gdb_layer = "Plants_KU_ML_20221103",
                      return_table = FALSE)




# ----Check new presence function 1/3/2023 ----
K <- v_presence_table(sample_frame = "Kahuku", table_type = "tibble")
ML <- v_presence_table(sample_frame = "Mauna Loa", table_type = "tibble")

KML <- K %>%
  bind_rows(ML) %>%
  pull(Code) %>%
  unique(KML)

KML
l %>%
  knitr::kable(caption = "Group 1")
# Get the number of plots monitored for each year for the table caption.
n_df <- v_presence_table(sample_frame = "Olaa", table_type = "tibble")
n_df <- n_df %>%
  dplyr::select(Year, All_N, Fixed_N) %>%
  tidyr::drop_na() %>%
  dplyr::distinct()
n_fun <- function(x) {paste0(x[1], " = [", x[2], ", ", x[3], "]")}
n_list <- apply(n_df, FUN = n_fun, MARGIN = 1)
n_list <- paste(n_list, collapse = "; ")
n_list

# ----Check crosstalk objects ----
map_cover_data <- read_csv("C:/Users/JJGross/Downloads/MapCoverTotal2_cover_data.csv")
natvsnon_data <- read_csv("C:/Users/JJGross/Downloads/natvsnon_data_table.csv")

look_join <- dplyr::left_join(by = "key", x = map_cover_data, y = natvsnon_data, )

look <- anti_join(by = "key", x = map_cover_data, y = natvsnon_data)

look2 <- anti_join(natvsnon_data, map_cover_data)

# ----Merge NPSpecies Lists ----

# Pohue Inventory Species List
library(readxl)

HAVO_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/NPSpecies_FullListWithDetails_HAVO_20230126115210.xlsx")
# Pohue species list from Kahuku Village EIS (Botanical Resource Assessment - Maya LeGrande)
Pohue_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/Pohue_Species_List.xlsx")

names(HAVO_veg)
names(Pohue_veg)

# Make edits to species names if needed

Pohue_veg2 <- Pohue_veg


# **********************************************************************
#Issue with weird white space (after converting pdf to excel table)
look <- Pohue_veg2$`Scientific Name`[Pohue_veg2$Family == "Euphorbiaceae"]
look2 <- Pohue_veg2$`Scientific Name`[Pohue_veg2$Family == "Asteraceae"]
look
look2
# below lines should replace a space " " with no space ""
# This works for second set but not for first
gsub(" ", "", look)
gsub(" ", "", look2)

gsub("\s", " ", look)
gsub("\s", "", look2)

gsub("\\W", " ", "test var. test") # this removes periods so not good solution
# Convert utf8 characters to integers to see that the space should be "32"
# Instead it shows up as "160"
utf8ToInt(" ")
look2[1]
utf8ToInt(look2[1])

look[1]
utf8ToInt(look[1])

intToUtf8(160)

# Use \\W to grab all weird spaces
#gsub("\\W", "", look) # \\W does not work because it also includes periods
gsub(intToUtf8(160), "", look)
# *******************************************



Pohue_veg2 <- Pohue_veg2 %>%
  mutate(`Scientific Name`= gsub(intToUtf8(160), " ", `Scientific Name`))

Pohue_veg2$`Scientific Name` <- str_replace_all(Pohue_veg2$`Scientific Name`, "subsp.", "ssp.")
Pohue_veg2$`Scientific Name` <- str_trim(Pohue_veg2$`Scientific Name`)

# HAVO_veg2 <- HAVO_veg
# Rename some species in HAVO NPSpecies
#HAVO_veg2$`Scientific Name`[HAVO_veg2$`Scientific Name` == ""] <- ""

# Rename some species in Pohue Kahuku Village Report (to match NPSpecies - even though NPSpecies might be out of date)
Pohue_veg2$`Scientific Name`[Pohue_veg2$`Scientific Name` == "Andropogon virginicus var. virginicus"] <- "Andropogon virginicus"
Pohue_veg2$`Scientific Name`[Pohue_veg2$`Scientific Name` == "Asclepias physocarpus"] <- "Asclepias physocarpa"
Pohue_veg2$`Scientific Name`[Pohue_veg2$`Scientific Name` == "Emilia sonchifolia var. sonchifolia"] <- "Emilia sonchifolia"

HAVO_bind2 <- HAVO_veg %>%
  mutate(Units = "HAVO") %>%
  select(Family, Scientific_Name = "Scientific Name", Nativeness,
         Common_Name = "Common Names", Units, Occurrence, Abundance)

Pohue_bind2 <- Pohue_veg2 %>%
  mutate(Units = "Pohue") %>%
  mutate(Occurrence = "") %>%
  mutate(Abundance = "") %>%
  select(Family, Scientific_Name = "Scientific Name", Nativeness = "Nativity",
         Common_Name = "Common name", Units)

HAVO_Pohue2 <- bind_rows(HAVO_bind2, Pohue_bind2) %>%
  dplyr::mutate(Occurrence = replace_na(Occurrence,"")) %>%
  dplyr::mutate(Abundance = replace_na(Abundance,"")) %>%
  dplyr::group_by(Scientific_Name) %>%
  dplyr::mutate(Units = paste(Units, collapse = ", ")) %>%
                #Occurrence = paste(Occurrence, collapse = ", "),
                #Abundance = paste(Abundance, collapse = ", ")) %>%
  tidyr::separate(Common_Name, c("Common_Name", NA), sep = ",", remove = FALSE) %>%
  dplyr::distinct(Scientific_Name, .keep_all = TRUE) %>%
  dplyr::arrange(dplyr::desc(Units), Family, Scientific_Name)

readr::write_excel_csv(HAVO_Pohue2, "C:/Users/JJGross/Downloads/NPSpecies_and_Kahuku_village_20230126.csv")

# KAHO Early Detection Field Maps Species List

KAHO_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/NPSpecies_FullListWithDetails_KAHO_20221118154653.xlsx")
PUHE_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/NPSpecies_FullListWithDetails_PUHE_20221118154822.xlsx")
PUHO_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/NPSpecies_FullListWithDetails_PUHO_20221118154945.xlsx")

KONA_veg <- bind_rows(KAHO_veg, PUHE_veg, PUHO_veg)

names(KONA_veg)

KONA_veg2 <- KONA_veg %>%
  dplyr::select(Unit = "Park Code", Family, Scientific_Name = "Scientific Name", Common_Names = "Common Names", Occurrence, Nativeness, Abundance) %>%
  dplyr::group_by(Scientific_Name) %>%
  dplyr::mutate(Units = paste(Unit, collapse = ", "),
                Occurrence = paste(Occurrence, collapse = ", "),
                Abundance = paste(Abundance, collapse = ", ")) %>%
  tidyr::separate(Common_Names, c("Common_Name", NA), sep = ",", remove = FALSE) %>%
  dplyr::select(Family, Scientific_Name, Nativeness, Common_Name, Units, Occurrence, Abundance) %>%
  dplyr::distinct(Scientific_Name, .keep_all = TRUE) %>%
  dplyr::arrange(Family, Scientific_Name)

unique(KONA_veg$Abundance)

# use write_excel_csv to preserve hawaiian diacriticals
readr::write_excel_csv(KONA_veg2, "C:/Users/JJGross/Downloads/NPSpecies_Kona_Parks_20221118.csv")

# ----Test USFS FIA packages ----
install.packages("rFIA")
library(rFIA)
HI_FIA <- getFIA(states = "HI")

#devtools::install_github("atkinsjeff/ForestAnalysisInR")
library(ForestAnalysisInR)
## basic example code
launchRFA()

library("ForestAnalysisInR")

detections <- FilterPACNVeg("LgTrees") %>%
  filter(Year == 2022) #%>%
  #select(Scientific_Name, Code, Life_Form, Nativity) %>%
  dplyr::distinct()

# get species DB table for Audreyʻs etymology project ----
detections <- FilterPACNVeg("Presence") %>%
  filter(Year == 2022) %>%
  select(Scientific_Name, Code, Life_Form, Nativity) %>%
  dplyr::distinct()

a <- FilterPACNVeg("Species_extra")

b <- detections %>%
  dplyr::left_join(a, by = c("Scientific_Name", "Code")) %>%
  filter(Park == "HAVO") %>%
  select(-Life_Form.x, -Nativity.x)

readr::write_excel_csv(b, "C:/Users/JJGross/Downloads/2022_Presence.csv")

write_csv(b, "C:/Users/JJGross/Downloads/2022_Presence.csv")

detections_code <- detections %>%
  select(Code)

b_code <- b %>%
  select(Code)

not_in <- b %>%
  filter(!Code %in% detections$Code)

not_in <- detections %>%
  filter(!Code %in% b$Code)

# get two independent crosstalk graphs ----
a <- v_cover_bar_spp_plot(sample_frame = "Kahuku", crosstalk_filters = TRUE, crosstalk_group = "test1")
b <- v_cover_bar_spp_plot(sample_frame = "Kahuku", crosstalk_filters = TRUE, crosstalk_group = "test2")
crosstalk::bscols(a,b,widths = 6,6)
# add 'year' filter to ratio graph/map ----



grp1 <- "cov_total"

#cover_data <- dplyr::mutate(cover_data, key = paste0(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle))
#cover <- crosstalk::SharedData$new(cover_data, group = crosstalk_group, key = ~key)
#filter <- crosstalk::filter_checkbox("year", "Year", grp1, ~Year)

cover_data_look <- UnderNativityCover(paired_change = FALSE, crosstalk = FALSE, sample_frame = "Kahuku")
str(cover_data_look)

pts_look <- PlotAndTransectLocations(protocol = "FTPC", crosstalk = FALSE, sample_frame = "Kahuku")
str(pts_look)

plt1 <- UnderNativityCover.plot.nat_v_non(
  sample_frame = "Kahuku",
  #cycle = 3,
  cycle_filter = TRUE,
  paired_change = FALSE,
  combine_strata = TRUE,
  crosstalk = TRUE,
  crosstalk_group = grp1,
  interactive = TRUE)

map1 <- MapCoverTotal2(crosstalk = TRUE,
                       crosstalk_group = grp1,
                       sample_frame = "Kahuku",
                       cycle = 3)

crosstalk::bscols(plt1, map1)


# troubleshoot EIPS maps ----
test_data <- v_EIPS_prep(sample_frame = "Kahuku")

### Non-Native Species
v_EIPS_map_interstation(.data = test_data,
                        parameter = "Mean_Species_Cover",
                        change = FALSE)

v_EIPS_map_interstation(.data = test_data,
                        parameter = "Mean_Total_Cover",
                        change = FALSE,
                        agol_sample_frame = "Haleakalā Subalpine")

v_EIPS_map_interstation(.data = test_data,
                        parameter = "Max_Richness",
                        change = FALSE)



MapCoverTotal2(crosstalk = TRUE, crosstalk_group = "cover", combine_strata = TRUE, rmv_old_fixed = FALSE, sample_frame = "Mauna Loa")


sample_frame <- "Kahuku"

getmax <- function(col) stringr::str_extract_all(col,"[0-9\\.-]+") %>%
  lapply(.,function(x) max(as.numeric(x), na.rm = T) ) %>%
  unlist()

getmax(test_pts$Year_Text)

test_pts <- PlotAndTransectLocations(protocol = "FTPC", crosstalk = FALSE, crosstalk_group = crosstalk_group, sample_frame = sample_frame) %>%
  dplyr::mutate(Cycle_Text = Cycle,
                Year_Text = Year) %>%
  dplyr::mutate(Cycle = getmax(Cycle),
                Year = getmax(Year),
                Sample_Unit_Number = as.integer(Sample_Unit_Number)) %>%
  dplyr::rename(Plot_Type = Sample_Unit_Type,
                Plot_Number = Sample_Unit_Number) %>%
  dplyr::select(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle, Lat, Long)
cover_data <- UnderNativityCover(combine_strata = combine_strata, paired_change = FALSE, crosstalk = FALSE, park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle,
                                 plot_type = plot_type, silent = silent)

# Combine cover and location data
cover_data <- dplyr::left_join(cover_data, pts, by = c("Unit_Code", "Sampling_Frame", "Plot_Type", "Plot_Number", "Year", "Cycle")) %>%
  dplyr::mutate(nat_ratio = Native_Cover_Total_pct / (NonNative_Cover_Total_pct + Native_Cover_Total_pct) * 100) %>%
  dplyr::arrange(nat_ratio)






# ---- filtering out old fixed if new fixed present
chk_ftpc_pts <- read_csv("C:/Users/JJGross/Downloads/ftpc_pts.csv")

rmv_old_fixed <- chk_ftpc_pts %>%
  dplyr::group_by(Protocol, Sampling_Frame, Sample_Unit_Number) %>%
  dplyr::slice_max(Cycle)





# ---- Prep Veg Crew Demo
names(FilterPACNVeg())

# Grab understory data
understory <- FilterPACNVeg("Understory")

# look at data
understory

# simple filter
understory %>%
  pull(Sampling_Frame) %>%
  unique()

#Summarize understory data
?summarize_understory  #custom function within pacnvegetation package

#Summarize by "Nativity" and filter to "Kahuku"
understory_nat <- summarize_understory(combine_strata = TRUE,
                                    plant_grouping = "Nativity",
                                    paired_change = FALSE,
                                    sample_frame = "Kahuku")

# Use ggplot to make simple bar chart from the summarized data

# Total Nativity cover by plot
understory_nat %>%
  #filter(Plot_Type == "Fixed") %>%
  filter(Nativity != "Unknown") %>%
  ggplot2::ggplot(aes(x = Year,
             y = Cover,
             fill = Nativity)) +
  geom_bar(stat="identity", position = position_dodge()) +
  facet_wrap(~Plot_Number)

#Summarize by "Species" and filter to "Kahuku"
understory_spp <- summarize_understory(combine_strata = TRUE,
                                    plant_grouping = "Species",
                                    paired_change = FALSE,
                                    sample_frame = "Kahuku")

# Use ggplot to make simple bar chart from the summarized data

# Spp cover by plot
understory_spp %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Nativity != "Unknown") %>%
  ggplot2::ggplot(aes(x = Code,
             y = Cover,
             fill = Nativity)) +
  geom_bar(stat="identity", position = position_dodge()) +
  facet_wrap(~Plot_Number)



# Nativity change by plot
understory2_chg <- summarize_understory(combine_strata = TRUE,
                                    plant_grouping = "Nativity",
                                    paired_change = TRUE,
                                    sample_frame = "Kahuku")
understory2_chg %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Nativity != "Unknown") %>%
  ggplot2::ggplot(aes(x = Year,
             y = Chg_Prior,
             fill = Nativity)) +
  geom_bar(stat="identity", position = position_dodge()) +
  facet_wrap(~Plot_Number)





# Re-run using same function but different "arguments"
understory2_spp_chg <- summarize_understory(combine_strata = TRUE,
                                        plant_grouping = "Species",
                                        paired_change = TRUE,
                                        sample_frame = "Kahuku")


# Change in each species across all plots
understory2_spp_chg %>%
  filter(Code == "CENCLA") %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Nativity != "Unknown") %>%
  ggplot2::ggplot(aes(x = Plot_Number,
             y = Chg_Prior,
             fill = Year)) +
  geom_bar(stat="identity", position = position_dodge()) +
  facet_wrap(~Code)

# Change in all species across all plots
understory2_spp_chg %>%
  filter(Plot_Number == 7) %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Nativity != "Unknown") %>%
  filter(Chg_Prior != 0) %>%
  ggplot2::ggplot(aes(x = Code,
             y = Chg_Prior,
             fill = Year)) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) +
  facet_wrap(~Plot_Number)


# Re-run using same function but different "arguments"
understory3 <- summarize_understory(combine_strata = FALSE,
                                            plant_grouping = "Nativity",
                                            paired_change = FALSE,
                                            sample_frame = "Kahuku")

understory_stats <- understory3 %>%
  dplyr::filter(Nativity == "Native" |
                  Nativity == "Non-Native") %>%
  dplyr::group_by(Sampling_Frame, Cycle, Stratum, Nativity) %>%
  dplyr::summarise(NPLOTS = sum(!is.na(Cover)),
                   MEAN = round(mean(Cover, na.rm = TRUE),3),
                   MED = round(median(Cover, na.rm = TRUE),3),
                   MIN = round(min(Cover, na.rm = TRUE),3),
                   MAX = round(max(Cover, na.rm = TRUE),3),
                   SD = sd(Cover, na.rm = TRUE),
                   ERR = qt(0.975,df=NPLOTS-1)*(SD/sqrt(NPLOTS)),
                   L = MEAN - ERR,
                   R = MEAN + ERR)


# Change in each species across all plots
understory_stats %>%
  ggplot2::ggplot(aes(x = Cycle,
             y = MEAN,
             fill = Nativity)) +
  geom_bar(stat="identity", position = position_dodge()) +
  facet_grid(vars(Stratum), vars(Nativity))

# visualizations can also be turned into functions:
v_cover_plot_bar_nativity(sample_frame = "Kahuku")

?v_cover_plot_bar_nativity

UnderNativityCover.plot.nat_v_non(sample_frame = "Kahuku",
                                  cycle = 3)


















understoryBarCover(sample_frame = "Olaa")

Trees <- FilterPACNVeg(data_name = "LgTrees", sample_frame = "Mauna Loa", is_qa_plot = FALSE)
write_csv(x = Trees, file = "C:/Users/JJGross/Downloads/Trees.csv")



SmWoody <- FilterPACNVeg(data_name = "SmWoody", sample_frame = "Mauna Loa", is_qa_plot = FALSE)


# ---- Troubleshoot EIPS transect maps in brief
library(crosstalk)
devtools::install_github("jcheng5/d3scatter")

library(d3scatter)
shared_iris <- SharedData$new(iris)

d3scatter(shared_iris, ~Petal.Length, ~Petal.Width, ~Species, width="100%", height=300)
d3scatter(shared_iris, ~Sepal.Length, ~Sepal.Width, ~Species, width="100%", height=300)


sf <- "Olaa"

test_data <- v_EIPS_prep(sample_frame = sf)

### Non-Native Species
v_EIPS_map_interstation(.data = test_data,
                        parameter = "Mean_Species_Cover",
                        change = FALSE)

# ---- Troubleshoot transect 38 Olaa showing way off on map

look <- PlotAndTransectLocations(protocol = "EIPS", sample_frame = "Olaa")
look_38 <- FilterPACNVeg("Events_extra_xy_EIPS", sample_frame = "Olaa")
look_38_image_pts <- FilterPACNVeg("EIPS_image_pts", sample_frame = "Olaa")

# ---- Troubleshoot sunburst plot


sample_frame <- "Haleakala"
cycle <- 2
und_test <- FilterPACNVeg("Understory", sample_frame = sample_frame, cycle = cycle)  # Only get data from most recent cycle

# PLACEHOLDER
# TODO: replace this with actual grouping column

set.seed(11) # Set random number generation seed so that GROUP_COL is the same each time
#und_test <- mutate(und_test, GROUP_COL = sample(LETTERS[c(1, 2, 2, 3, 5, 5, 5)], size = dplyr::n(), replace = TRUE))

#und_test <- und_test %>%
#  dplyr::mutate(GROUP_COL = dplyr::case_when(Plot_Number <= 5 ~ "A",
#                                             Plot_Number > 5 & Plot_Number < 15 ~ "B",
#                                             Plot_Number >= 15 ~ "C",
#                                             TRUE ~ "D"))
und_test <- und_test %>%
  dplyr::group_by(Cycle, Sampling_Frame, Plot_Number) %>%
  tidyr::nest() %>%
  dplyr::mutate(GROUP_COL = sample(LETTERS[1:5], size = dplyr::n(), replace = TRUE)) %>%
  tidyr::unnest(data) %>%
  dplyr::ungroup()

und_test <- und_test %>%
  dplyr::mutate(Life_Form=replace(Life_Form, Code=="SOPCHR", "Shrub"))

# prep data for sunburst plot
mgmt_unit <- FALSE
#group_by <- c("Cycle", "Nativity")
group_by <- c("Cycle")
if (mgmt_unit) {
  group_by <- c("GROUP_COL", group_by)
}
group_by

und_test <- UnderCombineStrata(und_test) %>%
  dplyr::mutate(across(where(is.character), replace_na, "No Veg")) %>%
  dplyr::group_by(across(tidyselect::all_of(c("Unit_Code", "Sampling_Frame", "Plot_Number", "Nativity", "Life_Form", "Scientific_Name", "Code", group_by)))) %>%
  dplyr::summarize(Hits_Sp = dplyr::n(), .groups = "drop") %>%
  #complete(tidyr::nesting(!!!syms(c("Unit_Code", "Sampling_Frame", "Plot_Number", "Life_Form", "Scientific_Name", "Code", group_by))),
  #         fill = list(Hits_Sp = 0)) %>%
  complete(tidyr::nesting(!!!syms(c("Unit_Code", "Sampling_Frame", "Plot_Number", group_by))),
           tidyr::nesting(!!!syms(c("Nativity", "Code", "Scientific_Name", "Life_Form"))),
           fill = list(Hits_Sp = 0)) %>%
  dplyr::mutate(Plot_Percent = Hits_Sp/300) %>%
  dplyr::group_by(across(tidyselect::all_of(c("Unit_Code", "Sampling_Frame", "Nativity", "Life_Form", "Scientific_Name", "Code", group_by)))) %>%
  dplyr::summarize(n = dplyr::n(),
                   plots_present = sum(Hits_Sp > 0),
                   Avg_Cover = round(mean(Plot_Percent), 3),
                   Std_Dev = round(sd(Plot_Percent), 3),
                   .groups = "drop")


# Create sunburst plot
plot_levels <- c("Nativity", "Life_Form", "Code")
if (mgmt_unit) {
  plot_levels <- c("GROUP_COL", plot_levels)
}
plot_levels

sb <- dplyr::select(und_test, tidyselect::all_of(c(plot_levels, "Avg_Cover")))
sb <- as.sunburstDF(sb, value_column = "Avg_Cover")
#sb$color <- colors[stringr::str_replace(sb$ids, " - .*", "")]
sunburst <- plotly::plot_ly(sb,
                            ids = ~ids,
                            labels = ~labels,
                            parents = ~parents,
                            values = ~values,
                            type = 'sunburst',
                            branchvalues = 'total')#,
                            #marker = list(colors = ~color))

sunburst



# ---- Add in-line variables to Rmarkdown brief template

names <- readxl::read_xlsx(here::here("R", "PACN_veg_names.xlsx")) %>%
  dplyr::filter(Sampling_Frame == params$sample_frame)

x <- names$Park_Name
x
here("R", "PACN_veg_names.csv")

park_name <- FilterPACNVeg("Events_extra_other",
                           sample_frame = params$sample_frame) %>%
  dplyr::slice(1) %>%
  dplyr::pull(Site_Name)

park_name <- FilterPACNVeg("Events_extra_other") %>%
  dplyr::distinct(Sampling_Frame) %>%
  dplyr::pull(Sampling_Frame)
park_name

# ---- Search for Angiopteris

Species_extra <- FilterPACNVeg(data_name = "Species_extra") %>%
  select(Code, Taxonomic_Family) %>%
  dplyr::distinct()

Events_extra_xy <- FilterPACNVeg(data_name = "Events_extra_xy") %>%
  select(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Start_Lat, Start_Long)

EIPS_image_pts <- FilterPACNVeg(data_name = "EIPS_image_pts") %>%
  mutate(Year = as.character(Year)) %>%
  select(-Latitude_Dir, -Longitude_Dir, -GCS)

angiopteris <-  FilterPACNVeg(data_name = "Presence") %>%
  #filter(Code == "ANGEVE") %>%
  filter(Unit_Code == "HAVO") %>%
  dplyr::left_join(Species_extra) %>%
  dplyr::left_join(Events_extra_xy)

# Species Lumping Table (moved to draft_scripts) ----

library(magrittr)

Species_lump <- FilterPACNVeg("Species_extra") %>%
  dplyr::select(Lump_Park = Park,
                Lump_Nativity = Nativity,
                Lump_Life_Form = Life_Form,
                Lump_Genus = Genus,
                Lump_Species = Species,
                Lump_Subdivision = Subdivision,
                Lump_Scientific_Name = Scientific_Name,
                Lump_Code = Code)

Species_lump2 <- Species_lump %>%
  dplyr::group_by(Lump_Park, Lump_Genus, Lump_Species) %>%
  dplyr::filter(dplyr::n()>1) %>%
  dplyr::filter(!is.na(Lump_Genus)) %>%
  dplyr::filter(is.na(Lump_Subdivision))

Species_extra <- FilterPACNVeg("Species_extra") %>%
  dplyr::left_join(Species_lump2, by = c("Park" = "Lump_Park", "Genus" = "Lump_Genus", "Species" = "Lump_Species"))

Species_per_park <- Species_extra %>%
  dplyr::group_by(Lump_Park, Lump_Genus, Lump_Species) %>%
  dplyr::distinct()


# EIPS functions Test ----
library(tidyverse)
sample_frame

test_haleakala <- v_EIPS_prep(sample_frame = "Haleakala")

v_EIPS_map_interstation(.data = test_haleakala,
                        parameter = "Mean_Total_Cover",
                        change = FALSE,
                        sample_frame = "Haleakalā Subalpine")

v_EIPS_map_interstation(.data = test_haleakala,
                        parameter = "Mean_Species_Cover",
                        change = TRUE)

v_EIPS_map_interstation(.data = test_haleakala,
                        parameter = "Max_Richness",
                        change = FALSE)

v_EIPS_map_interstation(.data = test_haleakala,
                        parameter = "Mean_Total_Cover",
                        change = TRUE)

v_EIPS_map_interstation(.data = test_haleakala,
                        parameter = "Mean_Species_Cover",
                        change = TRUE)

olaa_richness <- v_EIPS_map_interstation(.data = test_olaa,
                                         parameter = "Max_Richness",
                                         change = FALSE)

olaa_spp_cover <- v_EIPS_map_interstation(.data = test_olaa,
                                          parameter = "Mean_Species_Cover",
                                          change = FALSE)


# Package Test ----

FilterPACNVeg("EIPS_data", community = "Coastal Strand")
look <- FilterPACNVeg("SmWoody", community = "Wet Forest")
look <- FilterPACNVeg("Presence")

look_locs <- PlotAndTransectLocations()
look_locs_havo <- PlotAndTransectLocations(park = "HAVO")


# Veg map & species locations ----

Hawaii_vegmap_data <- read_vegmap_db(vegmap_db_paths)

hi_poaceae_vegmap <- Hawaii_vegmap_data %>%
  filter(!is.na(lat), !is.na(long)) %>%
  filter(Family == "Poaceae")

names(FilterPACNVeg())

look <- FilterPACNVeg(data_name = "Understory")

# ---- 20220829

Species_extra <- FilterPACNVeg(data_name = "Species_extra") %>%
  select(Code, Taxonomic_Family) %>%
  dplyr::distinct()

Events_extra_xy <- FilterPACNVeg(data_name = "Events_extra_xy") %>%
  select(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Start_Lat, Start_Long)

EIPS_image_pts <- FilterPACNVeg(data_name = "EIPS_image_pts") %>%
  mutate(Year = as.character(Year)) %>%
  select(-Latitude_Dir, -Longitude_Dir, -GCS)

hypolepis <-  FilterPACNVeg(data_name = "Presence") %>%
  #filter(Code == "HYPHAW") %>%
  filter(Unit_Code == "HAVO") %>%
  dplyr::left_join(Species_extra) %>%
  dplyr::left_join(Events_extra_xy)

write_csv(hypolepis, "C:/Users/JJGross/Downloads/hypolepis.csv")

library(leaflet)

pal <- colorFactor(palette = "Spectral",
                   domain = hypolepis$Code)

m <- leaflet(hypolepis) %>%
  addProviderTiles(providers$OpenTopoMap) %>%
  addCircleMarkers(lat = ~Start_Lat, lng = ~Start_Long,
    label = ~as.character(paste(Code, Plot_Number, sep = " Plot ")),
    radius = 6,
    stroke = FALSE, fillOpacity = 0.75,
    fillColor = ~pal(Code))
m



poaceae_FTPC <- FilterPACNVeg(data_name = "Presence") %>%
  filter(Unit_Code %in% c("HAVO", "HALE", "KALA", "KAHO")) %>%
  dplyr::left_join(Species_extra) %>%
  filter(Taxonomic_Family == "Poaceae") %>%
  dplyr::left_join(EIPS_image_pts)

poaceae_EIPS <- v_EIPS_prep() %>%
  filter(Unit_Code %in% c("HAVO", "HALE", "KALA", "KAHO")) %>%
  mutate(Start_Station_m = as.character(Start_Station_m)) %>%
  dplyr::left_join(Species_extra, by = "Code") %>%
  filter(Taxonomic_Family == "Poaceae") %>%
  select(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Segment, Start_Station_m, Scientific_Name, Code, Cover_Class, Meters_Per_Station) %>%
  dplyr::left_join(EIPS_image_pts, by = c("Unit_Code", "Community", "Sampling_Frame", "Year", "Cycle", "Transect_Type", "Transect_Number", "Start_Station_m" = "Image_Point"))

write_csv(poaceae_EIPS, file = "C:/Users/JJGross/Downloads/Poaceae_Nonnative_Transects.csv")
write_csv(poaceae_FTPC, file = "C:/Users/JJGross/Downloads/Poaceae_Plant_Community_Plots.csv")

look <- poaceae_EIPS %>%
  select(Scientific_Name) %>%
  distinct

# ---- 20220615

summarize_understory(sample_frame = "Olaa", plant_grouping = "Nativity")

v_cover_plot_bar_nativity(sample_frame = params$sample_frame, paired_change = FALSE, param = "Cover")

SmWoody <- FilterPACNVeg(data_name = "SmWoody", sample_frame = "Mauna Loa", is_qa_plot = FALSE)

SmWoody %>%
  dplyr::distinct(Code)

Trees %>%
  dplyr::distinct(Code)

SS_SmWoody1 <- SS_SmWoody %>%
  filter(Plot_Number == 3) %>%
  dplyr::group_by(Status, Code, Cycle) %>%
  dplyr::summarize(total = sum(Count)) %>%
  pivot_wider(names_from = Cycle, values_from = total)


#spp <- FilterPACNVeg(data_name = "Species_extra", sample_frame = "Mauna Loa", is_qa_plot = FALSE)


# ---- 20220610
update_photos <- process_photos(AGOL_Layer = "EIPS",
                                gdb_name = "EIPS_OL_ER_20220502.gdb",
                                gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                                gdb_layer = "EIPS_OL_ER_20220502",
                                return_table = TRUE)

update_photos$DATA <- as.list(update_photos$DATA)

update_photos2 <- update_photos[1:5,]

update_photos2$DATA <- as.list(update_photos2$DATA)

apply(X = update_photos, MARGIN = 1, FUN = watermark, new_folder = "watermark_20220610")


# Large Trees - Basal Area ------------------------------------------------


Trees <- FilterPACNVeg(data_name = "LgTrees", sample_frame = "Muchot", is_qa_plot = FALSE)

SmWoody <- FilterPACNVeg(data_name = "SmWoody", sample_frame = "Muchot", is_qa_plot = FALSE)

SmWoody %>%
  dplyr::distinct(Code)

Trees %>%
  dplyr::distinct(Code)

SS_SmWoody1 <- SS_SmWoody %>%
  filter(Plot_Number == 3) %>%
  dplyr::group_by(Status, Code, Cycle) %>%
  dplyr::summarize(total = sum(Count)) %>%
  pivot_wider(names_from = Cycle, values_from = total)

write_csv(SS_SmWoody1, "SmWoody_Plt3.csv")


SS_Trees_BA3 <- SS_Trees %>%
  mutate(bole_dbh = dplyr::case_when(is.na(DBH_Bole) & !is.na(DBH) ~ DBH,
                              is.na(DBH_Bole) & is.na(DBH) ~ 0,
                              TRUE ~ DBH_Bole)) %>%
  mutate(Basal_Area_m2_bole = BA(bole_dbh),
         A_BD = BA(DBH_Other)) %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Quad, Status,
           Height, Height_Dead, Boles, DBH, DBH_Other, Vigor, Rooting, Fruit_Flower, Foliar,
           Shrublike_Growth, Resprouts, Measurement_Type, Scientific_Name, Code,
           Life_Form, Nativity, A_BD) %>%
  dplyr::summarize(A_DBH = sum(Basal_Area_m2_bole),
            bole_check = n()) #%>%
  #filter(Plot_Number == 3)
str(SS_Trees_BA3$Cycle)

SS_Trees_BA4 <- SS_Trees_BA3 %>%
  filter(A_DBH != is.na(A_DBH),
         A_BD != is.na(A_BD))

SS_Trees_BA4 %>%
  mutate(Cycle = as.character(Cycle)) %>%
  pivot_longer(cols = c("A_BD", "A_DBH"), names_to = "Measurement", values_to = "Area") %>%
  ggplot2::ggplot(aes(x = Cycle, y = Area)) +
  geom_violin() +
  facet_grid(rows = vars(Measurement))

SS_Trees_dbh <- SS_Trees_BA4 %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Quad, Status,
           Height, Height_Dead, Boles, DBH, DBH_Other, Vigor, Rooting, Fruit_Flower, Foliar,
           Shrublike_Growth, Resprouts, Measurement_Type, Scientific_Name, Code,
           Life_Form, Nativity, A_BD)
  pivot_wider(names_from = Cycle, values_from = A_DBH)

SS_Trees_BA4
dbh1 <- SS_Trees_BA4 %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Cycle == 1) %>%
  pull(A_DBH) %>%
  var()
dbh2 <- SS_Trees_BA4 %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Cycle == 2) %>%
  pull(A_DBH) %>%
  var()

bd1 <- SS_Trees_BA4 %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Cycle == 1) %>%
  pull(A_BD) %>%
  var()
bd2 <- SS_Trees_BA4 %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Cycle == 2) %>%
  pull(A_BD) %>%
  var()
bd2 - dbh2
bd1 - dbh1
var_bd <- bd2-bd1
var_dbh <- dbh2-dbh1
var_bd/var_dbh

var(A_BD)

SS_Trees_BA3 %>%
  mutate(Cycle = as.character(Cycle)) %>%
  ggplot2::ggplot(aes(x = Cycle, y = A_BD)) +
  ggplot2::geom_boxplot() #+
  #facet_grid(cols = vars())



dbh1 <- SS_Trees_BA3 %>%
  filter(Cycle == 1) %>%
  pull(A_DBH)

mean(dbh1)
var(dbh1)

dbh2 <- SS_Trees_BA3 %>%
  filter(Cycle == 2) %>%
  pull(A_DBH)

mean(dbh2)
var(dbh2)


SS_Trees_Plt3_a <- SS_Trees_Plt3 %>%
  dplyr::arrange(Quad, Year)

write_csv(SS_Trees_Plt3_a, "Lrg_Tree_Plt3.csv")


bole_checks <- SS_Trees_BA3 %>%
  filter(Boles != bole_check)

BA_vs_DBH <- SS_Trees_BA3 %>%
  filter(!is.na(A_BD),
           Code == "METPOL1")

# Variables
#basal <- BA_vs_DBH$A_BD
#DBH <- BA_vs_DBH$A_DBH
ggplot(BA_vs_DBH, aes(x = A_BD, y = A_DBH)) +
  ggplot2::geom_point()

l <- BA_vs_DBH %>%
  filter(!is.na(Height)) %>%
  mutate(LD_height = dplyr::case_when(Height_Dead > Height ~ Height_Dead,
                              TRUE ~ Height))

ggplot(l, aes(x = LD_height, y = A_DBH)) +
  ggplot2::geom_point()
ggplot(l, aes(x = LD_height, y = A_BD)) +
  ggplot2::geom_point()

ggplot(BA_vs_DBH, aes(x = A_BD, y = A_DBH)) +
  ggplot2::geom_point()
ggplot(BA_vs_DBH, aes(x = A_BD, y = A_DBH)) +
  ggplot2::geom_point()


# Distribution of CONT variable
library(ggpubr)
ggdensity(BA_vs_DBH, x = "A_BD", fill = "lightgray", title = "BD") +
  #scale_x_continuous(limits = c(3, 12)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(BA_vs_DBH, x = "A_DBH", fill = "lightgray", title = "DBH") +
  #scale_x_continuous(limits = c(3, 12)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(l, x = "LD_height", fill = "lightgray", title = "height") +
  #scale_x_continuous(limits = c(3, 12)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

cor.test(BA_vs_DBH$Basal_Area_m2_BD, BA_vs_DBH$Basal_Area_m2_DBH, method = 'pearson')

lm1 <- lm(Basal_Area_m2_BD ~ Basal_Area_m2_DBH, data = BA_vs_DBH)
lm1
summary(lm1)
par(mfrow=c(2,2))

plot(lm1, which=1:4)

ggplot(BA_vs_DBH) +
  geom_qq(aes(sample = Basal_Area_m2_BD))

nq <- 100
p <- (1 : nq) / nq - 0.5 / nq
ggplot() +
  ggplot2::geom_point(aes(x = qnorm(p), y = quantile(BA_vs_DBH$Basal_Area_m2_BD, p)))

ggplot() +
  ggplot2::geom_point(aes(x = qexp(p), y = quantile(diamonds$price, p)))

basal <- BA_vs_DBH$Basal_Area_m2_BD
DBH <- BA_vs_DBH$Basal_Area_m2_DBH
n <- nrow(BA_vs_DBH)
p <- (1 : n) / n - 0.5 / n
ggplot(BA_vs_DBH) +
  ggplot2::geom_point(aes(x = basal, y = sort(pnorm(fheight, m, s))))



understory_nativity <- summarize_understory(plant_grouping = "Nativity", combine_strata = TRUE)

understory_species <- summarize_understory(plant_grouping = "Species", combine_strata = TRUE)

library(tidyverse)

understory_nativity1 <- understory_nativity %>%
  filter(Nativity != "Unknown",
         !is.na(Nativity)) %>%
  filter(Cycle != 3)

understory_species1 <- understory_species %>%
  mutate(Presence = dplyr::case_when(Cover > 0 ~ 1,
                              TRUE ~ 0)) %>%
  filter(Presence == 1) %>%
  filter(!is.na(Scientific_Name)) %>%
  filter(Cycle != 3) %>%
  filter(Nativity != "Unknown")

understory_nativity1 %>%
  count(Year, Unit_Code)
#library(ggplot2)

write_csv(understory_species1, "Understory_Species.csv")
write_csv(understory_nativity1, "Understory_Nativity.csv")

library(tidyverse)

understory_species1 <- read_csv("Understory_Species.csv")

understory_species2 <- understory_species1 %>%
  dplyr::group_by(Unit_Code, Sampling_Frame, Cycle, Year, Plot_Type, Plot_Number, Nativity) %>%
  dplyr::summarize(Richness = sum(Presence))

ggplot(understory_nativity1, aes(x=Cover, fill = Nativity)) +
  geom_histogram() +
  facet_grid(cols = vars(Sampling_Frame),
                         rows = vars(Cycle)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(understory_species1, aes(x=Presence)) +
  geom_histogram()

EIPS <- FilterPACNVeg(data_name = "EIPS_data")



update_photos <- process_photos(AGOL_Layer = "FTPC",
               gdb_name = "FTPC_OL_ER_20220503.gdb",
               gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
               gdb_layer = "FTPC_OL_ER_20220503",
               return_table = TRUE)

update_photos2 <- update_photos[1:5,]

update_photos2$DATA <- as.list(update_photos2$DATA)

apply(X = update_photos2, MARGIN = 1, FUN = watermark, new_folder = "watermark2")



# Fix orientation -------------
library(magick)

plots2update <- "OL_47"
update_photos2 <- update_photos %>%
  filter(samp_plot %in% plots2update)

# northwest corner
nw1 <- "TEST"
nw2 <- "ORIENTATION"
nw <- paste(nw1, nw2, sep = "\n")


p2 <- update_photos2$DATA[[3]]

mp2 <- magick::image_read(p2)
mp2

q.upsidedown <- image_attributes(mp2)

mp2_orient <- magick::image_orient(mp1)


img.marked <- magick::image_annotate(mp2_orient, nw,
                                size = 25,
                                gravity = "northwest",
                                font = "Helvetica",
                                color = "white",
                                strokecolor = "black",
                                weight = 900)
img.marked
# Check photos -----------------------------------------------------------------

# Tables to check
# ............... chk_missing
# ............... dupes



# FTPC Photos ---------

# Load FTPC data
chk <- process_photos(AGOL_Layer = "FTPC",
                      gdb_name = "AGOL_Backups_20221103.gdb",
                      gdb_location = "C:/Users/JJGross/Downloads/AGOL_Backups_20221103",
                      gdb_layer = "FTPC_KU_ML_20221103",
                      return_table = TRUE)



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

# EIPS Photos ---------

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



# Code to Rotate an upsidedown image -------------

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

# --------------------------------

# example of function parameters to process photos

process_photos(AGOL_Layer = "EIPS",
                      gdb_name = "EIPS_OL_ER_20220502.gdb",
                      gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                      gdb_layer = "EIPS_OL_ER_20220502",
                      return_table = FALSE)


# ------------------------------------------------------------------------------

transects <- FilterPACNVeg("EIPS_data")

l <- transects %>%
  filter(Sampling_Frame == "Mauna Loa" |
           Sampling_Frame == "Kahuku") %>%
  dplyr::distinct(Cycle, Sampling_Frame, Transect_Number, Transect_Type) %>%
  mutate(Transect_Number = as.numeric(Transect_Number))
  count(Cycle, Sampling_Frame)

AMME_transects <- FilterPACNVeg("EIPS_data") %>%
  filter(Sampling_Frame == "Muchot")

l <- AMME_transects %>%
  dplyr::group_by(Cycle, Transect_Number, Segment) %>%
  summarise(All_Segments = n())

AMME_transects %>%
  dplyr::group_by(Cycle, Scientific_Name, Code, Life_Form, Nativity) %>%
  summarise(total_segments = n())

canopy <- FilterPACNVeg("Canopy")

canopy <- canopy %>%
  dplyr::arrange(Cycle, Sampling_Frame, Plot_Number) #%>%
  summarise(n = n())

small_woody <- FilterPACNVeg("SmWoody") %>%
  #filter(Sampling_Frame == "Haleakala") %>%
  filter(Cycle == 2)


species_missed <- qc_presence_complete()
species_missed <- qc_presence_complete(all_records = FALSE)

FilterPACNVeg("LgTrees") %>%
  filter(Caudex_Length != 999) %>%
  filter(Life_Form == "Tree Fern") %>%
  ggplot2::ggplot(aes(x=Caudex_Length)) +
  geom_histogram(color="black", fill="white") +
  facet_grid(. ~ Sampling_Frame)

large_trees %>%
  #filter(Status == "Dead") %>%
  filter(Life_Form == "Tree Fern") #%>%
  count(Foliar)


shrubs <- FilterPACNVeg("SmWoody")

chk <- process_photos(AGOL_Layer = "EIPS",
               gdb_name = "EIPS_OL_ER_20220502.gdb",
               gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
               gdb_layer = "EIPS_OL_ER_20220502",
               return_table = TRUE)


?process_photos
process_photos(AGOL_Layer = "EIPS",
               gdb_name = "EIPS_Olaa_Nahuku_20220323_1.gdb",
               gdb_location = "C:/Users/JJGross/Documents/RData/PROJECTS/pacnvegetation/geodatabase",
               gdb_layer = "EIPS_Olaa_Nahuku_20220323",
               return_table = FALSE)


LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_1_2021_20220120.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
            cache = TRUE, force_refresh = FALSE)


plt <- UnderNativityCover.plot.nat_v_non(sample_frame = "Haleakala",
                                         combine_strata = TRUE,
                                         crosstalk = TRUE,
                                         interactive = TRUE)
plt




test_sum_und <- summarize_understory(combine_strata = TRUE,
                                     plant_grouping = "Nativity",
                                     sample_frame = "Haleakala")

last_fixed <- test_sum_und %>%
  select(Unit_Code, Sampling_Frame, Cycle, Year, Plot_Type, Plot_Number)


remove_prior_fixed <- test_sum_und %>%
  dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number) %>%
  dplyr::mutate(last_cycle = max(Cycle)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(last_visit = dplyr::case_when(Cycle == last_cycle ~ TRUE,
                TRUE ~ FALSE)) #%>%
  #dplyr::filter(Cycle == last_cycle)


dplyr::filter(dplyr::case_when(Plot_Type == "Fixed"))

plt <- UnderNativityCover.plot.nat_v_non(sample_frame = "Haleakala",
                                         combine_strata = TRUE,
                                         crosstalk = TRUE,
                                         interactive = TRUE)
plt

ps <- highlight_key(data)

year_filter <- crosstalk::filter_checkbox("Cycle", "Monitoring Cycle", data, ~Cycle)

crosstalk::bscols(
  widths = c(11, 1),
  plt, year_filter
)

plt2 <- plt %>%
  plotly::highlight_key(~Plot_Number) %>%
  plotly::highlight(on = "plotly_hover", off = "plotly_doubleclick")
plt2
  plotly::add_segments(x = 0, xend = 150, y = 150, yend = 0,
                       showlegend = TRUE,
                       name = "1:1",
                       line = list(color = "gray"))
plt2


plt <- UnderNativityCover.plot.nat_v_non(sample_frame = "Haleakala",
                                         combine_strata = TRUE,
                                         paired_change = TRUE,
                                         paired_cycle = 1,
                                         crosstalk = TRUE,
                                         interactive = TRUE)
plt

tot_data_noCT_ratio <- tot_data_noCT %>%
  #mutate(nat_ratio = dplyr::case_when(Native_Cover_Total_pct > NonNative_Cover_Total_pct ~ Native_Cover_Total_pct/(Native_Cover_Total_pct+NonNative_Cover_Total_pct),
  #                                      Native_Cover_Total_pct < NonNative_Cover_Total_pct ~ NonNative_Cover_Total_pct/(NonNative_Cover_Total_pct+Native_Cover_Total_pct)*-1,
  #                                      TRUE ~ 0)) %>%
  mutate(nat_ratio = Native_Cover_Total_pct / (NonNative_Cover_Total_pct + Native_Cover_Total_pct) * 100) %>%
  mutate(tot_cover = Native_Cover_Total_pct + NonNative_Cover_Total_pct)

#pal <- grDevices::colorRampPalette(c("red", "orange", "orange", "yellow", "yellow", "green"))(length(unique(tot_data_noCT_ratio$nat_ratio)))
pal <- grDevices::colorRampPalette(c("red", "orange", "yellow", "green"))(length(unique(tot_data_noCT_ratio$nat_ratio)))

plt <- plotly::plot_ly(colors = pal) %>%
  plotly::add_segments(x = 0, xend = 144, y = 0, yend = 144,
                       showlegend = TRUE,
                       name = "1:1",
                       line = list(color = "gray")) %>%
  plotly::add_markers(data = tot_data_noCT_ratio,
                    x = ~ Native_Cover_Total_pct,
                    y = ~ NonNative_Cover_Total_pct,
                    hoverinfo = "text",
                    color = ~ nat_ratio,
                    #colors = pal,
                    #size = ~ tot_cover,
                    type = "scatter",
                    #mode = "markers",
                    marker = list(line = list(color = "black"), width = 2, size = ~ tot_cover*.1),
                    text = ~paste('</br> Plot: ', Plot_Number,
                                  '</br> Native cover: ', round(Native_Cover_Total_pct, 1),
                                  '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1)),
                                  #'</br> Native cover: ', round(Native_Cover_Total_pct, 1)),
                    showlegend = TRUE,
                    name = "Plot") %>%#) %>%
  plotly::highlight(on = "plotly_hover") %>%
  plotly::layout(xaxis = list(title = "Native cover"), #, range = lims
                 yaxis = list(title = "Non-native cover")) %>% #, range = lims
  plotly::colorbar(title = "% Native", limits = c(0,100))
plt

plt <- plotly::plot_ly(data = tot_data_noCT_ratio,
                       x = ~ Native_Cover_Total_pct,
                       y = ~ NonNative_Cover_Total_pct,
                       hoverinfo = "text",
                       color = ~ nat_ratio,
                       colors = pal,
                       size = ~ tot_cover,
                       type = "scatter",
                       mode = "markers",
                       marker = list(line = list(color = "black"), width = 2),
                       text = ~paste('</br> Plot: ', Plot_Number,
                                     '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1),
                                     '</br> Native cover: ', round(Native_Cover_Total_pct, 1))) %>%
  plotly::highlight(on = "plotly_hover") %>%
  plotly::add_segments(x = 0, xend = 100, y = 0, yend = 100) %>%
  plotly::layout(xaxis = list(title = "Native cover"), #, range = lims
                 yaxis = list(title = "Non-native cover"), #, range = lims
                 showlegend = T) %>%
  plotly::colorbar(title = "% Native", limits = c(0,100))
plt

# Get monitoring cycles
cycles <- FilterPACNVeg("Events_extra_xy", sample_frame = params$sample_frame) %>%
  dplyr::select(Cycle) %>%
  unique() %>%
  dplyr::arrange(Cycle)
cycles <- cycles[["Cycle"]]

grp <- "cov_change"
tot_grp <- "tot_change"

plt <- UnderNativityCover.plot.nat_v_non(sample_frame = params$sample_frame,
                                         cycle = max(cycles),
                                         paired_cycle = min(cycles),
                                         paired_change = TRUE,
                                         combine_strata = TRUE,
                                         crosstalk = TRUE,
                                         crosstalk_group = grp,
                                         interactive = TRUE)
plt

tot_data_noCT <- UnderNativityCover(combine_strata = TRUE, paired_change = FALSE,
                                    crosstalk = FALSE, sample_frame = "Haleakala")


tot_data <- UnderNativityCover(combine_strata = TRUE, paired_change = FALSE,
                             crosstalk = TRUE, sample_frame = "Haleakala")


# tot_data_noCT_diff <- tot_data_noCT %>%
#   mutate(diff_abline = dplyr::case_when(Native_Cover_Total_pct > NonNative_Cover_Total_pct ~ abs(Native_Cover_Total_pct-NonNative_Cover_Total_pct),
#                                         Native_Cover_Total_pct < NonNative_Cover_Total_pct ~ abs(NonNative_Cover_Total_pct-Native_Cover_Total_pct)*-1,
#                                         TRUE ~ 0)) %>%
#   mutate(diff_abline = dplyr::case_when(NonNative_Cover_Total_pct == 0 ~ 100,
#                                         Native_Cover_Total_pct == 0 ~ -100,
#                                         TRUE ~ diff_abline))

tot_data_noCT_ratio <- tot_data_noCT %>%
  #mutate(nat_ratio = dplyr::case_when(Native_Cover_Total_pct > NonNative_Cover_Total_pct ~ Native_Cover_Total_pct/(Native_Cover_Total_pct+NonNative_Cover_Total_pct),
  #                                      Native_Cover_Total_pct < NonNative_Cover_Total_pct ~ NonNative_Cover_Total_pct/(NonNative_Cover_Total_pct+Native_Cover_Total_pct)*-1,
  #                                      TRUE ~ 0)) %>%
  mutate(nat_ratio = Native_Cover_Total_pct / NonNative_Cover_Total_pct + Native_Cover_Total_pct) %>%
  mutate(tot_cover = Native_Cover_Total_pct + NonNative_Cover_Total_pct)

#breaks <- c(-1, -0.5, 0, 0.5, 1)
breaks <- c(-1, -0.5, 0, 0.5, 1)

test <- ggplot2::ggplot(tot_data_noCT_ratio, aes(x = Native_Cover_Total_pct, y = NonNative_Cover_Total_pct,
                                        color = nat_ratio, size = tot_cover,
                                        text=sprintf("Plot: %s<br>Year: %s", Plot_Number, Year))) + # Set up text for plotly hover info
  ggplot2::geom_point() +
  geom_abline(intercept = 0, slope=1, color="blue") +
  scale_color_gradientn(colors = c("red", "yellow", "green"), limits = c(-1,1),
                        values = scales::rescale(c(-1, -0.5, 0.7, 1))) +
  ggplot2::labs(colour = "Native Ratio",
       size = "Total Veg Cover (%)",
       x = "Native Cover",
       y = "Non-native Cover") +
  #scale_shape_identity() +
  theme_bw()

test

test <- ggplot2::ggplot(tot_data_noCT_ratio, aes(x = Native_Cover_Total_pct, y = NonNative_Cover_Total_pct,
                                        fill = nat_ratio, size = tot_cover,
                                        text=sprintf("Plot: %s<br>Year: %s", Plot_Number, Year))) + # Set up text for plotly hover info
  ggplot2::geom_point(shape = 21, color = "black") +
  geom_abline(intercept = 0, slope=1, color="blue") +
  scale_fill_gradientn(colors  = c("red", "yellow", "green"), limits = c(-1,1),
                        values = scales::rescale(c(-1, -0.5, 0.7, 1))) +
  ggplot2::labs(fill = "Native Ratio",
       size = "Total Veg Cover (%)",
       x = "Native Cover",
       y = "Non-native Cover") +
  theme_bw()

test

test2 <- test %>%
  plotly::ggplotly(tooltip= c("text")) #%>%
test2

pal <- grDevices::colorRampPalette(c("red", "orange", "orange", "yellow", "yellow", "green"))(length(unique(tot_data_noCT_ratio$nat_ratio)))

plt <- plotly::plot_ly(data = tot_data_noCT_ratio,
        x = ~ Native_Cover_Total_pct,
        y = ~ NonNative_Cover_Total_pct,
        hoverinfo = "text",
        color = ~ nat_ratio,
        colors = pal,
        size = ~ tot_cover,
        type = "scatter",
        mode = "markers",
        marker = list(line = list(color = "black"), width = 2),
        text = ~paste('</br> Plot: ', Plot_Number,
                      '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1),
                      '</br> Native cover: ', round(Native_Cover_Total_pct, 1))) %>%
  plotly::highlight(on = "plotly_hover") %>%
  plotly::layout(xaxis = list(title = "Native cover"), #, range = lims
                 yaxis = list(title = "Non-native cover"), #, range = lims
                 showlegend = FALSE)
plt


pal


  plotly::plot_ly(data = tot_data_noCT_ratio,
                  x = ~ Native_Cover_Total_pct,
                  y = ~ NonNative_Cover_Total_pct,
                  #hoverinfo = "text",
                  color = ~ nat_ratio,
                  colors = pal,
                  size = ~ tot_cover,
                  type = "scatter",
                  mode = "markers",
                  marker = list(line = list(color = "black"), width = 2),
                  text = ~paste('</br> Plot: ', Plot_Number,
                                '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1),
                                '</br> Native cover: ', round(Native_Cover_Total_pct, 1)))
plt



str(tot_data)

plotly::plotly_data(tot_data)

tot_data$data

library("RColorBrewer")
display.brewer.all(type = 'div')

tot_plt <- tot_data_noCT_ratio %>%
  plotly::plot_ly() %>%
  plotly::add_segments(y = 0, yend = 200,
                       x = 0, xend = 200) %>%
  plotly::add_trace(x = ~ Native_Cover_Total_pct,
                    y = ~ NonNative_Cover_Total_pct,
                    hoverinfo = "text",
                    type = "scatter",
                    mode = "markers",
                    showlegend = FALSE,
                    marker = color ~ diff_abline,
                    text = ~paste('</br> Plot: ', Plot_Number,
                                  '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1),
                                  '</br> Native cover: ', round(Native_Cover_Total_pct, 1))) %>%
  # plotly::add_text(data = data,
  #                  x = ~ Native_Cover_Change_pct,
  #                  y = ~ NonNative_Cover_Change_pct,text = ~Plot_Number, textposition = "top right") %>%
  plotly::highlight(on = "plotly_hover") %>%
  plotly::layout(xaxis = list(title = "Native cover"),
                 yaxis = list(title = "Non-native cover"),
                 showlegend = FALSE)
tot_plt

tot_plt <- UnderNativityCover.plot.nat_v_non(sample_frame = params$sample_frame,
                                             cycle = max(cycles),
                                             paired_change = FALSE,
                                             combine_strata = TRUE,
                                             crosstalk = TRUE,
                                             crosstalk_group = tot_grp,
                                             interactive = TRUE)
tot_plt

map <- MapCoverChange(crosstalk = TRUE, crosstalk_group = grp, sample_frame = params$sample_frame, cycle = max(cycles), paired_cycle = min(cycles))

bscols(plt, map)





MapPACNVeg2(sample_frame = "Haleakala")



MapCoverChange(sample_frame = "Haleakala", cycle = 2)


pacnvegetation:::pchIcons(pch = rep(22, nrow(cover_data)),
         width = 30,
         height = 30,
         bg = colorspace::darken(cover_data$color),
         col = cover_data$color, 0.3)

MapCoverChange(combine_strata = TRUE, park = "WAPA", cycle = 2, paired_cycle = 1)

look <- summarize_understory(combine_strata = TRUE, plant_grouping = "Nativity", paired_change = TRUE, sample_frame = "Olaa")
look_old <- UnderNativityCover(combine_strata = TRUE, paired_change = TRUE, sample_frame = "Olaa", cycle = 3)


look <- FilterPACNVeg("Understory", park = "HAVO", cycle = 2)
look <- FilterPACNVeg("Understory", sample_frame = "Haleakala", cycle = 2)

chk <- look %>%
  #filter(Plot_Number == 10) %>%
  filter(Stratum == "Low") %>%
  dplyr::group_by(Sampling_Frame, Cycle, Dead, Scientific_Name, Code, Nativity) %>%
  summarise(hits = n())



v_cover_plot_bar_nativity(sample_frame = "Haleakala", paired_change = FALSE, param = "Cover")


v_cover_plot_bar_nativity(sample_frame = "Haleakala", paired_change = TRUE, param = "Chg_Per_Year")

v_cover_plot_bar_nativity(sample_frame = "Nahuku/East Rift", paired_change = TRUE, param = "Chg_Per_Year")



haleakala_nativity <- summarize_understory(sample_frame = "Haleakala",
                               paired_change = FALSE,
                               plant_grouping = "Nativity")

haleakala_nativity_stats <- add_stats(haleakala_nativity)

haleakala_nativity_paired <- summarize_understory(sample_frame = "Haleakala", paired_change = FALSE, plant_grouping = "Nativity", combine_strata = TRUE)

