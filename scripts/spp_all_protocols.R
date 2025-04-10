#--- 1. Workspace prep ------------------------------------------------------
library(pacnvegetation)
library(tidyverse)

#**  Make sure to download latest FTPC and EIPS data using start.R  *
LoadPACNVeg(force_refresh = FALSE, eips_paths = "foo")

samp <- "Nahuku/East Rift"
samp <- "Olaa"
spp_filter <- c("Arundina graminifolia", "Phaius tankervilleae", "Spathoglottis plicata")



#--- 2. EIPS presence ----

# NOTE EIPS points are inter-station points (not segments):



test_data <- v_EIPS_prep(sample_frame = samp)

# Currently option to export df from function not available so I used breakpoint
# in v_EIPS_map_interstation3() function to utilize intermediate
# 'station_summary_centroids' table before map:

v_EIPS_map_interstation3(.data = test_data,
                         parameter = "Mean_Species_Cover",
                         change = FALSE,
                         agol_sample_frame = samp)



EIPS_orchids_Olaa <- station_summary_centroids |>
  sf::st_drop_geometry() |>
  dplyr::select(-geometry)

typeof(EIPS_orchids_Olaa)

EIPS_orchids_Olaa_filter <- EIPS_orchids_Olaa |>
  dplyr::filter(Scientific_Name %in% spp_filter) |>
  dplyr::filter(!is.na(Sampling_Frame))

readr::write_csv(EIPS_orchids_Olaa_filter, paste0("C:/Users/JJGross/Downloads/EIPS_interstation_points_", samp, "_", Sys.Date(), ".csv"))

# Stop debugger:

EIPS_Station_Summary <- readr::read_csv(paste0("C:/Users/JJGross/Downloads/EIPS_interstation_points_", samp, "_", Sys.Date(), ".csv"))

EIPS_Species_info <- FilterPACNVeg(data_name = "Species_extra_EIPS")

Events_extra_QAQC_EIPS <- FilterPACNVeg(data_name = "Events_extra_QAQC_EIPS")

#Events_extra_xy_EIPS <- FilterPACNVeg(data_name = "Events_extra_xy_EIPS")

names(FilterPACNVeg())

EIPS_presence <- EIPS_Station_Summary |>
  mutate(Transect_Number = as.character(Transect_Number)) |>
  select(-Life_Form) |>
  left_join(EIPS_Species_info, by = join_by(Scientific_Name, Code, Unit_Code == Park)) |>
  #left_join(Events_extra_xy_EIPS) |>
  mutate(Site_Type = paste(Transect_Type, "Transect")) |>
  select(Unit_Code, Sampling_Frame, Year, Scientific_Name, Code, Life_Form,
         Long = long, Lat = lat,
         Taxonomic_Order, Taxonomic_Family, Genus, Species, Subdivision,
         Authority, Authority_Source, Citation, Life_Cycle, Park_Common_Name,
         Distribution, Conservation_Status, Site_Type,
         Site_Number = Transect_Number) |>
  mutate(Nativity = "Non-Native") |>
  mutate(Plot_Shape = "Rectangle",
         Plot_Width_m = 5,
         Plot_Length_m = 200) |>
  mutate(GCS = "World Geodetic System 1984") |>
  mutate(Lat_Dir = "North") |>
  mutate(Long_Dir = case_when(Sampling_Frame == "Muchot" ~ "East",
                              .default = "West")) |>
  mutate(Project = "EIPS")



#--- 3. FTPC presence ----

#understory <- FilterPACNVeg(data_name = "Understory")

all_presence <- FilterPACNVeg(data_name = "Presence") |>
  dplyr::filter(Scientific_Name %in% spp_filter) |>
  dplyr::filter(Sampling_Frame == samp)

FTPC_xy <- FilterPACNVeg(data_name = "Events_extra_xy") |>
  select(-Azimuth_Plot, -QA_Plot, -Certified, -Verified, -Start_Lat, -Start_Long,
         -End_Lat, -End_Long, Lat = Center_Lat, Long = Center_Long)

FTPC_date <- FilterPACNVeg(data_name = "Events_extra_QAQC") |>
  mutate(Observation_Date = as.Date(Start_Date)) |>
  select(Unit_Code, Sampling_Frame, Cycle, Plot_Number, Observation_Date)

FTPC_spp_info <- FilterPACNVeg(data_name = "Species_extra") |>
  select(-Life_Form, -Nativity, -Update_Date, -Update_By, -Update_Comments, -Species_ID)

FTPC_presence <- all_presence |>
  filter(Sampling_Frame == samp) |>
  filter(Cycle %in% c(1,2)) |>
  mutate(Plot_Shape = "Rectangle") |>
  mutate(Plot_Width_m = case_when(Community == "Coastal Strand" ~ 10,
                                  .default = as.numeric(20))) |>
  mutate(Plot_Length_m = case_when(Community == "Coastal Strand" ~ 20,
                                  .default = as.numeric(50))) |>
  left_join(FTPC_xy) |>
  left_join(FTPC_date) |>
  left_join(FTPC_spp_info, by = join_by(Scientific_Name, Code, Unit_Code == Park)) |>
  select(-Certified, -Verified, -cf, -Outside_Plot, -Comments, -QA_Plot) |>
  select(-Community, -Sampling_Frame_Formal, -Cycle) |>
  mutate(Site_Type = paste(Plot_Type, " Plot")) |>
  select(-Plot_Type) |>
  mutate(Site_Number = as.character(Plot_Number)) |>
  select(-Plot_Number) |>
  mutate(Project = "FTPC")


#--- 4. Veg Maps ----

vegmap_db_paths <- c(#"C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/havodata.accdb",
  #"C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/haledata.accdb",
  "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/kahodata.mdb",
  #"C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/kaladata.mdb",
  "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/puhedata.mdb",
  "C:/Users/JJGross/OneDrive - DOI/Documents/Veg_Map_Data/puhodata.mdb")

#WAPA_vegmap_db_paths <- c("C:/Users/JJGross/Documents/Veg_Map_Data/wapadata.mdb")


vegmap_data <- read_vegmap_db(vegmap_db_paths)

check <- vegmap_data$Field_Name == vegmap_data$Sci_Name
# Are any values False?
!any(check) # columns are the same, remove Sci_Name

vegmap_presence <- vegmap_data |>
  select(-Plot_Event, -Spp_Code, Scientific_Name = Field_Name,
         Observation_Date = Event_Date, -Field_X, -Field_Y, -GPS_Error, -UTM_Zone,
         -Sci_Name, -ComName, -SciName_w_Auth, -Family, Lat = lat, Long = long,
         Plot_Radius_m = Plot_Radius, Plot_Width_m = Plot_Width,
         Plot_Length_m = Plot_Length) |>
  tidyr::separate(Plot_Code, sep = "\\.", into = c("Unit_Code", "Site_Number")) |>
  mutate(Site_Number = as.integer(Site_Number)) |>
  mutate(Site_Type = case_when(Observation_pt == TRUE ~ "Observation point",
                               Observation_pt == FALSE ~ "Classification plot")) |>
  mutate(Plot_Radius_m = case_when(Observation_pt == TRUE ~ NA,
                               Observation_pt == FALSE ~ 11.284)) |>
  mutate(Plot_Shape = case_when(Observation_pt == TRUE ~ "Variable spatial extent",
                                Observation_pt == FALSE ~ "Circle")) |>
  select(-Observation_pt) |>
  mutate(Project = "Veg Mapping")

#--- 5. Early Detection ----
#--- 6. Combine All ----

all_presence <- bind_rows(FTPC_presence, EIPS_presence) |> #vegmap_presence
  relocate(Project, Unit_Code, Year, Observation_Date, Lat, Long, GCS, Lat_Dir, Long_Dir,
           Scientific_Name, Code, Nativity, Life_Form) |>
  relocate(Site_Type, Site_Number, Plot_Shape,
           #Plot_Radius_m,
           Plot_Length_m, Plot_Width_m,
           .after = Nativity#ITIS_TSN
  )
  #select(-Year)


readr::write_csv(all_presence, paste0("C:/Users/JJGross/Downloads/Olaa_nonnative_orchids_", Sys.Date(), ".csv"))
