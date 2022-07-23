
library(pacnvegetation)
library(tidyverse)
#library(tidytext)


# LoadPACNVeg(ftpc_params = "pacnveg",
#             eips_paths = c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20220428.mdb",
#                          "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/2021_established_invasives_20220422.mdb"),
#             cache = TRUE,
#             expire_interval_days = 14,
#             force_refresh = FALSE)

#WritePACNVeg(dest.folder = "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/R_WritePACNVeg/20220719")

LoadPACNVeg(ftpc_params = "pacnveg",
            eips_paths = c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20220503.mdb",
                           "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/2021_established_invasives_20220606.mdb",
                           "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/2022_established_invasives_20220503.mdb"),
            cache = TRUE,
            expire_interval_days = 30,
            force_refresh = FALSE)



names(FilterPACNVeg())

chk_undr_stry <- summarize_understory(sample_frame = "Olaa", plant_grouping = "Nativity")

# helper functions ----


#' Convert EIPS Cover Class column to Range of Cover or vice-versa
#' @description Input a column with values 0-7 for Braun-Blanquet cover classes and
#' returns two percent cover columns (min and max percent cover). Note that
#' Cover Class "0" returns 0 for min and max, while cover class 7 will return 0.75
#' for min and 1.0 for max.
#' @return Filename and path
#' #' @examples
#' #' \dontrun{
#' MapPACNVeg(protocol = "FTPC")
#' MapPACNVeg(park = "AMME")
#'}
v_EIPS_cover_class2percent <- function(df, cover_column) {
  df <- df %>%
    dplyr::mutate(Cov_Range_Min = dplyr::case_when({{ cover_column }} == 1 ~ 0.005,
                                                   {{ cover_column }} == 2 ~ 0.01,
                                                   {{ cover_column }} == 3 ~ 0.05,
                                                   {{ cover_column }} == 4 ~ 0.10,
                                                   {{ cover_column }} == 5 ~ 0.25,
                                                   {{ cover_column }} == 6 ~ 0.50,
                                                   {{ cover_column }} == 7 ~ 0.75,
                                                   is.na({{ cover_column }}) ~ 0,
                                                   {{ cover_column }} == "OUT" ~ 0)) %>%
    dplyr::mutate(Cov_Range_Max = dplyr::case_when({{ cover_column }} == 1 ~ 0.009,
                                                   {{ cover_column }} == 2 ~ 0.04,
                                                   {{ cover_column }} == 3 ~ 0.09,
                                                   {{ cover_column }} == 4 ~ 0.24,
                                                   {{ cover_column }} == 5 ~ 0.49,
                                                   {{ cover_column }} == 6 ~ 0.74,
                                                   {{ cover_column }} == 7 ~ 1.0,
                                                    is.na({{ cover_column }}) ~ 0,
                                                   {{ cover_column }} == "OUT" ~ 0))
  return(df)
}

#' Convert EIPS Cover Class column to Range of Cover or vice-versa
#' @description Input a column with values 0-7 for Braun-Blanquet cover classes and
#' returns two percent cover columns (min and max percent cover). Note that
#' Cover Class "0" returns 0 for min and max, while cover class 7 will return 0.75
#' for min and 1.0 for max.
#' @return Filename and path
#' #' @examples
#' #' \dontrun{
#' MapPACNVeg(protocol = "FTPC")
#' MapPACNVeg(park = "AMME")
#'}
v_EIPS_cover_percent2class <- function(df, range_column, cover_column_name) {
  paste_col <- paste("col", rlang::as_string(cover_column_name), sep ="_")
  df <- df %>%
    dplyr::mutate( {{cover_column_name}} := dplyr::case_when(
      {{ range_column }} == 0 ~ 0,
      {{ range_column }} > 0 & {{ range_column }} < 0.01 ~ 1,
      {{ range_column }} >= 0.01 & {{ range_column }} < 0.05 ~ 2,
      {{ range_column }} >= 0.05 & {{ range_column }} < 0.10 ~ 3,
      {{ range_column }} >= 0.10 & {{ range_column }} < 0.25 ~ 4,
      {{ range_column }} >= 0.25 & {{ range_column }} < 0.50 ~ 5,
      {{ range_column }} >= 0.50 & {{ range_column }} < 0.75 ~ 6,
      {{ range_column }} >= 0.75 ~ 7))

  df2 <- df %>%
    dplyr::mutate({{ paste_col }} := dplyr::case_when(
      .data[[cover_column_name]] == 0  ~ "#FFFFFF",
      .data[[cover_column_name]] == 1  ~ "#F6F4C6",
      .data[[cover_column_name]] == 2  ~ "#EEE98D",
      .data[[cover_column_name]] == 3  ~ "#EAC07D",
      .data[[cover_column_name]] == 4  ~ "#E7976E",
      .data[[cover_column_name]] == 5  ~ "#E36E5F",
      .data[[cover_column_name]] == 6  ~ "#E04550",
      .data[[cover_column_name]] == 7  ~ "#DD1C41"))
  return(df2)
}

#cover_column_name2 <- "Schtu_Bob"
#deparse(substitute(cover_column_name2))
#paste("col", rlang::as_string(cover_column_name2), sep ="_")
#rm(cover_column_name)

# ---- v_summarize_EIPS() Function: ----
parameter <- "Total Richness" # map
parameter <- "Total Cover Class" # map
parameter <- "Sp Cover Class" # map
parameter <- "Sp Freq" # bar chart

EIPS <- FilterPACNVeg(data_name = "EIPS_data",
                      #sample_frame = "Olaa",
                      is_qa_plot = FALSE)

EIPS_pts <- FilterPACNVeg(data_name = "EIPS_image_pts",
                               #sample_frame = "Olaa",
                               is_qa_plot = FALSE)

# Just certified / verified data?
#EIPS_pts_extra <- FilterPACNVeg(data_name = "Events_extra_other_EIPS",
                          #sample_frame = "Olaa",
                          #is_qa_plot = FALSE)

# Change Year to first year of the Cycle and prep data
EIPS_pts <- EIPS_pts %>%
  dplyr::group_by(Sampling_Frame, Cycle) %>%
  dplyr::mutate(Year = min(Year)) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Transect_Number = as.factor(Transect_Number)) %>%
  dplyr::select(-Latitude_Dir, -Longitude_Dir, -GCS, -GPS_Error)

# Change Year to first year of the Cycle and prep data
EIPS2 <- EIPS %>%
  dplyr::group_by(Sampling_Frame, Cycle) %>%
  dplyr::mutate(Year = min(Year)) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Transect_Number = as.factor(Transect_Number))

# Add segment meter lengths and transect meter lengths specific to Community
EIPS3 <- EIPS2 %>%
  dplyr::mutate(Seg_Length_m = dplyr::case_when(Community == "Subalpine Shrubland" ~ 20,
                                                Community == "Wet Forest" & Sampling_Frame != "Kahuku" ~ 20,
                                                Community == "Wet Forest" & Sampling_Frame == "Kahuku" ~ 10,
                                                Community == "Coastal Strand" | Community == "Mangrove Wetland" ~ 10,
                                                TRUE ~ NA_real_)) %>%
  dplyr::mutate(Tran_Length_m = dplyr::case_when(Community == "Subalpine Shrubland" ~ 500,
                                              Community == "Wet Forest" & Sampling_Frame != "Kahuku" ~ 1000,
                                              Community == "Wet Forest" & Sampling_Frame == "Kahuku" ~ 250,
                                              Community == "Coastal Strand" | Community == "Mangrove Wetland"~ 499,
                                              TRUE ~ NA_real_)) %>%
  dplyr::mutate(Start_m = (Segment * Seg_Length_m) - Seg_Length_m,
                End_m = (Segment * Seg_Length_m)) %>%
  dplyr::mutate(Start_Station_m = dplyr::case_when(Tran_Length_m == 500 & Start_m < 100 ~ 0,
                                                   Tran_Length_m == 500 & Start_m < 200 ~ 100,
                                                   Tran_Length_m == 500 & Start_m < 300 ~ 200,
                                                   Tran_Length_m == 500 & Start_m < 400 ~ 300,
                                                   Tran_Length_m == 500 & Start_m < 500 ~ 400)) %>%
  dplyr::mutate(Start_Station_m = dplyr::case_when(Tran_Length_m == 1000 & Start_m < 200 ~ 0,
                                                   Tran_Length_m == 1000 & Start_m < 400 ~ 200,
                                                   Tran_Length_m == 1000 & Start_m < 600 ~ 400,
                                                   Tran_Length_m == 1000 & Start_m < 800 ~ 600,
                                                   Tran_Length_m == 1000 & Start_m < 1000 ~ 800,
                                                   TRUE ~ Start_Station_m)) %>%
  dplyr::mutate(Start_Station_m = dplyr::case_when(Tran_Length_m < 500 & Start_m < 50 ~ 0,
                                                   Tran_Length_m < 500 & Start_m < 100 ~ 50,
                                                   Tran_Length_m < 500 & Start_m < 150 ~ 100,
                                                   Tran_Length_m < 500 & Start_m < 200 ~ 150,
                                                   Tran_Length_m < 500 & Start_m < 250 ~ 200,
                                                   Tran_Length_m < 500 & Start_m < 300 ~ 250,
                                                   Tran_Length_m < 500 & Start_m < 350 ~ 300,
                                                   Tran_Length_m < 500 & Start_m < 400 ~ 350,
                                                   Tran_Length_m < 500 & Start_m < 450 ~ 400,
                                                   Tran_Length_m < 500 & Start_m < 500 ~ 450,
                                                   TRUE ~ Start_Station_m)) %>%
  dplyr::mutate(End_Station_m = dplyr::case_when(Tran_Length_m == 500 ~ Start_Station_m + 100,
                                                 Tran_Length_m == 1000 ~ Start_Station_m + 200,
                                                 Tran_Length_m < 500 ~ Start_Station_m + 50)) %>%
  dplyr::mutate(Segs_Per_Station = (End_Station_m-Start_Station_m)/Seg_Length_m) %>%
  dplyr::mutate(Meters_Per_Station = Segs_Per_Station*Seg_Length_m)

chk_stations <- EIPS3 %>%
  select(Community, Sampling_Frame, Seg_Length_m, Tran_Length_m, Start_Station_m, End_Station_m) %>%
  distinct()

# Change Cover Class to low and high percentage (low_per, high_per)
EIPS4 <- EIPS3 %>%
  v_EIPS_cover_class2percent(cover_column = Cover_Class)


# SP. COVER----
t_cover_spp_seg <- EIPS4 %>%
  group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year,
           Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
           Seg_Length_m, Segs_Per_Station, Meters_Per_Station, Segment,
           Code, Scientific_Name, Life_Form, Nativity) %>%
  summarize(Tot_Seg_Cover_Min = sum(Cov_Range_Min),
            Tot_Seg_Cover_Max = sum(Cov_Range_Max),
            Tot_Seg_Richness = sum(!is.na(Code))) %>%
  ungroup()

# Summarize by inter-station
t_cover_spp_station <- t_cover_spp_seg %>%
  group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year,
           Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
           Seg_Length_m, Segs_Per_Station, Meters_Per_Station,
           Code, Scientific_Name, Life_Form, Nativity) %>%
  summarize(Actual_Segs = n_distinct(Segment),
            Max_Seg_Richness = max(Tot_Seg_Richness),
            Tot_Station_Cov_Min = sum(Tot_Seg_Cover_Min),
            Tot_Station_Cov_Max = sum(Tot_Seg_Cover_Max)) %>%
  mutate(Actual_Meters = Actual_Segs * Seg_Length_m,
         Mean_Seg_Cov_Min = Tot_Station_Cov_Min/Actual_Segs,
         Mean_Seg_Cov_Max = Tot_Station_Cov_Max/Actual_Segs)#,
#Mean_Cov_per_20m_Low = Tot_Station_Cov_Low/(Actual_Meters/20),
#Mean_Cov_per_20m_High = Tot_Station_Cov_High/(Actual_Meters/20))

# Check for duplicates- should only be one species code per segment
chk_dups <- t_cover_spp_seg %>%
  filter(Tot_Seg_Richness > 1) %>%
  select(Cycle, Year, Sampling_Frame, Transect_Number, Segment, Code, Number_of_Dups = Tot_Seg_Richness) %>%
  arrange(Cycle, Year, Sampling_Frame, Transect_Number, Segment, Code)

nrow(chk_dups)

# if there are any rows in duplicate table, print warning message
if (nrow(chk_dups) != 0) {
  warning("duplicate species records within some segments. use 'chk_dups' to see which segments")
  print(chk_dups)
}


# TOTAL (cover & richness)----

# Richness & Total Cover by Segment
t_cover_total_seg <- EIPS4 %>%
  group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station, Segment) %>%
  summarize(Tot_Seg_Cover_Min = sum(Cov_Range_Min),
            Tot_Seg_Cover_Max = sum(Cov_Range_Max),
            Tot_Seg_Richness = sum(!is.na(Code))) %>%
  ungroup()

# Summarize by inter-station
t_cover_total_station <- t_cover_total_seg %>%
  group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station) %>%
  summarize(Actual_Segs = n_distinct(Segment),
            Max_Seg_Richness = max(Tot_Seg_Richness),
            Tot_Station_Cov_Min = sum(Tot_Seg_Cover_Min),
            Tot_Station_Cov_Max = sum(Tot_Seg_Cover_Max)) %>%
  mutate(Actual_Meters = Actual_Segs * Seg_Length_m,
         Mean_Seg_Cov_Min = Tot_Station_Cov_Min/Actual_Segs,
         Mean_Seg_Cov_Max = Tot_Station_Cov_Max/Actual_Segs)#,
#Mean_Cov_per_20m_Low = Tot_Station_Cov_Low/(Actual_Meters/20),
#Mean_Cov_per_20m_High = Tot_Station_Cov_High/(Actual_Meters/20))


#variable - if variable "species, then,
# if variable total, then,
EIPS_station_summary <- t_cover_spp_station

# Remove incomplete stations with only 1 or two segments
discard <- EIPS_station_summary %>%
  filter(Actual_Meters < Meters_Per_Station/2)

EIPS_station_summary2 <- anti_join(EIPS_station_summary, discard)

#join to GPS points:
EIPS_station_summary3 <- EIPS_station_summary2 %>%
  mutate(Start_Image_Point = as.character(Start_Station_m)) %>%
  left_join(EIPS_pts, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "Start_Image_Point" = "Image_Point")) %>%
  rename(Lat_Start = Latitude,
         Long_Start = Longitude) %>%
  mutate(End_Image_Point = as.character(End_Station_m)) %>%
  left_join(EIPS_pts, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "End_Image_Point" = "Image_Point")) %>%
  rename(Lat_End = Latitude,
         Long_End = Longitude)

# EIPS_station_summary4 <- EIPS_station_summary3 %>%
#   dplyr::mutate(Cover_Class_Max = dplyr::case_when(Mean_Seg_Cov_Max == 0  ~ 0,
#                                                    Mean_Seg_Cov_Max > 0 & Mean_Seg_Cov_Max < 0.01 ~ 1,
#                                                    Mean_Seg_Cov_Max >= 0.01 & Mean_Seg_Cov_Max < 0.05 ~ 2,
#                                                    Mean_Seg_Cov_Max >= 0.05 & Mean_Seg_Cov_Max < 0.10 ~ 3,
#                                                    Mean_Seg_Cov_Max >= 0.10 & Mean_Seg_Cov_Max < 0.25 ~ 4,
#                                                    Mean_Seg_Cov_Max >= 0.25 & Mean_Seg_Cov_Max < 0.50 ~ 5,
#                                                    Mean_Seg_Cov_Max >= 0.50 & Mean_Seg_Cov_Max < 0.75 ~ 6,
#                                                    Mean_Seg_Cov_Max >= 0.75 ~ 7))


# convert percent cover to class----
EIPS_station_summary4 <- EIPS_station_summary3 %>%
  v_EIPS_cover_percent2class(range_column = Mean_Seg_Cov_Max, cover_column_name = "Mean_MaxCover")

# convert Richness to class
EIPS_station_summary5 <- EIPS_station_summary4 %>%
  dplyr::relocate(Max_Seg_Richness, .after = last_col()) %>%
  dplyr::mutate(Richness_Class = dplyr::case_when(Max_Seg_Richness == 0 ~ "0",
                                                  Max_Seg_Richness == 1 ~ "1",
                                                  Max_Seg_Richness >= 2 & Max_Seg_Richness <= 3 ~ "2-3",
                                                  Max_Seg_Richness >= 4 & Max_Seg_Richness <= 5 ~ "4-5",
                                                  Max_Seg_Richness >= 6 & Max_Seg_Richness <= 8 ~ "6-8",
                                                  Max_Seg_Richness >= 9 & Max_Seg_Richness <= 14 ~ "9-14",
                                                  Max_Seg_Richness >= 15 ~ "15+")) %>%
  dplyr::mutate(col_Richness = dplyr::case_when(Richness_Class == "0" ~ "#FFFFFF",
                                                Richness_Class == "1" ~ "#FFFFCC",
                                                Richness_Class == "2-3" ~ "#C7E9B4",
                                                Richness_Class == "4-5" ~ "#7FCDBB",
                                                Richness_Class == "6-8" ~ "#41B6C4",
                                                Richness_Class == "9-14" ~ "#2C7FB8",
                                                Richness_Class == "15+" ~ "#253494")) %>%
  mutate(Txt_Range = paste0(round(Mean_Seg_Cov_Min*100), "-", round(Mean_Seg_Cov_Max*100), "% Cover")) %>%
  mutate(tran_seg_id = paste(Sampling_Frame, Year, Transect_Number, Start_Station_m, sep = "_"))



#RColorBrewer::brewer.pal(n=6,"YlGnBu")

# Pivot data longer so each segment has a row for start lat/long and a separate
# row for end lat/long -
# this is so table can be read by simple features function sf_multilinestring()
Olaa_test4 <- EIPS_station_summary5 %>%
  group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number,
           Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station) %>%
  pivot_longer(cols = c("Lat_Start", "Lat_End"), names_to = "S_E", names_prefix = "Lat_", values_to = "Lat") %>%
  pivot_longer(cols = c("Long_Start", "Long_End"), names_to = "long_S_E", names_prefix = "Long_",values_to = "Long") %>%
  filter(S_E == long_S_E) %>%
  mutate(tran_id = paste(Sampling_Frame, Year, Transect_Number, sep = "_")) %>%
  mutate(tran_seg_id = paste(Sampling_Frame, Year, Transect_Number, Start_Station_m, sep = "_"))

chk_missing_pts <- Olaa_test4 %>%
  filter(is.na(Long)) %>%
  select(tran_seg_id) %>%
  distinct() %>%
  pull(tran_seg_id)
  #select(tran_seg_id, S_E) %>%
  #distinct() #%>%
  #group_by(tran_seg_id) %>%
  #summarize(n = n())
  #filter(is.na(Long))
  # # labels for leaflet legend:
  # mutate(leg_labels = case_when(Mean_MaxCover == 0 ~ "0",
  #                               Mean_MaxCover == 1 ~ "<1",
  #                               Mean_MaxCover == 2 ~ "1-5",
  #                               Mean_MaxCover == 3 ~ "5-10",
  #                               Mean_MaxCover == 4 ~ "10-25",
  #                               Mean_MaxCover == 5 ~ "25-50",
  #                               Mean_MaxCover == 6 ~ "50-75",
  #                               Mean_MaxCover == 7 ~ "75+"))



#'*Remove rows missing spatial data - these need to be fixed!*
# Add spatial data to Olaa 38 and others:
Olaa_test5 <- Olaa_test4 %>%
  #filter(!Sampling_Frame == "Olaa" | !Transect_Number == 38) %>%
  filter(!tran_seg_id %in% chk_missing_pts)

# Make table a simple feature (sf)
EIPS_sf <- sfheaders::sf_multilinestring(
  obj = Olaa_test5
  , x = "Long"
  , y = "Lat"
  , multilinestring_id = "tran_seg_id"
  , keep = T
)

EIPS_sf_centroids <- EIPS_sf %>%
  dplyr::group_by(tran_seg_id) %>%
  dplyr::summarize(geometry = sf::st_union(geometry)) %>%
  sf::st_centroid()

EIPS_sf_centroids_2 <- EIPS_station_summary5 %>%
  select(-Lat_Start, -Lat_End, -Long_Start, -Long_End) %>%
  filter(!tran_seg_id %in% chk_missing_pts) %>%
  left_join(EIPS_sf_centroids) %>%
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2))) %>%
  filter(Sampling_Frame == "Olaa")

all_sites <- EIPS_sf_centroids_2 %>%
  #mutate(tran_seg_id = factor(tran_seg_id)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(Cycle, Year, Unit_Code, Community, Sampling_Frame,
                  Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
                  Seg_Length_m, Segs_Per_Station, Meters_Per_Station,
                  tran_seg_id, lat, long) %>%
  dplyr::mutate(Scientific_Name = "All Sites",
                Code = "All Sites",
                Mean_MaxCover = 0,
                col_Mean_MaxCover = "#FFFFFF")

EIPS_sf_centroids_2 <- bind_rows(all_sites, EIPS_sf_centroids_2)


EIPS_sf_base <- EIPS_sf %>%
  select(Cycle, Year, Unit_Code, Community, Sampling_Frame, Transect_Type, Transect_Number, Start_Station_m, geometry) %>%
  filter(Sampling_Frame == "Olaa")

#sub <- EIPS_sf %>%
#  filter(Transect_Number == 1)

#chk <- sf::st_cast(sub, "LINESTRING")
#EIPS_sp_chk <- sf::as_Spatial(chk)
#EIPS_sp_sub <- sf::as_Spatial(sub)

library(crosstalk)
library(leaflet)

sd_cover <- crosstalk::SharedData$new(EIPS_sf_centroids_2)

# custom palette for legend
Palette <- c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41")
Levels <- as.factor(c(0,1,2,3,4,5,6,7))
Labels <- c("0","<1", "1 - 5","5 - 10","10 - 25", "25 - 50", "50 - 75", "75+")
custom_leg <- data.frame(Palette, Levels, Labels)

bscols(widths = c(1,NA),device = "lg",
       list(
         crosstalk::filter_checkbox("year", "Year", sd_cover, ~Year),
         crosstalk::filter_select("species", "Species", sd_cover, ~Code)
         ),
       leaflet(sd_cover, width = "100%", height = 900) %>%
         addProviderTiles("OpenStreetMap") %>%
         addCircleMarkers(color = ~col_Mean_MaxCover,
                          stroke = FALSE,
                          radius = 6,
                          fillOpacity = 1,
                          popup = ~paste(sep = "<br/>",
                                         paste("Transect", Transect_Number),
                                         Scientific_Name,
                                         Txt_Range)) %>%
         addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
            title = "Cover (%)",
            opacity = 1))


txt_range <- paste0(EIPS_sf_centroids_2$Mean_Seg_Cov_Min*100, "-", EIPS_sf_centroids_2$Mean_Seg_Cov_Max*100, "% Cover")


leaflet(sd_cover) %>%
  addTiles() %>%
  addCircleMarkers()


base_map <- leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolylines(data = EIPS_sf_base, color = ~ "white", opacity = 1) %>%
  addCircleMarkers(data = EIPS_sf_centroids_2$geometry,
                   color = EIPS_sf_centroids_2$col_Mean_MaxCover,
                   stroke = FALSE,
                   radius = 6,
                   fillOpacity = 1)
base_map
#centroid_map <-

#pal <- c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41")
#classes <- as.factor(c(0,1,2,3,4,5,6,7))
#cover_cols <- data.frame(pal,classes)

#make color palette for leaflet map

#fpal_cover <- colorFactor(palette = c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41"),
#                          levels = as.factor(c(0,1,2,3,4,5,6,7)))

#Palette <- c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41")
#Levels <- as.factor(c(0,1,2,3,4,5,6,7))
#Labels <- c("0","<1", "1 - 5","5 - 10","10 - 25", "25 - 50", "50 - 75", "75+")
#custom_leg <- data.frame(Palette, Levels, Labels)

#fpal_cover2 <- colorFactor(palette = c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41"),
#                          levels = as.factor(c("0" , "<1", "1-5", "5-10", "10-25", "25-50", "50-75", "75+")))

#fpal_cover

#str(Olaa_test5$Mean_MaxCover)

test_map <- leaflet() %>%
  addProviderTiles(leaflet::providers$Esri.WorldImagery)
test_map

# Species Cover
EIPS_sf_sp <- EIPS_sf %>%
  filter(Code == "PSICAT")

for (i in 1:nrow(EIPS_sf_sp)) {
  sub_data <- EIPS_sf_sp[i,]
  #print(sub_data)
  test_map <- addPolylines(map = test_map,
                           data = sub_data,
                           color = ~col_Mean_MaxCover,
                           opacity = 1, label = ~as.character(Transect_Number))
}
test_map


# Total Cover
for (i in 1:nrow(EIPS_sf)) {
  sub_data <- EIPS_sf[i,]
  #print(sub_data)
  test_map <- addPolylines(map = test_map,
                           data = sub_data,
                           color = ~col_Mean_MaxCover,
                           opacity = 1, label = ~as.character(Transect_Number))
}
test_map
# Richness

#hist(EIPS_sf$Max_Seg_Richness)
#qpal <- colorQuantile("Blues", EIPS_sf$Max_Seg_Richness, n = 7)

for (i in 1:nrow(EIPS_sf)) {
  sub_data <- EIPS_sf[i,]
  #print(sub_data)
  test_map <- addPolylines(map = test_map,
                           data = sub_data,
                           color = ~col_Richness,
                           opacity = 1, label = ~as.character(Transect_Number))
}

test_map

#for (i in 1:nrow(EIPS_sf)) {
#  sub_data <- EIPS_sf[i,]
#  #print(sub_data)
#  test_map <- addPolylines(map = test_map,
#                           data = sub_data,
#                           color = ~cover_class_cols(Mean_MaxCover),
#                           opacity = 1)
#}

#test_map %>%
  #leaflet(data = Olaa_test5$Mean_MaxCover) %>%
  #addLegend("bottomright", pal = fpal_cover, values = ~ Mean_MaxCover,
            #title = "Non-native Plant Cover") #,
            #labFormat = labelFormat(prefix = "$"),
            #opacity = 1)

#test_map %>%
#  addLegend(pal = cover_cols$pal, values = cover_cols$classes)

#test_map %>%
#  addLegend(pal = fpal_cover, values = as.factor(c(0,1,2,3,4,5,6,7)), labels = Olaa_test5$leg_labels)

# ---- MAP ----

# Cover

# custom palette for legend
Palette <- c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41")
Levels <- as.factor(c(0,1,2,3,4,5,6,7))
Labels <- c("0","<1", "1 - 5","5 - 10","10 - 25", "25 - 50", "50 - 75", "75+")
custom_leg <- data.frame(Palette, Levels, Labels)

test_map %>%
  addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
            title = "Cover (%)",
            opacity = 1) %>%
  addPopups()

# Richness

Palette <- c("#FFFFFF", "#FFFFCC", "#C7E9B4", "#7FCDBB", "#41B6C4", "#2C7FB8", "#253494")
Labels <- c("0", "1", "2-3", "4-5", "6-8", "9-14", "15+")
custom_leg <- data.frame(Labels, Palette)

test_map %>%
  addLegend(colors = custom_leg$Palette,
            labels = custom_leg$Labels,
            title = "Species",
            opacity = 1)
























#str(cover_cols$pal)
#str(cover_cols$classes)




library(sf)

## MULTIPOINT
p <- rbind(c(3.2,4), c(3,4.6), c(3.8,4.4), c(3.5,3.8), c(3.4,3.6), c(3.9,4.5))
mp <- st_multipoint(p)
plot(mp)

##LINESTRING
s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))
ls <- st_linestring(s1)
plot(ls)

## MULTILINESTRING
s2 <- rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
s3 <- rbind(c(0,4.4), c(0.6,5))
mls <- st_multilinestring(list(s1,s2,s3))
mls
plot(mls)


## Olaa Multlinestring
Olaa_mls <- EIPS_station_summary4 %>%
  ungroup() %>%
  filter(Sampling_Frame == "Olaa",
         Year == 2021)

Olaa_mls_t1 <- Olaa_mls %>%
  filter(Transect_Number == 1) %>%
  dplyr::select(Start_Long, Start_Lat) %>%
  as.list()
Olaa_mls_t1
s2
str(s2)
Olaa_mls_t1
str(Olaa_mls_t1)

Olaa_mls_t2 <- Olaa_mls %>%
  filter(Transect_Number == 2) %>%
  dplyr::select(Start_Long, Start_Lat) %>%
  as.list()
Olaa_mls_t2

Olaa_mls_t32 <- Olaa_mls %>%
  filter(Transect_Number == 32) %>%
  dplyr::select(Start_Long, Start_Lat) %>%
  as.list()
Olaa_mls_t32

Olaa_mls_list <- st_multilinestring(c(Olaa_mls_t1, Olaa_mls_t2, Olaa_mls_t32))
#Olaa_mls_list
Olaa_geomlist <- st_sfc(Olaa_mls_t1, Olaa_mls_t2, Olaa_mls_t32)
Olaa_geomlist
Olaa_geomlist <- st_sfc(Olaa_mls_list)
Olaa_geomlist
Olaa_geomlist <- st_sfc(Olaa_mls_list)
Olaa_geomlist
look <- as.data.frame(Olaa_geomlist)

Olaa_attributes <- c("#d11141", "#f37735", "#C8E52A")


Olaa_sf <- sf::st_sf(Cover = Olaa_attributes, Olaa_geomlist)
Olaa_sf

look <- as.data.frame(Olaa_sf)
plot(Olaa_mls_list)

#library(leaflet)
map <-  leaflet(Olaa_sf) %>%
  leaflet::addTiles() %>%
  leaflet::addPolylines(color = ~Cover)
map

Olaa_sf

cover_map <- leaflet() %>%
  addTiles()

for (i in 1:nrow(Olaa_sf)) {
  sub_data <- Olaa_sf[i,]
  print(sub_data)
  cover_map <- addPolylines(map = cover_map,
                            data = sub_data,
                            color = ~Cover)

}

cover_map

cover_map
sub_data <- Olaa_sf
look<- as.data.frame(sub_data$Olaa_geomlist)
cover_map <- addPolylines(map = cover_map,
                          data = sub_data,
                          color = ~Cover)
cover_map
Olaa_sf[1,]


for (i in 1:nrow(Olaa_sf)) {
  cover_map <- addPolylines(map = cover_map,
                            data = Olaa_sf,
                            lng = as.numeric(Olaa_sf[i, c('lng', 'nextLng')]),
                            lat = as.numeric(Olaa_sf[i, c('lat', 'nextLat')]),
                            color = as.character(Olaa_sf[i, c('Cover')])
                            #lng = as.numeric(Olaa_sf[i, c('lng', 'nextLng')]),
                            #lat = as.numeric(Olaa_sf[i, c('lat', 'nextLat')]),
                            #color = ~Cover)
  )
}
cover_map






str(EIPS_stations)

#check

EIPS_segment_check <- EIPS4 %>%
   group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Segment) %>%
   summarize(n_dist = n_distinct()) %>%
   group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number) %>%
   summarise(segments = n())

EIPS_transects_incomplete <- EIPS_segment_check %>%
  filter(Sampling_Frame == "Olaa" & segments != 50)

if (nrow(EIPS_transects_incomplete)>0) {
  warning('some transects incomplete')
  EIPS_transects_incomplete
  }

nn_richness_seg <- EIPS2 %>%
  group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type,  Transect_Number, Segment, Code) %>%
  summarize(sp = sum(!is.na(Code))) %>%
  group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type,  Transect_Number, Segment) %>%
  summarise(Richness = sum(sp))

nn_richness_mean <- seg_richness %>%
  group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type,  Transect_Number) %>%
  summarize(Mean_Richness = mean(Richness))

# spp_count %>%
#   filter(Cycle == 3) %>%
#   ggplot(aes(x=Segment, y=spp)) +
#   geom_bar(stat="identity") +
#   facet_grid(vars(Cycle, Transect_Number))



#if (paired_change == FALSE) {

#  return(understory3)

#} else {

all_vars <- c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number")

inv_freq2 <- inv_freq %>%
    dplyr::filter(Transect_Type == "Fixed")

  arrange_remove <- c("Cycle", "Year")
  arrange_vars <- all_vars[!all_vars %in% arrange_remove]

  max_cycle <- inv_freq2 %>% dplyr::pull(Cycle) %>% max()
  max_cycle_lable <- rlang::as_label(max_cycle)
  max_cycle_lable <- paste0("Cycle", max_cycle_lable, "vs1")

  inv_freq3 <- inv_freq2 %>%
    # Arrange table so that difference in cover between cycles can be calculated easily (example - cycle 1 value for
    #   cover is followed by cycle 2 value for cover).
    dplyr::group_by(dplyr::across(arrange_vars)) %>%
    dplyr::arrange(Cycle, Year, .by_group = TRUE) %>%
    # Calculate the change in cover per cycle
    dplyr::mutate(Chg_Prior = Spp_Freq - dplyr::lag(Spp_Freq, order_by = Cycle)) %>%
    dplyr::mutate(Year = as.numeric(as.character(Year))) %>%
    dplyr::mutate(Years_Prior = Year - dplyr::lag(Year, order_by = Cycle)) %>%
    dplyr::mutate(Year = as.factor(Year)) %>%
    dplyr::mutate(Chg_Per_Year = Chg_Prior / Years_Prior) %>%
    dplyr::mutate(!!max_cycle_lable := Spp_Freq - dplyr::lag(Spp_Freq, order_by = Cycle,
                                                          n = max_cycle-1)) %>%
    dplyr::ungroup()

# inv_freq3 output of summarize_EIPS() function


# ---- v_EIPS_plot_spp_per_transect
inv_freq %>%
  ggplot2::ggplot(ggplot2::aes(x = Year, y = Spp_Freq)) +
  ggplot2::geom_boxplot()

inv_freq %>%
  filter(Transect_Type == "Rotational") %>%
  ggplot2::ggplot(ggplot2::aes(x = Year, y = Spp_Freq)) +
  ggplot2::geom_boxplot()

inv_freq %>%
  filter(Transect_Type == "Fixed") %>%
  ggplot2::ggplot(ggplot2::aes(x = Year, y = Spp_Freq)) +
  ggplot2::geom_boxplot()


spp_count  %>%
  ggplot2::ggplot(ggplot2::aes(x = fct_reorder(Transect_Number, spp, .fun = median), y = spp, fill = Year)) +
  ggplot2::geom_boxplot()

  ggplot2::geom_col(position = ggplot2::position_dodge()) #+
  ggplot2::geom_errorbar(ggplot2::aes(ymin=L, ymax=R), width=.2,
                         position=ggplot2::position_dodge(.9)) +


# ---- v_EIPS_plot_bar() Function:
param = "Spp_Freq"

# Get raw data
inv_freq3 <- inv_freq3

# add stats
inv_freq_stats <- add_stats(inv_freq3, Unit_Code, Sampling_Frame,
                                Cycle, Year)

  # sample size calculation for text (output is on graph caption)
sample_size <- inv_freq_stats %>%
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

plot <- inv_freq_stats %>%
  dplyr::mutate(SF_no_space = stringr::str_replace_all(Sampling_Frame, " ", "_")) %>%
  dplyr::filter(NPLOTS != 0) %>%
  dplyr::filter(Parameter == param) %>%
  ggplot2::ggplot(ggplot2::aes(x = Year, y = MEAN, fill = Sampling_Frame)) +
  ggplot2::geom_col(position = ggplot2::position_dodge()) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=L, ymax=R), width=.2,
                         position=ggplot2::position_dodge(.9)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(y = paste(label_param, "(non-natives per 20 m)")) +
  #ggh4x package allows nested facets:
  #ggplot2::facet_grid(Stratum ~ SF_no_space,
  #                    labeller = ggplot2::label_parsed,
  #                    scales = "free_x") +
  ggplot2::scale_fill_manual(values = "#d95f02", limits = force) +
  ggplot2::xlab("Year") +
  ggplot2::theme(legend.position="none") +
  ggplot2::labs(caption = sample_size) +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5))
plot

inv_freq_stats <- add_stats(inv_freq, Unit_Code, Sampling_Frame,
                                Cycle, Transect_Type, Transect_Number)
