
library(pacnvegetation)
library(tidyverse)
#library(tidytext)


LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20220428.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/2021_established_invasives_20220422.mdb"),
            cache = TRUE, force_refresh = FALSE)

names(FilterPACNVeg())

chk_undr_stry <- summarize_understory(sample_frame = "Olaa", plant_grouping = "Nativity")

# ---- v_summarize_EIPS() Function:

parameter <- "Richness"
parameter <- "Species_Freq"

EIPS <- FilterPACNVeg(data_name = "EIPS_data",
                      #sample_frame = "Olaa",
                      is_qa_plot = FALSE)

EIPS_pts <- FilterPACNVeg(data_name = "EIPS_image_pts",
                               #sample_frame = "Olaa",
                               is_qa_plot = FALSE)

EIPS_pts_extra <- FilterPACNVeg(data_name = "Events_extra_other_EIPS",
                          #sample_frame = "Olaa",
                          is_qa_plot = FALSE)

EIPS_pts <- EIPS_pts %>%
  dplyr::group_by(Sampling_Frame, Cycle) %>%
  dplyr::mutate(Year = min(Year)) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Transect_Number = as.factor(Transect_Number)) %>%
  dplyr::select(-Latitude_Dir, -Longitude_Dir, -GCS, -GPS_Error)

EIPS2 <- EIPS %>%
  dplyr::group_by(Sampling_Frame, Cycle) %>%
  dplyr::mutate(Year = min(Year)) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Transect_Number = as.factor(Transect_Number))

# Add meter lengths
EIPS3 <- EIPS2 %>%
  dplyr::mutate(Seg_Length_m = dplyr::case_when(Community == "Subalpine Shrubland" ~ 20,
                                                Community == "Wet Forest" & Sampling_Frame != "Kahuku" ~ 20,
                                                Community == "Wet Forest" & Sampling_Frame == "Kahuku" ~ 10,
                                                Community == "Coastal Strand" | Community == "Mangrove Forest" ~ 10,
                                                TRUE ~ NA_real_)) %>%
  dplyr::mutate(Tran_Length_m = dplyr::case_when(Community == "Subalpine Shrubland" ~ 500,
                                              Community == "Wet Forest" & Sampling_Frame != "Kahuku" ~ 1000,
                                              Community == "Wet Forest" & Sampling_Frame == "Kahuku" ~ 250,
                                              Community == "Coastal Strand" | Community == "Mangrove Forest"~ 499,
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
  dplyr::distinct()

# Change Cover Class to low and high percentage (low_per, high_per)
EIPS4 <- EIPS3 %>%
  dplyr::mutate(Cov_Range_Low = dplyr::case_when(Cover_Class == 1 ~ 0.005,
                                                 Cover_Class == 2 ~ 0.01,
                                                 Cover_Class == 3 ~ 0.05,
                                                 Cover_Class == 4 ~ 0.10,
                                                 Cover_Class == 5 ~ 0.25,
                                                 Cover_Class == 6 ~ 0.50,
                                                 Cover_Class == 7 ~ 0.75,
                                                 is.na(Cover_Class) ~ 0,
                                                 Cover_Class == "OUT" ~ 0)) %>%
  dplyr::mutate(Cov_Range_High = dplyr::case_when(Cover_Class == 1 ~ 0.009,
                                                 Cover_Class == 2 ~ 0.04,
                                                 Cover_Class == 3 ~ 0.09,
                                                 Cover_Class == 4 ~ 0.24,
                                                 Cover_Class == 5 ~ 0.49,
                                                 Cover_Class == 6 ~ 0.74,
                                                 Cover_Class == 7 ~ 1.0,
                                                 is.na(Cover_Class) ~ 0,
                                                 Cover_Class == "OUT" ~ 0))



# Richness & Total Cover by Segment
EIPS_seg_summary <- EIPS4 %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station, Segment) %>%
  dplyr::summarize(Tot_Seg_Cover_Low = sum(Cov_Range_Low),
            Tot_Seg_Cover_High = sum(Cov_Range_High),
            Tot_Seg_Richness = sum(!is.na(Code))) %>%
  dplyr::ungroup()


EIPS_station_summary <- EIPS_seg_summary %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station) %>%
  dplyr::summarize(Actual_Segs = n_distinct(Segment),
            Max_Seg_Richness = max(Tot_Seg_Richness),
            Tot_Station_Cov_Low = sum(Tot_Seg_Cover_Low),
            Tot_Station_Cov_High = sum(Tot_Seg_Cover_High)) %>%
  mutate(Actual_Meters = Actual_Segs * Seg_Length_m,
         Mean_Seg_Cov_Low = Tot_Station_Cov_Low/Actual_Segs,
         Mean_Seg_Cov_High = Tot_Station_Cov_High/Actual_Segs)#,
            #Mean_Cov_per_20m_Low = Tot_Station_Cov_Low/(Actual_Meters/20),
            #Mean_Cov_per_20m_High = Tot_Station_Cov_High/(Actual_Meters/20))

# Remove incomplete stations with only 1 or two segments
discard <- EIPS_station_summary %>%
  filter(Actual_Meters < Meters_Per_Station/2)

EIPS_station_summary2 <- anti_join(EIPS_station_summary, discard)

#join to GPS points:

EIPS_station_summary3 <- EIPS_station_summary2 %>%
  mutate(Start_Image_Point = as.character(Start_Station_m)) %>%
  dplyr::left_join(EIPS_pts, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "Start_Image_Point" = "Image_Point")) %>%
  rename(Start_Lat = Latitude,
         Start_Long = Longitude) %>%
  mutate(End_Image_Point = as.character(End_Station_m)) %>%
  dplyr::left_join(EIPS_pts, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "End_Image_Point" = "Image_Point")) %>%
  rename(End_Lat = Latitude,
         End_Long = Longitude)

EIPS_station_summary4 <- EIPS_station_summary3 %>%
  dplyr::mutate(Cover_Class_High = dplyr::case_when(Mean_Seg_Cov_High == 0  ~ 0,
                                                    Mean_Seg_Cov_High > 0 & Mean_Seg_Cov_High < 0.01 ~ 1,
                                                    Mean_Seg_Cov_High >= 0.01 & Mean_Seg_Cov_High < 0.05 ~ 2,
                                                    Mean_Seg_Cov_High >= 0.05 & Mean_Seg_Cov_High < 0.10 ~ 3,
                                                    Mean_Seg_Cov_High >= 0.10 & Mean_Seg_Cov_High < 0.25 ~ 4,
                                                    Mean_Seg_Cov_High >= 0.25 & Mean_Seg_Cov_High < 0.50 ~ 5,
                                                    Mean_Seg_Cov_High >= 0.50 & Mean_Seg_Cov_High < 0.75 ~ 6,
                                                    Mean_Seg_Cov_High >= 0.75 ~ 7))
library(leaflet)
map <-  leaflet(EIPS_station_summary4)
map <- addTiles(map)
for( group in levels(EIPS_station_summary4$Mean_Seg_Cov_High)){
  map <- addPolylines(map, lng=~Start_long,lat=~Start_lat,data=EIPS_station_summary4[EIPS_station_summary4$Mean_Seg_Cov_High==group,], color=~"red")
}
map

# map:





str(EIPS_stations)

#check

EIPS_segment_check <- EIPS4 %>%
   dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Segment) %>%
   dplyr::summarize(n_dist = n_distinct()) %>%
   dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number) %>%
   summarise(segments = n())

EIPS_transects_incomplete <- EIPS_segment_check %>%
  filter(Sampling_Frame == "Olaa" & segments != 50)

if (nrow(EIPS_transects_incomplete)>0) {
  warning('some transects incomplete')
  EIPS_transects_incomplete
  }

nn_richness_seg <- EIPS2 %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type,  Transect_Number, Segment, Code) %>%
  dplyr::summarize(sp = sum(!is.na(Code))) %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type,  Transect_Number, Segment) %>%
  summarise(Richness = sum(sp))

nn_richness_mean <- seg_richness %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type,  Transect_Number) %>%
  dplyr::summarize(Mean_Richness = mean(Richness))

# spp_count %>%
#   filter(Cycle == 3) %>%
#   ggplot2::ggplot(aes(x=Segment, y=spp)) +
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
    dplyr::group_by(across(arrange_vars)) %>%
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
  ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(Transect_Number, spp, .fun = median), y = spp, fill = Year)) +
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
