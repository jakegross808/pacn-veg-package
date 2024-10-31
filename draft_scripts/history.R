


library(pacnvegetation)
library(tidyverse)

FTPC_HALE_2023 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_FTPC_Sampling_Points_Photos/FeatureServer/89"
EIPS_HALE_2023 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_EIPS_Sampling_Points_Photos/FeatureServer/93"
Plants_HALE_2023v1 <-"https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_VEG_Sampling_Plant_Photos/FeatureServer/91"
Plants_HALE_2023v2 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_VEG_Sampling_Plant_Photos_v2/FeatureServer/91"

subset_photos_layers <- c("FTPC_HALE_2023", "EIPS_HALE_2023")

temp_dest <- "C:/Users/JJGross/Downloads/"

download_agol2(subset_photos_layers,
               #master_spreadsheet_folder = mfolder,
               #after_date_filter = m_last_date_tz_1,
               temp_dest,
               test_run = TRUE,
               only_staff = TRUE)

test_first6 <- download_agol2(subset_photos_layers,
                              #master_spreadsheet_folder = mfolder,
                              #after_date_filter = m_last_date_tz_1,
                              temp_dest,
                              only_staff = TRUE,
                              test_run = TRUE)

tstaff <- test_first5 %>%
  filter(stringr::str_detect(Subject,"Staff"))



xlsx_location <- paste0(temp_dest, as.character(mfile))
xl_csv_location <- paste0(temp_dest, as.character(tools::file_path_sans_ext(mfile)), ".csv")
xlsx_location
xl_csv_location
openxlsx::write.xlsx(iris, file = xlsx_location)
openxlsx::write.xlsx(test_first5, file = xlsx_location)
readr::write_excel_csv(test_first5, file = xl_csv_location)

# Get R to read a sharepoint xlsx file:
mfolder <- "C:/Users/JJGross/OneDrive - DOI/HALE_2023_Plants_Master_Spreadsheet"
mfile <- list.files(mfolder, pattern = "\\.xlsx$")
mpath <- paste0(mfolder, "/", mfile)
mpath
mtable1 <- readxl::read_xlsx(mpath, col_types = )

mtable2 <- mtable1 %>%
  mutate(ESRIGNSS_LATITUDE = suppressWarnings(as.double(ESRIGNSS_LATITUDE))) %>%
  mutate(ESRIGNSS_LONGITUDE = suppressWarnings(as.double(ESRIGNSS_LONGITUDE))) %>%
  mutate(ESRIGNSS_ALTITUDE = suppressWarnings(as.double(ESRIGNSS_ALTITUDE))) %>%
  mutate(ESRIGNSS_H_RMS = suppressWarnings(as.double(ESRIGNSS_H_RMS))) %>%
  mutate(photo_cnt = suppressWarnings(as.character(photo_cnt))) %>%
  mutate(pt_date = suppressWarnings(as.character(pt_date)))


mpark <- mtable$Unit_Code %>%
  unique()

park <- c("AMME", "HALE", "HAVO", "KAHO", "KALA", "NPSA", "WAPA")
park_time_zones <- c("Pacific/Guam", "HST", "HST", "HST", "HST", "US/Samoa", "Pacific/Guam" )
all_tz <- data.frame(park, park_time_zones)

park_tz <- all_tz %>%
  filter(park %in% mpark) %>%
  pull(park_time_zones)

m_last_date <- mtable %>%
  select(pt_date) %>%
  pull() %>%
  max(na.rm = TRUE)

# Add time zone to last date because gets imported as "UTC"
m_last_date_tz <- ymd_hms(m_last_date, tz = park_tz)
# Add one minute because excel did not import seconds for some reason:
m_last_date_tz_1 <- m_last_date_tz + 60


# Download table to see dates of recent photos
test_first4 <- download_agol2(subset_photos_layers,
                              master_spreadsheet_folder = mfolder,
                              #after_date_filter = m_last_date_tz_1,
                              temp_dest,
                              test_run = TRUE)

# subset table to only include records after last download
recent_photos <- test_first3[test_first3$pt_date > m_last_date_tz_1, ]

look <- bind_rows(mtable2, recent_photos) %>%
  dplyr::arrange(pt_date)



# ---- check brief .Rmd to get it working again

# getting following error:

# processing file: no_text_20230508.Rmd
# |............                                |  27% [sampling-map]
# Quitting from lines 154-155 [sampling-map] (no_text_20230508.Rmd)
# Error in `if (is.na(agol_sf$Zone)) ...`:
#   ! the condition has length > 1
# Backtrace:
#   1. pacnvegetation::MapPACNVeg2(...)
#
# Execution halted

# Ran chucks until line 153, then:
MapPACNVeg2(sample_frame = "Muchot", protocol = "FTPC")
# Had to change to replace_na with "no zone assigned"









# ----  herbarium_data ----
# currently saved the .Rmd in R folder: herbarium_labels_word_ftExtra.Rmd



# --- check photo processing ----

# Can pull directly from AGOL with headless account set up
Plants_HALE_2023v1 <-"https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_VEG_Sampling_Plant_Photos/FeatureServer/91"
#Plants_HALE_2023v2 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HALE_2023_VEG_Sampling_Plant_Photos_v2/FeatureServer/91"
#subset_photos_layers <- c("Plants_HALE_2023v1")
temp_dest <- "C:/Users/JJGross/Downloads/"
test_first <- download_agol(Plants_HALE_2023v1, temp_dest, test_run = TRUE)

# I haven't incorporated headless pull into process_photos function yet,
# So for now just download .gdb and run following:

# Run with return_table = TRUE first to see if working:
table <- process_photos(AGOL_Layer = "FTPC",
               gdb_name = "get_AGOL_layers.gdb",
               gdb_location = "C:/Users/JJGross/Documents/ArcGIS/Projects/get_AGOL_layers",
               gdb_layer = "FTPC_ERF14_OLF01",
               return_table = TRUE)

# Then change to FALSE and remove assignment to object
process_photos(AGOL_Layer = "FTPC",
               gdb_name = "get_AGOL_layers.gdb",
               gdb_location = "C:/Users/JJGross/Documents/ArcGIS/Projects/get_AGOL_layers",
               gdb_layer = "FTPC_ERF14_OLF01",
               return_table = FALSE)

# --- Clean! Presence spp consistency chk ----
chk_pres <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                                          sample_frame = "Olaa",
                                          plot_number = 1)


rare <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                                      sample_frame = "Olaa") %>%
  dplyr::group_by(Sampling_Frame, Scientific_Name, Code, Plot_Number) %>%
  dplyr::summarize(observed = n(), .groups = "drop") %>%
  # only count one time if found more than once in a fixed
  mutate(observed = 1) %>%
  dplyr::group_by(Sampling_Frame, Code, Scientific_Name) %>%
  dplyr::summarize(plots_observed = n()) %>%
  # "rare" will be 4 plots_observed or less
  filter(plots_observed < 5) %>%
  mutate(less_than_5_plots = TRUE) %>%
  dplyr::right_join(chk_pres) %>%
  filter(less_than_5_plots == TRUE)

# Join rare species flags to Presence
chk_pres1 <- chk_pres %>%
  dplyr::left_join(rare)

chk_pres2 <- chk_pres1 %>%
  mutate(Cycle = as.integer(Cycle)) %>%
  dplyr::arrange(Scientific_Name)

# Code below was to split graph in half but doesn't work with geom_point filtering
# find the middle of the data set
#df_length <- length(chk_pres2$Scientific_Name)
#df_middle <- df_length/2

# Which species is in the middle of the dataset
#split_at_sp <- chk_pres2$Code[df_middle]

# select the row after the last record for that species to split evenly accross species
#split_at <- max(which(chk_pres2$Code == split_at_sp)) + 1
#split_at

# Splits dataset equally in half:
#chk_pres2$split <- "first"
#chk_pres2$split[split_at:df_length] <- "second"


# Nativity discrete scale Colors:
nativity_colors <- c("Native" = "#1b9e77",
                     "No_Veg" = "grey",
                     "Non-Native" = "#d95f02",
                     "Unknown" = "#7570b3")

select_rare <- function(condition){
  function(d) d %>% dplyr::filter_(condition)
}

select_out <- function(condition){
  function(d) d %>% dplyr::filter_(condition)
}

chk_pres2 %>%
  ggplot2::ggplot(aes(x= Scientific_Name, y=Cycle)) +
  ggplot2::geom_segment(aes(x=Scientific_Name,
                   xend=Scientific_Name,
                   y=min(Cycle),
                   yend=max(Cycle),
                   color = Nativity),
               linetype="dashed",
               linewidth=0.1) +
  # Draw points
  ggplot2::geom_point(size = 8, data = ~filter(.x, less_than_5_plots == TRUE), color = "yellow") +
  ggplot2::geom_point(size = 5, aes(color = Nativity)) +
  ggplot2::geom_point(size = 2, data = ~filter(.x, Outside_Plot == TRUE), color = "black") +
  ggplot2::labs(title="Check Presence",
       subtitle= (paste0(chk_pres2$Sampling_Frame[1], " Plot ", chk_pres2$Plot_Number[1])),
       caption= (paste0("QA/QC"))) +
  ggplot2::scale_color_manual(values = nativity_colors) +
  ggplot2::scale_x_discrete(limits = rev) +
  ggplot2::scale_y_continuous(limits = c(0, max(chk_pres2$Cycle)+1)) + #breaks = integer_breaks(),
  ggplot2::coord_flip() +
  #facet_wrap(scales = "free", vars(chk_pres2$split)) +
  ggplot2::theme(strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank()) +
  ggplot2::theme(aspect.ratio=18)





# --- Presence spp consistency chk ----
chk_pres <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                                          sample_frame = "Olaa",
                                          plot_number = 1)


rare <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                                      sample_frame = "Olaa") %>%
  dplyr::group_by(Sampling_Frame, Scientific_Name, Code, Plot_Number) %>%
  dplyr::summarize(observed = n(), .groups = "drop") %>%
  # only count one time if found more than once in a fixed
  mutate(observed = 1) %>%
  dplyr::group_by(Sampling_Frame, Code, Scientific_Name) %>%
  dplyr::summarize(plots_observed = n()) %>%
  # "rare" will be 4 plots_observed or less
  filter(plots_observed < 5) %>%
  mutate(less_than_5_plots = TRUE) %>%
  dplyr::right_join(chk_pres) %>%
  filter(less_than_5_plots == TRUE) %>%
  mutate(rare_color = "yellow")

any_rare <- any(!is.na(rare$less_than_5_plots))
rare

chk_pres1 <- chk_pres %>%
  dplyr::left_join(rare)

chk_pres2 <-  chk_pres1%>%
  mutate(outside_color = dplyr::case_when(
    Outside_Plot == TRUE ~ "black",
    .default = ""))

chk_pres2 <- chk_pres2 %>%
  mutate(Cycle = as.integer(Cycle)) %>%
  dplyr::arrange(Scientific_Name)

#chk_pres2$Outside_Plot[3] <- TRUE

# find the middle of the data set
df_length <- length(chk_pres2$Scientific_Name)
df_middle <- df_length/2

# Which species is in the middle of the dataset
split_at_sp <- chk_pres2$Code[df_middle]

# select the row after the last record for that species to split evenly accross species
split_at <- max(which(chk_pres2$Code == split_at_sp)) + 1
split_at

chk_pres2$split <- "first"
chk_pres2$split[split_at:df_length] <- "second"

#chk_pres1 <- chk_pres %>%
#  dplyr::left_join(rare)

# Plot
# A function factory for getting integer y-axis values.
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# Nativity discrete scale Colors:
nativity_colors <- c("Native" = "#1b9e77",
                     "No_Veg" = "grey",
                     "Non-Native" = "#d95f02",
                     "Unknown" = "#7570b3")

select_rare <- function(condition){
  function(d) d %>% dplyr::filter_(condition)
}

select_out <- function(condition){
  function(d) d %>% dplyr::filter_(condition)
}

chk_pres2 %>%
  ggplot2::ggplot(aes(x= Scientific_Name, y=Cycle)) +
  ggplot2::geom_segment(aes(x=Scientific_Name,
                   xend=Scientific_Name,
                   y=min(Cycle),
                   yend=max(Cycle),
                   color = Nativity),
               linetype="dashed",
               linewidth=0.1) +
  #{if(any_rare)geom_point(size=8, x = rare$Scientific_Name,
  #                       y = rare$Cycle,
  #                       color = rare$less_than_5_plots)} +
  #geom_point(fill = chk_pres$less_than_5_plots, size=6) +
  #geom_point(aes(color = rare_color)) +   # Draw points
  ggplot2::geom_point(size = 8, data = ~filter(.x, less_than_5_plots == TRUE), color = "yellow") +
  #geom_point(data = ~filter(.x, Species == "versicolor"), shape = 5)
  ggplot2::geom_point(size = 5, aes(color = Nativity)) +   # Draw points
  ggplot2::geom_point(size = 2, data = ~filter(.x, Outside_Plot == TRUE), color = "black") +
  #geom_point(aes(size = 2, shape=21, fill = outside_color)) +   # Draw points
  #{if(any_out)geom_point(size=1, aes(x = out$Scientific_Name, y = out$Cycle), color = out$out_color)} +
  #{if(any_out)geom_point(size=1, x = out$Scientific_Name,
  #                       y = out$Cycle,
  #                       color = out$out_color)} +
  #geom_point(size=1, aes(x = out$Scientific_Name, y = out$Cycle), color = "black") +
  # Draw dashed lines
  ggplot2::labs(title="Check Presence",
       subtitle= (paste0(chk_pres2$Sampling_Frame[1], " Plot ", chk_pres2$Plot_Number[1])),
       caption= (paste0("QA/QC"))) +
  ggplot2::scale_color_manual(values = nativity_colors) +
  #scale_shape_manual(values = c('Women' = 17, 'Men' = 16))
  ggplot2::scale_x_discrete(limits = rev) +
  ggplot2::scale_y_continuous(breaks = integer_breaks(), limits = c(0, max(chk_pres2$Cycle)+1)) +
  ggplot2::coord_flip() +
  #facet_wrap(scales = "free", vars(chk_pres2$split)) +
  ggplot2::theme(strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank())





# QA/QC ----
spp_list_HAVO <- master_spp_list(veg_species_db_full_path, park = "HAVO")

# Check 2021 again after 2023 Olaa QC F1 and Nahuku F14 plots entered:
# Complelte: pacnvegetation::qc_presence_complete(sample_frame = "Olaa")
# Complelte: pacnvegetation::qc_presence_complete(sample_frame = "Nahuku/East Rift")
pacnvegetation::qc_presence_complete(sample_frame = "Mauna Loa")
pacnvegetation::qc_presence_complete(sample_frame = "Kahuku")

# ---- Understory spp consistency chk ---
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = "Olaa",
                                  combine_strata = TRUE,
                                  cycle = c(1,2,3),
                                  plot_number = 11)



# ---- Presence spp consistency chk ---
chk_pres <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                                          sample_frame = "Olaa",
                                          plot_number = 1)


rare <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                                      sample_frame = "Olaa") %>%
  dplyr::group_by(Sampling_Frame, Scientific_Name, Code, Plot_Number) %>%
  dplyr::summarize(observed = n(), .groups = "drop") %>%
  # only count one time if found more than once in a fixed
  mutate(observed = 1) %>%
  dplyr::group_by(Sampling_Frame, Code, Scientific_Name) %>%
  dplyr::summarize(plots_observed = n()) %>%
  # "rare" will be 4 plots_observed or less
  filter(plots_observed < 5) %>%
  mutate(less_than_5_plots = TRUE) %>%
  dplyr::right_join(chk_pres) %>%
  filter(less_than_5_plots == TRUE) %>%
  mutate(out_color = "yellow")

any_rare <- any(!is.na(rare$less_than_5_plots))
rare

chk_pres <- chk_pres %>%
  mutate(Cycle = as.integer(Cycle)) %>%
  dplyr::arrange(Scientific_Name)

chk_pres$Outside_Plot[3] <- TRUE

# find the middle of the data set
df_length <- length(chk_pres$Scientific_Name)
df_middle <- df_length/2

# Which species is in the middle of the dataset
split_at_sp <- chk_pres$Code[df_middle]

# select the row after the last record for that species to split evenly accross species
split_at <- max(which(chk_pres$Code == split_at_sp)) + 1
split_at

chk_pres$split <- "first"
chk_pres$split[split_at:df_length] <- "second"

#chk_pres1 <- chk_pres %>%
#  dplyr::left_join(rare)

# Plot
# A function factory for getting integer y-axis values.
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# Nativity discrete scale Colors:
nativity_colors <- c("Native" = "#1b9e77",
                     "No_Veg" = "grey",
                     "Non-Native" = "#d95f02",
                     "Unknown" = "#7570b3")



out <- chk_pres %>%
  filter(Outside_Plot == TRUE) %>%
  mutate(out_color = "black")
out
any_out <- any(!is.na(chk_pres$Outside_Plot))

chk_pres %>%
ggplot(aes(x= Scientific_Name, y=Cycle, color = Nativity)) +
  ggplot2::geom_segment(aes(x=Scientific_Name,
                   xend=Scientific_Name,
                   y=min(Cycle),
                   yend=max(Cycle)),
               linetype="dashed",
               linewidth=0.1) +
  #{if(any_rare)geom_point(size=8, x = rare$Scientific_Name,
  #                       y = rare$Cycle,
  #                       color = rare$less_than_5_plots)} +
  #geom_point(fill = chk_pres$less_than_5_plots, size=6) +
  ggplot2::geom_point(size=4) +   # Draw points
  #{if(any_out)geom_point(size=1, aes(x = out$Scientific_Name, y = out$Cycle), color = out$out_color)} +
  {if(any_out)geom_point(size=1, x = out$Scientific_Name,
                         y = out$Cycle,
                         color = out$out_color)} +
  #geom_point(size=1, aes(x = out$Scientific_Name, y = out$Cycle), color = "black") +
  # Draw dashed lines
  ggplot2::labs(title="Check Presence",
       subtitle= (paste0(chk_pres$Sampling_Frame[1], " Plot ", chk_pres$Plot_Number[1])),
       caption= (paste0("QA/QC"))) +
  ggplot2::scale_color_manual(values = nativity_colors) +
  #scale_shape_manual(values = c('Women' = 17, 'Men' = 16))
  ggplot2::scale_x_discrete(limits = rev) +
  ggplot2::scale_y_continuous(breaks = integer_breaks(), limits = c(0, max(chk_pres$Cycle)+1)) +
  ggplot2::coord_flip() +
  facet_wrap(scales = "free", vars(chk_pres$split)) +
  ggplot2::theme(strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank())



# Old one for backup:
ggplot(chk_pres, aes(x= Scientific_Name, y=Cycle, color = Nativity)) +
  ggplot2::geom_segment(aes(x=Scientific_Name,
                   xend=Scientific_Name,
                   y=min(Cycle),
                   yend=max(Cycle)),
               linetype="dashed",
               linewidth=0.1) +
  ggplot2::geom_point(aes(fill = less_than_5_plots, size=6)) +
  ggplot2::geom_point(size=4) +   # Draw points
  #{if(any_out)geom_point(size=1, aes(x = out$Scientific_Name, y = out$Cycle), color = out$out_color)} +
  {if(any_out)geom_point(size=1, x = out$Scientific_Name,
                         y = out$Cycle,
                         color = out$out_color)} +
  #geom_point(size=1, aes(x = out$Scientific_Name, y = out$Cycle), color = "black") +
    # Draw dashed lines
  ggplot2::labs(title="Check Presence",
       subtitle= (paste0(chk_pres$Sampling_Frame[1], " Plot ", chk_pres$Plot_Number[1])),
       caption= (paste0("QA/QC"))) +
  ggplot2::scale_color_manual(values = nativity_colors) +
  #scale_shape_manual(values = c('Women' = 17, 'Men' = 16))
  ggplot2::scale_x_discrete(limits = rev) +
  ggplot2::scale_y_continuous(breaks = integer_breaks(), limits = c(0, max(chk_pres$Cycle)+1)) +
  ggplot2::coord_flip() +
  facet_wrap(scales = "free", vars(chk_pres$split)) +
  ggplot2::theme(strip.background = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_blank())








# master_spp_list ----


# Local Path to Veg Spp database
veg_species_db_folder <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
# If only one database in folder, this will grab full path:
veg_species_db_full_path <- list.files(veg_species_db_folder,full.names = TRUE)

look <- master_spp_list(db_path = veg_species_db_full_path, park = "All", presence_matrix = TRUE)

look2 <- look %>%
  select(Code, tail(names(.), 15))

# Create Abbrev Dataframe
Sampling_Frame <- c("Olaa", "Nahuku/East Rift", "Mauna Loa", "Kahuku", "Kaloko-Honokohau",
                    "Kipahulu District", "Haleakala", "Puu Alii", "Kalawao", "Hoolehua",
                    "Tutuila", "Tau", "Guam", "Muchot")
SF_Abbrev <- c("OL", "ER",	"ML",	"KU",	"KH",	"KD",	"HA",	"PA",	"KW",	"HO",	"TT",	"TA",	"GU",	"MU")

Abbrev <- data.frame(Sampling_Frame, SF_Abbrev)

# Code occurrence by Plant Community
ftpc_occ <- pacnvegetation::FilterPACNVeg(data_name = "Presence") %>%
  dplyr::left_join(Abbrev, by = dplyr::join_by(Sampling_Frame)) %>%
  dplyr::mutate(SF = SF_Abbrev) %>%
  dplyr::select(SF, Sampling_Frame, Plot_Number, Cycle, Code) %>%
  # Only count a species once for a fixed plot
  dplyr::group_by(SF, Sampling_Frame, Code, Plot_Number) %>%
  dplyr::summarise(count = dplyr::n_distinct(Code), .groups = "drop") %>%
  # Get total across each plant community per park
  dplyr::group_by(SF, Code) %>%
  dplyr::summarise(plots = dplyr::n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = SF, values_from = plots, values_fill = 0)

# Get master_list from Veg Spp DB using park (Unit_Code)
park <- "All"
master_list <- pacnveg_master_spp_list %>%
  dplyr::arrange(desc(Update_date_park))

if (park == "All") {
  master_list <- pacnveg_master_spp_list %>%
    dplyr::arrange(desc(Update_date_park)) %>%
    dplyr::distinct(Species_ID, .keep_all = TRUE)
  warning("dupicate species records removed, if species is present in more than one park, only the most recently edited record is selected. For example, common name in table may be specific to NPSA and missing Hawaii common name")
} else {
  master_list <- pacnveg_master_spp_list %>%
  dplyr::filter(Park == park)
}

master_list <- master_list %>%
  dplyr::arrange(Taxonomic_Family, Genus, Species) %>%
  dplyr::left_join(ftpc_occ, by = "Code")

if (presence_matrix == TRUE) {
  return(master_list)
}

# get new ftpc occurrence columns as a vector
new_cols <- dplyr::setdiff(names(master_list), names(pacnveg_master_spp_list))
new_cols

master_list2 <- master_list %>%
  dplyr::mutate_at(.vars = new_cols, ~ replace_na(., 0)) %>%
  # Take first common name
  tidyr::separate(col = Park_common_name, into = "common1", sep = ",",
                  extra = "drop", remove = FALSE ) %>%
  dplyr::mutate(LF = tidyr::replace_na(Life_form_park, replace = "_NO LIFEFORM_")) %>%
  dplyr::mutate(presence_rank = rowSums(across(all_of(new_cols)))) #%>%
  #dplyr::mutate(pres_rank_text = stringr::str_pad(presence_rank, 3, pad = "0"))


# Add column name to each value of each occurrence column

# var <- "mpg"
# Doesn't work: mtcars$var
# These both work, but note that what they return is different
# the first is a vector, the second is a data.frame
# mtcars[[var]]   dataframe
# mtcars[var]     vector

for (col in new_cols) {
  master_list2[new_cols][col] <- paste0(col, ":", master_list2[new_cols][[col]])
}

# Unit the new ftpc occurrence columns into one column:
master_list3 <- master_list2 %>%
  tidyr::unite("PC_presence", all_of(new_cols), sep = ", ")


if (length(new_cols) > 1) {
  master_list4 <- master_list3 %>%
    dplyr::mutate(FTPC_pres = paste0(presence_rank, " (", PC_presence, ")"))
} else {
  master_list4 <- master_list3 %>%
    dplyr::mutate(FTPC_pres = paste0(" (", PC_presence, ")"))
}

master_list5 <- master_list4 %>%
  dplyr::mutate(Field_ID = paste0(Scientific_name, " (", Code, ") ",
                                  Taxonomic_Family, " / ", Nativeness, " / ", LF,
                                  " / " , common1, " / ", FTPC_pres, " [syn: ", Synonym, "]")) %>%
  dplyr::arrange(desc(presence_rank), Field_ID) %>%
  dplyr::select(-Species_ID, -TSN, -common1, -Life_form, -Life_cycle, -Omit_in_NPSpecies, -Complete,
                -TSN_park, -PC_presence, -LF)








# figure out why new entries are not showing in spp DB----

# Issue was trying to join by "TSN" apparently some of the new records to not
# have a "TSN" number so just removed and instead just joined by "Species_ID" only
veg_species_db_path <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
veg_species_db <- list.files(veg_species_db_path,full.names = TRUE)
veg_species_db

c_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", veg_species_db)

# Establish connection with access database
c <- DBI::dbConnect(odbc::odbc(), .connection_string = c_string)

  # Download Species info table
tlu_Species <- dplyr::tbl(c, "tlu_Species") %>%
  dplyr::collect()
  names(tlu_Species)

  # Download park specific Species checklists
xref_Park_Species <- dplyr::tbl(c, "xref_Park_Species") %>%
  dplyr::collect()

  # Join Park Species checklists to with the additional species information
pacnveg_master_spp_list2 <- tlu_Species %>%
  dplyr::right_join(xref_Park_Species, by = c("Species_ID"), suffix = c("","_park"))

  DBI::dbDisconnect(conn)



# Kevin Spp Occurrence data request ----
selected_spp <- c(
  "Tibouchina herbacea", "Leptospermum scoparium","Toona ciliata",
  "Pinus luchuensis", "Angiopteris evecta","Sphaeropteris cooperi = (Cyathea cooperi)",
  "Pterolepis glomerata","Elephantopus mollis","Melochia umbellata",
  "Spathodea campanulata","Rhodomyrtus tomentosa",
  "Erigeron karvinskianus","Hedychium gardnerianum","Oxyspora paniculata",
  "Alstonia macrophylla")

# Not_found <- "Leptospermum scoparium" ,"Toona ciliata", "Pinus luchuensis",
# "Angiopteris evecta", "Pterolepis glomerata", "Melochia umbellata", "Rhodomyrtus tomentosa",
# "Oxyspora paniculata", "Alstonia macrophylla"

# 109 species occurrences from veg map:
hi_vegmap_spp <- hi_vegmap_data %>%
  filter(!is.na(lat), !is.na(long)) %>%
  filter(Sci_Name %in% c(selected_spp))

Vegmap <- hi_vegmap_spp %>%
  select(Plot_Code, Event_Date, Family, Sci_Name, ComName, lat, long, GPS_Error)

unique(Vegmap$Sci_Name)

write_csv2(Vegmap, file = paste0("C:/Users/JJGross/Downloads/Vegmap_", Sys.Date() ,".csv"))

# FTPC
names(FilterPACNVeg())
hi_FTPC_spp <- FilterPACNVeg(data_name = "Presence", is_qa_plot = FALSE) %>%
  filter(Unit_Code %in% c("HALE", "HAVO", "KALA", "KAHO")) %>%
  select(-QA_Plot) %>%
  filter(Scientific_Name %in% c(selected_spp))

pts_FTPC <- FilterPACNVeg(data_name = "Events_extra_xy", is_qa_plot = FALSE) %>%
  select(-QA_Plot) %>%
  dplyr::right_join(y = hi_FTPC_spp, by = dplyr::join_by(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number))

unique(hi_FTPC_spp$Scientific_Name)

FTPC <- pts_FTPC %>%
  select(Unit_Code, Community, Sampling_Frame, Cycle, Year,
         Plot_Number, Plot_Type, Start_Lat, Start_Long,
         Scientific_Name, Code, Nativity, Outside_Plot, cf, Certified = Certified.y) %>%
  mutate(Nativity = dplyr::case_when(Scientific_Name == "Angiopteris evecta" ~ "Non-Native",
                               .default = as.character(Nativity)))
write_csv2(FTPC, file = paste0("C:/Users/JJGross/Downloads/FTPC_", Sys.Date() ,".csv"))

# EIPS

test_data <- v_EIPS_prep() %>%
  filter(Unit_Code %in% c("HALE", "HAVO", "KALA", "KAHO")) %>%
  filter(Scientific_Name %in% c(selected_spp))

# Mean Species Cover by inter-station
station_summary <- test_data %>%
dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year,
                Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
                Seg_Length_m, Segs_Per_Station, Meters_Per_Station,
                Code, Scientific_Name, Life_Form, Nativity) %>%
  dplyr::summarize(Actual_Segs = dplyr::n_distinct(Segment),
                   Tot_Station_Cov_Min = sum(Cov_Range_Min),
                   Tot_Station_Cov_Max = sum(Cov_Range_Max)) %>%
  dplyr::mutate(Actual_Meters = Actual_Segs * Seg_Length_m,
                Mean_Seg_Cov_Min = Tot_Station_Cov_Min/Actual_Segs,
                Mean_Seg_Cov_Max = Tot_Station_Cov_Max/Actual_Segs)

ipts_EIPS <- FilterPACNVeg(data_name = "EIPS_image_pts") %>%
  filter(Unit_Code %in% c("HALE", "HAVO", "KALA", "KAHO"))

#Change Year to first year of the Cycle and prep data
ipts_EIPS2 <- ipts_EIPS %>%
  dplyr::group_by(Unit_Code, Sampling_Frame, Cycle) %>%
  dplyr::mutate(Year = min(Year)) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Transect_Number = as.factor(Transect_Number)) %>%
  dplyr::select(-Latitude_Dir, -Longitude_Dir, -GCS, -GPS_Error)

#join to GPS points:
station_summary2 <- station_summary %>%
  dplyr::mutate(Start_Image_Point = as.character(Start_Station_m)) %>%
  dplyr::left_join(ipts_EIPS2, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "Start_Image_Point" = "Image_Point")) %>%
  dplyr::rename(Lat_Start = Latitude,
                Long_Start = Longitude) %>%
  dplyr::mutate(End_Image_Point = as.character(End_Station_m)) %>%
  dplyr::left_join(ipts_EIPS2, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "End_Image_Point" = "Image_Point")) %>%
  dplyr::rename(Lat_End = Latitude,
                Long_End = Longitude) %>%
  dplyr::ungroup()

EIPS_start_stations <- station_summary2 %>%
  select(Unit_Code, Community, Sampling_Frame, Cycle, Year,
         Transect_Number, Transect_Type, Start_Station_m, Meters_Per_Station,
         Scientific_Name, Code, Nativity,
         Start_Image_Point, Lat_Start, Long_Start)

unique(EIPS_start_stations$Scientific_Name)

EIPS <- EIPS_start_stations

write_csv2(EIPS, file = paste0("C:/Users/JJGross/Downloads/EIPS_", Sys.Date() ,".csv"))






# Get labels for herbarium voucher specimens ----
library(tidyverse)

# Load ED plant records that need voucher labels:
gdb <- "C:/Users/JJGross/Downloads/ED_HAVO_2022_new/ED_HAVO_2022_new.gdb"
gdb_layer <- "ED_HAVO_2022"
ED_HAVO_2022 <- sf::read_sf(gdb, gdb_layer)
ED_HAVO_2022_vouchers <- ED_HAVO_2022 %>%
  filter(Specimen == "Yes")
ED_HAVO_2022_vouchers$Code <- stringr::str_extract(ED_HAVO_2022_vouchers$Photo_Taxon, "(?<=\\().*?(?=\\))")


# Load ED plant records that need voucher labels:
veg_species_db_path <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
veg_species_db <- list.files(veg_species_db_path,full.names = TRUE)
veg_species_db
pacnveg_master_spp_list <- read_spp_db(veg_species_db) %>%
  filter(Park == "HAVO")

# Specimens already known to park
join1 <- ED_HAVO_2022_vouchers %>%
  filter(New_Species == "No") %>%
  dplyr::left_join(y = pacnveg_master_spp_list, by = "Code")

# Specimens new to park (Veg Spp Database)
join2 <- ED_HAVO_2022_vouchers %>%
  filter(New_Species != "No") %>%
  dplyr::left_join(y = pacnveg_master_spp_list, by = c("New_sp_name" = "Scientific_name"))







# Veg Spp Database ----

dbpath <- "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db/PACN_veg_species_list_20230810.accdb"
pacnveg_master_spp_list <- read_vegspp_db(dbpath)





#' Load and Query The PACN Veg Species Database
#' Vital Signs > 05_focal_terr_plant_communities > Data > Database > Veg_species_db
#'
#' @param db_paths Database path (downloaded from sharepoint to location on computer)
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' dbpath <- "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db/PACN_veg_species_list_20230810.accdb"
#' pacnveg_master_spp_list <- read_vegspp_db(dbpath)
#'
#' }
read_vegspp_db <- function(db_paths) {
  conn_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", db_paths)

  # Establish connection with access database
  conn <- DBI::dbConnect(odbc::odbc(), .connection_string = conn_string)

  # Download Species info table
  tlu_Species <- dplyr::tbl(conn, "tlu_Species") %>%
    dplyr::collect()
  names(tlu_Species)

  # Download park specific Species checklists
  xref_Park_Species <- dplyr::tbl(conn, "xref_Park_Species") %>%
    dplyr::collect()

  # Join Park Species checklists to with the additional species information
  pacnveg_master_spp_list <- tlu_Species %>%
    dplyr::right_join(xref_Park_Species, by = c("Species_ID", "TSN"), suffix = c("","_park"))

  return(pacnveg_master_spp_list)

}



