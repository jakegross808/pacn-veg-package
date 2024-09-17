library(pacnvegetation)
library(tidyverse)
library(leaflet)


#--- 1. Read latest cache ------------------------------------------------------

#** Download latest FTPC and EIPS data first!*

# Write/Read csv from pacnvegetation package:
pacnveg_cache_path <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/R_WritePACNVeg"

# Read
path_file_info <- file.info(list.files(pacnveg_cache_path, full.names = T))
latest_folder <- rownames(path_file_info)[which.max(path_file_info$mtime)]

LoadPACNVeg(data_path = latest_folder,
            data_source = "file")

names(FilterPACNVeg())



FilterPACNVeg(data_name = "Presence") |>
  select(Sampling_Frame) |>
  distinct()

FilterPACNVeg(data_name = "Presence") |>
  select(Sampling_Frame_English) |>
  distinct()

FilterPACNVeg(data_name = "EIPS_data") |>
  select(Sampling_Frame) |>
    distinct()


#--- 2. variable specification -------------------------------------------------

var_sframe <- "Mauna Loa"

#nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15, #fixed
#                  46, 49, 51, 52, 54, 55, 56, 58, #2021 rotational
#                  24, 26, #2010 rotational
#                  31, 32, 33, 34, 35, 38, 41, 45) #2015 rotational



# Field Maps ----


## First Download -----------------------------------------------------------

#FTPC_HAVO_2022 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/HAVO_2022_FTPC_Sampling_Points_Photos/FeatureServer/30"
Plants_KALA_2024v2 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KALA_2024_VEG_Sampling_Plant_Photos_v2/FeatureServer/2"

var_photo_layers <- c("Plants_KALA_2024v2")

field_maps_data <- download_agol2(
  photo_layers = var_photo_layers,
  temp_dest = "C:/Users/JJGross/Downloads/Check_Photos/",
  #only_staff = FALSE,
  test_run = TRUE
)

field_maps_data <- download_agol2(
  photo_layers = var_photo_layers,
  temp_dest = "C:/Users/JJGross/Downloads/Check_Photos/",
  test_run = FALSE
)

## 2nd-n Downloads -----------------------------------------------------------
Plants_KALA_2024v2 <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/KALA_2024_VEG_Sampling_Plant_Photos_v2/FeatureServer/2"

var_photo_layers <- c("Plants_KALA_2024v2")

field_maps_data <- download_agol2(
  photo_layers = var_photo_layers,
  temp_dest = "C:/Users/JJGross/Downloads/Check_Photos/",
  master_spreadsheet_folder = "C:/Users/JJGross/Downloads/Check_Photos/Plants_KALA_2024_Spreadsheet",
  #after_date_filter = var_after_date, # This argument needs to be removed
  test_run = TRUE
)

field_maps_data <- download_agol2(
  photo_layers = var_photo_layers,
  temp_dest = "C:/Users/JJGross/Downloads/Check_Photos/",
  master_spreadsheet_folder = "C:/Users/JJGross/Downloads/Check_Photos/Plants_KALA_2024_Spreadsheet",
  test_run = FALSE
)

# FTPC ----
## status check -----------------------------------------------------------

# FTPC certification status
qaqc_status <- pacnvegetation::FilterPACNVeg(data_name = "Events_extra_QAQC")

qaqc_status <- pacnvegetation::FilterPACNVeg(
  data_name = "Events_extra_QAQC",
  sample_frame = var_sframe)

needs_verified <- qaqc_status |>
  filter(Verified == FALSE)
needs_verified

needs_certified <- qaqc_status |>
  filter(Certified == FALSE)
needs_certified
nrow(needs_certified)




## have photos been exported?----------------------------------------------------


## qc_presence ---------------------------------------------------------------

qc_presence_details <- pacnvegetation::qc_presence_complete(
  all_records = TRUE,
  sample_frame = var_sframe)

qc_presence <- pacnvegetation::qc_presence_complete(
  all_records = FALSE,
  sample_frame = var_sframe)

## qc_sampling_lifeform-------------------------------------------------------
qc_samp_lifeform <- pacnvegetation::FilterPACNVeg(data_name = "SmWoody",
                                              sample_frame = var_sframe)

# Need specific example to test next time issue is encountered.

## search trees/understory datasheets for a species------------------------------
chk <- qc_sp_datasheets(sample_frame = var_sframe,
                 #plot_number = 51,
                 #species_code = "CIBSP.",
                 silent = FALSE)

## list of presence--------------------------------------------------------------
all_pres <- pacnvegetation::FilterPACNVeg(data_name = "Presence") |>
  filter(Sampling_Frame == var_sframe) #|>
  #filter(Plot_Number %in% nahuku_plots)

all_pres_count <- all_pres |>
  group_by(Scientific_Name, Sampling_Frame, Year) |>
  summarise(count = n())


## Presence Dot Plots-----------------------------------------------------------
var_plot_numbers <- c(46:60)
var_sframe <- "Mauna Loa"
save_folder_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output"

for (x in var_plot_numbers) {
  pacnvegetation:::qc_spp_pres_dot_plot(sample_frame = var_sframe,
                       plot_number = x,
                       save_folder = save_folder_var)
}

pacnvegetation:::qc_spp_pres_dot_plot(sample_frame = var_sframe,
                                      plot_number = 14,
                                      save_folder = save_folder_var)

## Understory spp consistency chk ----------------------------------------------
var_plot_number <- 14
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = var_sframe,
                                  combine_strata = FALSE,
                                  cycle = c(1,2,3),
                                  plot_number = var_plot_number)

chk_cover <- pacnvegetation::summarize_understory(combine_strata = TRUE,
                                     plant_grouping = "Species",
                                     sample_frame = var_sframe)

path_var <- paste0("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output/", var_sframe, "/")
filename_var_v_cover <- paste0("v_cover_plot_spp_", var_plot_number, ".png")
filename_var_v_cover
ggsave(filename = filename_var_v_cover, path = path_var, height = 10, width = 15)
#---

v_cover_bar_spp_plot(sample_frame = "Puu Alii", crosstalk_filters = TRUE, crosstalk_group = "spp_plot1")

## LG Trees --------------------------------------
# Get count of trees per plot/quad

lg_trees <- FilterPACNVeg(data_name = "LgTrees",
                          sample_frame = var_sframe,
                          plot_number = var_plot_number)

lg_trees_no_boles <- lg_trees |>
  filter(is.na(DBH_Bole)) |>
  select(Plot_Number, Quad, DBH, Status, Vigor, Boles) |>
  mutate(list_boles = "") |>
  mutate(n_boles = 1)

lg_trees_boles <- lg_trees |>
  filter(!is.na(DBH_Bole)) |>
  group_by(Plot_Number, Quad, DBH, Status, Vigor, Boles) |>
  summarise(list_boles = paste(DBH_Bole, collapse =","), n_boles = length(DBH_Bole))

lg_trees2 <- bind_rows(lg_trees_no_boles, lg_trees_boles)

lg_trees2_quad <- lg_trees2 |>
  group_by(Plot_Number, Quad) |>
  summarise(tree_count = n())



# EIPS ---------

var_sframe

# EIPS certification status

EIPS_events_other <- FilterPACNVeg(data_name = "Events_extra_other_EIPS",
                                   sample_frame = var_sframe)

EIPS_events_other <- EIPS_events_other |>
  filter(Sampling_Frame == var_sframe)

EIPS_needs_verified_eips <- EIPS_events_other |>
  filter(Verified == FALSE)
EIPS_needs_verified_eips

EIPS_needs_certified <- EIPS_events_other |>
  filter(Certified == FALSE)
EIPS_needs_certified
nrow(EIPS_needs_certified)

# EIPS Segment complete check --------------------------------------

# v_EIPS_prep assumes all segments with no records was visited and no invasives were
# detected - this is incorrect assumption if transect was not fully completed (ie all segments monitored)
# In database for each segment, there is a box that can be checked if segment was not monitored.
# This column still needs dplyr::select() and pulled through in the utils.R file for ReadEIPS()

EIPS_check <- v_EIPS_prep(sample_frame = var_sframe, cycle = 3)

EIPS_segment_check <- EIPS_check |>
  group_by(Unit_Code, Sampling_Frame, Year, Cycle, Transect_Number, Tran_Length_m) |>
  summarize(segs_w_spp = n_distinct(Start_m))

# EIPS Presence -- spp consistency chk --------------------------------------
# TURN THIS INTO pacnvegetation qc_ FUNCTION

var_trans_num <- 3

eips_data_check <- pacnvegetation::FilterPACNVeg(data_name = "EIPS_data")|>
  pull(Sampling_Frame) |>
  unique()
eips_data_check

eips_data <- pacnvegetation::FilterPACNVeg(data_name = "EIPS_data") |>
  mutate(Sampling_Frame = case_when(Sampling_Frame == "ʻŌlaʻa" ~ "Olaa",
                                    .default = as.character(Sampling_Frame))) |>
  mutate(Sampling_Frame = case_when(Sampling_Frame == "Nāhuku/East Rift" ~ "Nahuku/East Rift",
                                    .default = as.character(Sampling_Frame))) |>
  filter(Sampling_Frame == var_sframe)


eips_presence <- eips_data |>
  select(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code) |>
  distinct()

eips_pres_trans_num <- eips_presence |>
  filter(Transect_Number == var_trans_num)

eips_rare <- eips_presence |>
  group_by(Sampling_Frame, Nativity, Scientific_Name, Code, Transect_Number) %>%
  summarize(observed = n(), .groups = "drop") %>%
  # only count one time if found more than once in a fixed
  mutate(observed = 1) %>%
  group_by(Sampling_Frame, Code, Scientific_Name) %>%
  summarize(transects_observed = n()) %>%
  # "rare" will be 4 plots_observed or less
  filter(transects_observed < 5) %>%
  mutate(less_than_5_transects = TRUE) %>%
  right_join(eips_pres_trans_num) %>%
  filter(less_than_5_transects == TRUE)

# Join rare species flags to Presence
eips_pres_trans_num1 <- eips_pres_trans_num %>%
  left_join(eips_rare)

eips_pres_trans_num2 <- eips_pres_trans_num1 %>%
  mutate(Cycle = as.integer(Cycle)) %>%
  arrange(Scientific_Name)

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
  function(d) d %>% filter_(condition)
}

select_out <- function(condition){
  function(d) d %>% filter_(condition)
}

graph_out <- eips_pres_trans_num2 %>%
  ggplot(aes(x= Scientific_Name, y=Cycle)) +
  geom_segment(aes(x=Scientific_Name,
                   xend=Scientific_Name,
                   y=min(Cycle),
                   yend=max(Cycle),
                   color = Nativity),
               linetype="dashed",
               linewidth=0.5) +
  # Draw points
  geom_point(size = 8, data = ~filter(.x, less_than_5_transects == TRUE), color = "yellow") +
  geom_point(size = 5, aes(color = Nativity)) +
  #geom_point(size = 2, data = ~filter(.x, Outside_Plot == TRUE), color = "black") +
  labs(title="Check Presence",
       subtitle= (paste0(eips_pres_trans_num2$Sampling_Frame[1], " Transect ", eips_pres_trans_num2$Transect_Number[1])),
       caption= (paste0("QA/QC"))) +
  scale_color_manual(values = nativity_colors) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(0, max(eips_pres_trans_num2$Cycle)+1)) + #breaks = integer_breaks(),
  coord_flip() +
  #facet_wrap(scales = "free", vars(eips_pres_trans_num2$split)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(aspect.ratio=6) #9
graph_out

var_folder_sframe <-var_sframe
if (var_sframe == "Nahuku/East Rift") {
  var_folder_sframe <- "Nahuku"
}

path_var <- paste0("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output/", var_folder_sframe, "/")
filename_var <- paste0("trans_", str_pad(var_trans_num, 2, pad = "0"), "_spp_pres-dot.png")
filename_var
ggsave(filename = filename_var, path = path_var, height = 10, width = 5)

# ---- EIPS Cover Class frequency ------------------------------------------

eips_data <- pacnvegetation::FilterPACNVeg(data_name = "EIPS_data") |>
  mutate(Sampling_Frame = case_when(Sampling_Frame == "ʻŌlaʻa" ~ "Olaa",
                                    .default = as.character(Sampling_Frame))) |>
  mutate(Sampling_Frame = case_when(Sampling_Frame == "Nāhuku/East Rift" ~ "Nahuku/East Rift",
                                    .default = as.character(Sampling_Frame))) |>
  filter(Sampling_Frame == var_sframe)

# Will need to make segs_per_tran calculation more robust
eips_cover_freq <- eips_data |>
  filter(Transect_Number == var_trans_num) |>
  select(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Segment, Cover_Class) |>
  distinct() |>
  mutate(segs_pres = 1) |>
  group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Cover_Class) |>
  summarize(segs_per_tran = sum(segs_pres)) |>
  mutate(Cover_Class = case_when(Cover_Class == "OUT" ~ "0",
                                 .default = as.character(Cover_Class)))

OUT <- eips_cover_freq |>
  filter(Cover_Class >= 0) |>
  mutate(OUT = sum(segs_per_tran))|>
  select(-Cover_Class, -segs_per_tran) |>
  distinct()
g0p <- eips_cover_freq |>
  filter(Cover_Class >= 1) |>
  mutate(`0` = sum(segs_per_tran))|>
  select(-Cover_Class, -segs_per_tran) |>
  distinct()
g1p <- eips_cover_freq |>
  filter(Cover_Class >= 2) |>
  mutate(`1` = sum(segs_per_tran))|>
  select(-Cover_Class, -segs_per_tran)|>
  distinct()
g5p <- eips_cover_freq |>
  filter(Cover_Class >= 3) |>
  mutate(`5` = sum(segs_per_tran))|>
  select(-Cover_Class, -segs_per_tran)|>
  distinct()
g10p <- eips_cover_freq |>
  filter(Cover_Class >= 4) |>
  mutate(`10` = sum(segs_per_tran))|>
  select(-Cover_Class, -segs_per_tran)|>
  distinct()
g25p <- eips_cover_freq |>
  filter(Cover_Class >= 5) |>
  mutate(`25` = sum(segs_per_tran))|>
  select(-Cover_Class, -segs_per_tran)|>
  distinct()
g50p <- eips_cover_freq |>
  filter(Cover_Class >= 6) |>
  mutate(`50` = sum(segs_per_tran))|>
  select(-Cover_Class, -segs_per_tran)|>
  distinct()
g75p <- eips_cover_freq |>
  filter(Cover_Class >= 7) |>
  mutate(`75` = sum(segs_per_tran))|>
  select(-Cover_Class, -segs_per_tran)|>
  distinct()

eips_add_cover <- eips_cover_freq %>%
  select(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code) |>
  distinct() |>
  #left_join(OUT) |>
  left_join(g0p) |>
  left_join(g1p) |>
  left_join(g5p) |>
  left_join(g10p) |>
  left_join(g25p) |>
  left_join(g50p) |>
  left_join(g75p)

eips_add_cover1 <- eips_add_cover |>
  pivot_longer(cols = `0`:`75`, names_to = "cover_greater_than", values_to = "segs") |>
  mutate(freq = segs/50) #|>
  #dplyr::mutate(freq = replace_na(freq, 0))

library(viridis)

acc <- eips_add_cover1 |>
  mutate(Cycle = as.factor(Cycle)) |>
  mutate(cover_greater_than = as_factor(cover_greater_than)) |>
  ggplot(aes(x=cover_greater_than,
             y= freq,
             group = Cycle,
             color = Cycle))+
  #geom_ribbon(aes(ymin=L95CI/100, ymax=U95CI/100, fill=Life_form), alpha=0.4,colour=NA)+
  geom_line(linewidth = 1)+
  geom_point(size = 2) +
  #scale_y_continuous(breaks = seq(0, 1, .1)) + #, limits = c(0, 1) +
  labs(x="% Cover", y=expression(paste("Frequency")))+
  ggtitle(paste(eips_add_cover1$Sampling_Frame, "Transect", eips_add_cover1$Transect_Number))+
  scale_color_viridis(discrete = TRUE, direction = -1) +
  theme(plot.title = element_text(size=14, face="bold", vjust=1, lineheight=0.8))+
  theme(axis.title.x=element_text(size=12, vjust=-0.2))+
  theme(axis.text.x=element_text(angle=0, size=11, vjust=0.5))+
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5))+
  scale_x_discrete(labels = function(x) paste0(">", x)) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                     limits = c(0, 1),
                     breaks=seq(0,1,.2)) +
  facet_wrap(~Scientific_Name)

  #scale_x_discrete(expand = c(0.02, 0.02),drop=FALSE)
acc

var_folder_sframe <-var_sframe
if (var_sframe == "Nahuku/East Rift") {
  var_folder_sframe <- "Nahuku"
}

path_var <- paste0("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output/", var_folder_sframe, "/")
filename_var <- paste0("trans_", str_pad(var_trans_num, 2, pad = "0"), "_spp-cover_x_freq.png")
filename_var
ggsave(filename = filename_var, path = path_var, height = 10, width = 20)
