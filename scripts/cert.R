library(pacnvegetation)
library(tidyverse)

#--- 1. Read latest cache ------------------------------------------------------

#**  Make sure to download latest FTPC and EIPS data using start.R  *
LoadPACNVeg(force_refresh = FALSE, eips_paths = "foo")

names(FilterPACNVeg())

all_presence <- FilterPACNVeg(data_name = "Presence")

all_cover <- FilterPACNVeg(data_name = "Understory")

all_EIPS <- FilterPACNVeg(data_name = "EIPS_data")

all_shrubbelt <- FilterPACNVeg(data_name = "SmWoody")



#--- 2. variable specification -------------------------------------------------

var_sframe <- "Kaloko-Honokohau"

#nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15, #fixed
#                  46, 49, 51, 52, 54, 55, 56, 58, #2021 rotational
#                  24, 26, #2010 rotational
#                  31, 32, 33, 34, 35, 38, 41, 45) #2015 rotational


# Photos / Field Maps ----

#** Use 'scripts/agol_fieldmaps_photo_download.R' **
#*For bi-weekly backups and initial photo/data checks
#*
#** Use 'scripts/agol_fieldmaps_photo_finalize.R' **
#*For final photo downloading/processing/database table linking



# FTPC ----
## status check -----------------------------------------------------------

# FTPC certification status
qaqc_status <- pacnvegetation::FilterPACNVeg(data_name = "Events_extra_QAQC")

qaqc_status <- pacnvegetation::FilterPACNVeg(
  data_name = "Events_extra_QAQC",
  sample_frame = var_sframe)

needs_verified <- qaqc_status |>
  dplyr::filter(Verified == FALSE)
needs_verified
nrow(needs_verified)

needs_certified <- qaqc_status |>
  dplyr::filter(Certified == FALSE)
needs_certified
nrow(needs_certified)




## have photos been exported?----------------------------------------------------


##plot sample size -------------------------------------------------------
n_FTPC_table <- FilterPACNVeg(data_name = "Events_extra_other") |>
  dplyr::group_by(Sampling_Frame, Cycle) |>
  dplyr::mutate(Year = min(Year)) |>
  dplyr::group_by(Sampling_Frame, Plot_Type, Cycle, Year) |>
  dplyr::summarise(n = n())

n_FTPC_table_sf <- n_FTPC_table |>
  dplyr::filter(Sampling_Frame == var_sframe)


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
  dplyr::group_by(Scientific_Name, Sampling_Frame, Year) |>
  summarise(count = n())


## Presence Dot Plots-----------------------------------------------------------
var_plot_numbers <- c(1:10, 21:28)
var_sframe
save_folder_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output"

for (x in var_plot_numbers) {
  pacnvegetation:::qc_FTPC_spp_pres_dot_plot(sample_frame = var_sframe,
                       plot_number = x,
                       save_folder = save_folder_var)
}

pacnvegetation::qc_FTPC_spp_pres_dot_plot(sample_frame = var_sframe,
                                      plot_number = 1,
                                      save_folder = save_folder_var)

## Understory spp consistency chk ----------------------------------------------
for (x in var_plot_numbers) {
  pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                    sample_frame = var_sframe,
                                    combine_strata = FALSE,
                                    cycle = c(1,2,3),
                                    remove_unknown = FALSE,
                                    plot_number = x) + ggplot2::labs(title=paste(var_sframe, "Plot #", x))


  path_var <- paste0("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output/", var_sframe, "/")
  filename_var_v_cover <- paste0("v_cover_plot_spp_", x, ".png")
  filename_var_v_cover
  ggsave(filename = filename_var_v_cover, path = path_var, height = 10, width = 15)
}


var_plot_number <- c(56)

pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = var_sframe,
                                  combine_strata = FALSE,
                                  cycle = c(1,2,3),
                                  remove_unknown = FALSE,
                                  plot_number = var_plot_number) + ggplot2::labs(title=paste(var_sframe, "Plot #", var_plot_number))


path_var <- paste0("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output/", var_sframe, "/")
filename_var_v_cover <- paste0("v_cover_plot_spp_", var_plot_number, ".png")
filename_var_v_cover
ggsave(filename = filename_var_v_cover, path = path_var, height = 10, width = 15)
#---
chk_cover <- pacnvegetation::FilterPACNVeg(data_name = "")
chk_cover

chk_cover <- pacnvegetation::summarize_understory(combine_strata = TRUE,
                                                   plant_grouping = "Species",
                                                   sample_frame = var_sframe)
chk_cover

chk_cover2 <- pacnvegetation::summarize_understory(combine_strata = TRUE,
                                                  plant_grouping = "Species",
                                                  sample_frame = var_sframe)
chk_cover2


pacnvegetation::understory_spp_trends_rank(sample_frame = "Mauna Loa", remove_nativity = "Non-Native", paired_change = FALSE, top_n = 10)
pacnvegetation::understory_spp_trends_rank(sample_frame = "Mauna Loa", remove_nativity = "Native", paired_change = FALSE)
pacnvegetation::understory_spp_trends_rank(sample_frame = "Mauna Loa", remove_nativity = "Non-Native", paired_change = TRUE)
pacnvegetation::understory_spp_trends_rank(sample_frame = "Mauna Loa", remove_nativity = "Native", paired_change = TRUE)

#v_cover_bar_spp_plot(sample_frame = "Olaa", crosstalk_filters = TRUE, crosstalk_group = "spp_plot1")

## LG Trees --------------------------------------
lg_trees_fixed <- FilterPACNVeg(data_name = "LgTrees",
                          sample_frame = var_sframe,
                          is_qa_plot = FALSE) |>
  dplyr::mutate(Year = as.character(Year)) |>
  dplyr::filter(Cycle %in% c(2,3)) |>
  dplyr::mutate(Quad = paste("Quad", as.character(Quad))) |>
  dplyr::filter(Plot_Number %in% 1:15)

BA <- lg_trees_fixed |>
  dplyr::filter(Life_Form == "Tree") |>
  dplyr::mutate(DBH_Bole = dplyr::case_when(is.na(DBH_Bole) ~ DBH,
                              .default = as.numeric(DBH_Bole))) |>
  dplyr::mutate(BA_m2 = (DBH_Bole^2)*0.00007854) |>
  dplyr::group_by(Sampling_Frame, Year, Plot_Number, Quad, Status, Large_Woody_ID) |>
  dplyr::summarise(BA_m2_per_tree = sum(BA_m2)) |>
  dplyr::group_by(Sampling_Frame, Year, Plot_Number, Quad, Status) |>
  dplyr::summarise(BA_m2_per_quad = sum(BA_m2_per_tree))

var_plot_number <- 1
BA |>
  filter(Plot_Number == var_plot_number) |>
  ggplot2::ggplot(aes(x=Year, y=BA_m2_per_quad, fill = Status)) +
    ggplot2::scale_fill_manual(values = c("Live" = "dark green",
                                  "Dead" = "tan")) +
  geom_bar(stat='identity') +
  facet_wrap(~Quad) +
  ggplot2::ggtitle(paste0("Tree BA - ", unique(BA$Sampling_Frame),
                 " - Plot ", var_plot_number))

path_var <- paste0("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output/", var_sframe, "/")
filename_BA <- paste0("TreeBA_", var_plot_number, ".png")
filename_BA
ggsave(filename = filename_BA, path = path_var, height = 10, width = 15)






lg_tree_count_all <- lg_trees_fixed |>
  dplyr::filter(Life_Form == "Tree") |>
  dplyr::distinct(Sampling_Frame, Year, Plot_Number, Status, Large_Woody_ID) |>
  dplyr::mutate(count = 1) |>
  dplyr::group_by(Sampling_Frame, Year, Status) |>
  dplyr::summarise(tree_count_all = sum(count))

lg_tree_count_all |>
  ggplot2::ggplot(aes(x=Year, y=tree_count_all)) +
  geom_bar(stat='identity') +
  facet_wrap(~Status) +
  ggplot2::ggtitle(paste0("Tree Count - ", unique(lg_tree_count_all$Sampling_Frame)))


# Per Plot ----
#Get count of trees per plot/quad
var_plot_numbers <- c(1)
var_plot_number <- 1
var_plot_number

# This includes QA plots for some reason???
lg_trees <- FilterPACNVeg(data_name = "LgTrees",
                         sample_frame = var_sframe,
                         plot_number = var_plot_number) |>
  dplyr::mutate(Year = as.character(Year)) |>
  dplyr::mutate(Quad = paste("Quad", as.character(Quad)))

lg_tree_count_quad <- lg_trees |>
  dplyr::filter(Life_Form == "Tree") |>
  dplyr::distinct(Sampling_Frame, Year, Plot_Number, Quad, Status, Large_Woody_ID) |>
  dplyr::mutate(count = 1) |>
  dplyr::group_by(Sampling_Frame, Year, Plot_Number, Quad, Status) |>
  dplyr::summarise(tree_count_quad = sum(count))

lg_tree_count_plot <- lg_tree_count_quad |>
  dplyr::group_by(Sampling_Frame, Year, Plot_Number, Status) |>
  dplyr::summarise(tree_count_plot = sum(tree_count_quad))

lg_tree_count_quad |>
  filter(Status == "Dead") |>
  ggplot2::ggplot(aes(x=Year, y=tree_count_quad)) +
  geom_bar(stat='identity', fill="tan") +
  facet_wrap(~Quad) +
  ggplot2::ggtitle(paste0("Dead Trees - ", unique(lg_tree_count_quad$Sampling_Frame),
                 " - Plot ", unique(lg_tree_count_quad$Plot_Number)))

lg_tree_count_plot |>
  filter(Status == "Dead") |>
  ggplot2::ggplot(aes(x=Year, y=tree_count_plot)) +
  geom_bar(stat='identity', fill="tan") +
  ggplot2::ggtitle(paste0("Dead Trees - ", unique(lg_tree_count_plot$Sampling_Frame),
                 " - Plot ", unique(lg_tree_count_plot$Plot_Number)))

lg_tree_count_quad |>
  filter(Status == "Live") |>
  ggplot2::ggplot(aes(x=Year, y=tree_count_quad)) +
  geom_bar(stat='identity', fill="forest green") +
  facet_wrap(~Quad) +
  ggplot2::ggtitle(paste0("Live Trees - ", unique(lg_tree_count_quad$Sampling_Frame),
                 " - Plot ", unique(lg_tree_count_quad$Plot_Number)))

lg_tree_count_plot |>
  filter(Status == "Live") |>
  ggplot2::ggplot(aes(x=Year, y=tree_count_plot)) +
  geom_bar(stat='identity', fill="forest green") +
  ggplot2::ggtitle(paste0("Dead Trees - ", unique(lg_tree_count_plot$Sampling_Frame),
                 " - Plot ", unique(lg_tree_count_plot$Plot_Number)))


Boles_DBH <- lg_trees |>
  dplyr::filter(Life_Form == "Tree") |>
  dplyr::mutate(DBH_Bole = dplyr::case_when(is.na(DBH_Bole) ~ DBH,
                              .default = as.numeric(DBH_Bole))) |>
  dplyr::mutate(DBH_Bole = dplyr::case_when(is.na(DBH_Bole & is.na(DBH)) ~ Measurement,
                              .default = as.numeric(DBH_Bole))) |>
  dplyr::mutate(Year = as.character(Year)) |>
  dplyr::mutate(Year = dplyr::case_when(QA_Plot == TRUE ~ paste0(Year, "_Q"),
                          .default = as.character(Year))) |>
  dplyr::arrange(Year, Quad, DBH_Bole)



# Bole DBH histogram
Boles_DBH |>
  filter(Year %in% c("2010", "2015", "2021")) |>
  #filter(Year %in% c("2016", "2022")) |>
  filter(Status == "Live") |>
  ggplot2::ggplot(aes(x=DBH_Bole, color=Year, fill=Year)) +
  geom_histogram(position="dodge", alpha = 0.5)+
  #geom_vline(data=lg_trees3, aes(xintercept=grp.mean, color=Year),
  #           linetype="dashed")+
  ggplot2::theme(legend.position="top") +
  facet_wrap(~Quad) +scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  ggplot2::scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# Bole DBH Density Plot
Boles_DBH |>
  filter(Year %in% c("2010", "2015", "2021")) |>
  #filter(Year %in% c("2016", "2022")) |>
  filter(Status == "Live") |>
  dplyr::mutate(Quad = paste("Quad", as.character(Quad))) |>
  #filter(text %in% c("Almost No Chance", "About Even", "Probable", "Almost Certainly")) %>%
  ggplot2::ggplot( aes(x=DBH_Bole, color=Year, fill=Year)) +
  geom_density(alpha=0.6) +
  facet_wrap(~Quad) +scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  ggplot2::scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  ggplot2::ggtitle(paste0("Trees - ", unique(Boles_DBH$Sampling_Frame), " - Plot ", unique(Boles_DBH$Plot_Number))) +
  ggplot2::xlab("DBH") +
  facet_wrap(~Quad)

# Need to bring unique number for tree individual from database import for this!
Boles_DBH_list <- Boles_DBH |>
  select(Year, Quad, Status, DBH_Bole) |>
  pivot_wider(names_from = Year,
              values_from = DBH_Bole)



# EIPS ---------

var_sframe

## transect sample size -------------------------------------------------------
n_EIPS_table <- FilterPACNVeg(data_name = "Events_extra_other_EIPS") |>
  dplyr::group_by(Sampling_Frame, Cycle) |>
  dplyr::mutate(Year = min(Year)) |>
  dplyr::group_by(Sampling_Frame, Transect_Type, Cycle, Year) |>
  dplyr::summarise(n = n())

n_EIPS_table_sf <- n_EIPS_table |>
  dplyr::filter(Sampling_Frame == var_sframe)

# EIPS certification status

EIPS_events_other <- FilterPACNVeg(data_name = "Events_extra_other_EIPS",
                                   sample_frame = var_sframe)

EIPS_events_other <- EIPS_events_other |>
  dplyr::filter(Sampling_Frame == var_sframe)

EIPS_needs_verified_eips <- EIPS_events_other |>
  dplyr::filter(Verified == FALSE)
EIPS_needs_verified_eips
nrow(EIPS_needs_verified_eips)

EIPS_needs_certified <- EIPS_events_other |>
  dplyr::filter(Certified == FALSE)
EIPS_needs_certified
nrow(EIPS_needs_certified)

# EIPS Segment complete check --------------------------------------

# v_EIPS_prep: segments with no records (NA) are assumed to be free of non-natives. However,
# sometime transects are not fully completed. Two scenarios - First, if incomplete at end of transect (and no one
# entered blank data in database) then 'Null' is applied and correctly ignored. Alternatively,
# if blank data entered by creating segment in database (ie - someone pages through segment tabs) then
# NA gets applied incorrectly resulting in NA which assumee no non-natives - but when really there is no data for segment).

EIPS_check <- v_EIPS_prep(sample_frame = var_sframe)

EIPS_segment_check <- EIPS_check |>
  dplyr::group_by(Unit_Code, Sampling_Frame, Year, Cycle, Transect_Number, Tran_Length_m) |>
  dplyr::summarize(segs_w_spp = n_distinct(Start_m))

# EIPS Presence -- spp consistency chk --------------------------------------


## Presence Dot Plots-----------------------------------------------------------
var_transect_numbers <- c(1:15, 46:60)
var_transect_numbers
var_sframe
save_folder_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output"


# If single graph needed:
qc_EIPS_spp_pres_dot_plot(sample_frame =var_sframe,
                          transect_number = 1)

# Make graphs for all transects listed in var_transect_numbers:
for (x in var_transect_numbers) {
  pacnvegetation:::qc_EIPS_spp_pres_dot_plot(sample_frame = var_sframe,
                                             transect_number = x,
                                             save_folder = save_folder_var)
  }




# ---- EIPS Cover Class frequency ------------------------------------------
var_transect_numbers <- c(1:15, 46:60)
var_sframe
save_folder_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output"


# If single graph needed:
EIPS_cover_x_freq(sample_frame =var_sframe,
                          transect_number = 2)

# Make graphs for all transects listed in var_transect_numbers:
for (x in var_transect_numbers) {
  pacnvegetation:::EIPS_cover_x_freq(sample_frame = var_sframe,
                                             transect_number = x,
                                             save_folder = save_folder_var)
}


# Final Species List ----
# Local Path to Veg Spp database
veg_species_db_folder <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
# If only one database in folder, this will grab full path:
veg_species_db_full_path <- list.files(veg_species_db_folder,full.names = TRUE)
veg_species_db_full_path
# Get raw data from Veg Species Database:
raw_spp_data <- read_spp_db(veg_species_db_full_path)

# Get master species list for a park (with ID_Field for field maps):
spp_list_master <- master_spp_list(veg_species_db_full_path, park = "HAVO")




#--- Target Species Locations----------------------------------------------------


# Target Species
target_species <- c("Arundina graminifolia", "Spathoglottis plicata", "Phaius tankervilleae")



#**  Make sure to download latest FTPC and EIPS data using start.R  *
LoadPACNVeg(force_refresh = FALSE, eips_paths = "foo")
names(FilterPACNVeg())

Events_extra_xy <- FilterPACNVeg(data_name = "Events_extra_xy")

EIPS_image_pts <- FilterPACNVeg(data_name = "EIPS_image_pts") |>
  mutate(Year = as.factor(Year))

EIPS_prep <- pacnvegetation::v_EIPS_prep() |>
  filter(Scientific_Name %in% target_species) |>
  dplyr::mutate(Start_Image_Point = as.character(Start_Station_m)) #|>
  dplyr::left_join(EIPS_image_pts, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "Start_Image_Point" = "Image_Point"))


pacnvegetation::v_EIPS_map_interstation3()

all_presence <- FilterPACNVeg(data_name = "Presence") |>
  left_join(Events_extra_xy) |>
  filter(Scientific_Name %in% target_species)

all_cover <- FilterPACNVeg(data_name = "Understory") |>
  filter(Scientific_Name %in% target_species)

all_EIPS <- FilterPACNVeg(data_name = "EIPS_data") |>
  filter(Scientific_Name %in% target_species)


all_locations
