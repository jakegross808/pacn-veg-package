library(pacnvegetation)
library(tidyverse)

#--- 1. Read latest cache ------------------------------------------------------

#**  Make sure to download latest FTPC and EIPS data using start.R  *

names(FilterPACNVeg())

all_presence <- FilterPACNVeg(data_name = "Presence")

all_presence_QAQC2 <- FilterPACNVeg(data_name = "Presence", is_qa_plot = TRUE) |>
  group_by(Cycle, Sampling_Frame, Plot_Number) |>
  summarise(n = n_distinct(Plot_Number))

all_cover <- FilterPACNVeg(data_name = "Understory")

all_EIPS <- FilterPACNVeg(data_name = "EIPS_data")

all_shrubbelt <- FilterPACNVeg(data_name = "SmWoody")

spp_extra <- FilterPACNVeg(data_name = "Species_extra")

all_LgTrees <- FilterPACNVeg(data_name = "LgTrees")

#--- 2. variable specification -------------------------------------------------

var_sframe <- "Puu Alii"

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
n_FTPC_table

n_FTPC_table_sf <- n_FTPC_table |>
  dplyr::filter(Sampling_Frame == var_sframe)
n_FTPC_table_sf

# QAQC Plots:
n_FTPC_table_QAQC <- FilterPACNVeg(data_name = "Events_extra_other", is_qa_plot = TRUE) |>
  dplyr::group_by(Sampling_Frame, Cycle) |>
  dplyr::mutate(Year = min(Year)) |>
  dplyr::group_by(Sampling_Frame, Plot_Type, Cycle, Year) |>
  dplyr::summarise(n = n())


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
                 species_code = "CYNDAC",
                 silent = FALSE)

chk_qaqc_plot <- qc_sp_datasheets(sample_frame = var_sframe,
                        #plot_number = 51,
                        species_code = "METPOL1",
                        is_qa_plot = TRUE,
                        silent = FALSE)

## list of presence--------------------------------------------------------------
all_pres <- pacnvegetation::FilterPACNVeg(data_name = "Presence") |>
  filter(Sampling_Frame == var_sframe) #|>
  #filter(Plot_Number %in% nahuku_plots)

all_pres_count <- all_pres |>
  dplyr::group_by(Scientific_Name, Sampling_Frame, Year) |>
  summarise(count = n())

richness <- all_pres|>
  dplyr::group_by(Sampling_Frame, Year, Plot_Number) |>
  summarise(count = n())

richness_summary_graph <- richness |>
  mutate(Year = as.integer(Year)) |>
  ### Plot the data
  ggplot2::ggplot(aes(x = Year, y = count)) +
  # Plot all city data here in gray lines
  geom_line() +
  geom_point() +
  facet_grid(~Plot_Number)

richness_summary_graph

## Understory species -----------------------------------------------------------
spp_of_interest <- "Melicope sessilis"

SOI_plots <- all_cover |>
  filter(Scientific_Name == spp_of_interest) |>
  pull(Plot_Number) |>
  unique()

# 1. Initialize an empty list to store your plots
plot_list <- list()

for (x in SOI_plots) {
  df <- pacnvegetation::v_cover_bar_stats(species_filter = spp_of_interest,
                                    plant_grouping = "Species",
                                    sample_frame = var_sframe,
                                    combine_strata = FALSE,
                                    cycle = c(1,2,3),
                                    remove_unknown = FALSE,
                                    plot_number = x) + ggplot2::labs(title=paste(var_sframe, "Plot #", x))
  # Store the plot in the list
  plot_list <- plot_list |>
    append(df)
}

plot_list[[1]]$

order(plot_list, decreasing = TRUE)
# 3. Display all plots together in a grid
# 'grobs = plot_list' passes the whole list, 'ncol' specifies columns
gridExtra::grid.arrange(grobs = plot_list, )

## Presence Dot Plots-----------------------------------------------------------
var_plot_numbers <- c(1:15, 46:60) #1:15, 46:60
var_sframe
save_folder_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2024 Certification/R_output"

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


  path_var <- paste0("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2024 Certification/R_output/", var_sframe, "/")
  filename_var_v_cover <- paste0("plot_", x, "_cover_spp.png")
  filename_var_v_cover
  ggsave(filename = filename_var_v_cover, path = path_var, height = 10, width = 15)
}


var_plot_number <- c(8)

pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = var_sframe,
                                  combine_strata = FALSE,
                                  cycle = c(1,2,3),
                                  remove_unknown = FALSE,
                                  plot_number = var_plot_number) + ggplot2::labs(title=paste(var_sframe, "Plot #", var_plot_number))


path_var <- paste0("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output/", var_sframe, "/")
filename_var_v_cover <- paste0("plot_", var_plot_number, "_cover_spp.png")
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


pacnvegetation::understory_spp_trends_rank(sample_frame = "Olaa", remove_nativity = "Non-Native", paired_change = FALSE, top_n = 10)
pacnvegetation::understory_spp_trends_rank(sample_frame = "Mauna Loa", remove_nativity = "Native", paired_change = FALSE)
pacnvegetation::understory_spp_trends_rank(sample_frame = "Mauna Loa", remove_nativity = "Non-Native", paired_change = TRUE)
pacnvegetation::understory_spp_trends_rank(sample_frame = "Mauna Loa", remove_nativity = "Native", paired_change = TRUE)

v_cover_bar_spp_plot(sample_frame = "Olaa", crosstalk_filters = TRUE, crosstalk_group = "spp_plot1")

## Shrub spp consistency chk ----------------------------------------------

smwoody <- FilterPACNVeg(data_name = "SmWoody",
                         sample_frame = var_sframe,
                         is_qa_plot = FALSE) |>
  dplyr::mutate(Year = as.character(Year))

smwoody_grouped <- smwoody |>
  dplyr::summarise(count = sum(Count),
                   .by = c(Scientific_Name, Code, Nativity, Life_Form,
                   Sampling_Frame, Year, Plot_Number, Sample_Area,
                   Status, LF_Sm_Woody, DBH, Length, Rooting))

var_plot_number
smwoody_grouped |>
  mutate(count = as.integer(count)) |>
  dplyr::filter(Plot_Number == var_plot_number) |>
  ggplot2::ggplot(aes(x=Year, y=count, fill = Status)) +
  ggplot2::scale_fill_manual(values = c("Live" = "dark green",
                                        "Dead" = "tan")) +
  geom_bar(stat='identity') +
  facet_grid(Scientific_Name ~ Sample_Area) +
  ggplot2::ggtitle(paste0("Small Woody Density - ", unique(smwoody_grouped$Sampling_Frame),
                          " - Plot ", var_plot_number))

for (x in var_plot_numbers) {
  graph_out <- smwoody_grouped |>
    mutate(count = as.integer(count)) |>
    dplyr::filter(Plot_Number == x) |>
    ggplot2::ggplot(aes(x=Year, y=count, fill = Status)) +
    ggplot2::scale_fill_manual(values = c("Live" = "dark green",
                                          "Dead" = "tan")) +
    geom_bar(stat='identity') +
    facet_grid(Scientific_Name ~ Sample_Area) +
    ggplot2::ggtitle(paste0("Small Woody Density - ", unique(smwoody_grouped$Sampling_Frame),
                            " - Plot ", x))
  path_var <- paste0("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2024 Certification/R_output/", var_sframe, "/")
  filename_var_v_cover <- paste0("plot_", x, "_smallwoody_spp.png")
  filename_var_v_cover
  ggsave(filename = filename_var_v_cover, path = path_var, height = 25, width = 10)
  print(paste("saved plot", x))
}




## LG Trees --------------------------------------
lg_trees <- FilterPACNVeg(data_name = "LgTrees",
                          sample_frame = var_sframe,
                          is_qa_plot = FALSE) |>
  dplyr::mutate(Life_Form = dplyr::case_when(is.na(Life_Form) & Caudex_Length == 999.0 ~ "Tree",
                                             is.na(Life_Form) & Caudex_Length != 999.0 ~ "Tree Fern",
                                            .default = as.character(Life_Form))) |>
  #filter(Plot_Number == var_plot_number) |> ###**delete!!!!!!!!!!!*
  dplyr::filter(Cycle %in% c(1,2,3)) |>
  dplyr::mutate(Quad = paste("Quad", as.character(Quad)))

BA <- lg_trees |>
  #filter(Plot_Number == var_plot_number) |> ###**delete!!!!!!!!!!!*
  dplyr::filter(Life_Form == "Tree") |>
  dplyr::mutate(DBH_Bole = dplyr::case_when(Boles == 1 ~ DBH,
                              .default = as.numeric(DBH_Bole))) |>
  dplyr::mutate(DBH_Bole = dplyr::case_when(is.na(DBH_Bole) & !is.na(Measurement_Type) ~ Measurement,
                                          .default = as.numeric(DBH_Bole))) |>
  dplyr::mutate(BA_m2 = (DBH_Bole^2)*0.00007854) |>
  dplyr::group_by(Sampling_Frame, Cycle, Plot_Number, Quad, Status, Large_Woody_ID) |>
  dplyr::summarise(BA_m2_per_tree = sum(BA_m2)) |>
  dplyr::group_by(Sampling_Frame, Cycle, Plot_Number, Quad, Status) |>
  dplyr::summarise(BA_m2_per_quad = sum(BA_m2_per_tree))

var_plot_number
BA |>
  filter(Plot_Number == var_plot_number) |>
  ggplot2::ggplot(aes(x=Cycle, y=BA_m2_per_quad, fill = Status)) +
    ggplot2::scale_fill_manual(values = c("Live" = "dark green",
                                  "Dead" = "tan")) +
  geom_bar(stat='identity') +
  facet_wrap(~Quad) +
  ggplot2::ggtitle(paste0("Tree BA - ", unique(BA$Sampling_Frame),
                 " - Plot ", var_plot_number))

for (x in var_plot_numbers) {
  graph_out <- BA |>
    filter(Plot_Number == x) |>
    ggplot2::ggplot(aes(x=Cycle, y=BA_m2_per_quad, fill = Status)) +
    ggplot2::scale_fill_manual(values = c("Live" = "dark green",
                                          "Dead" = "tan")) +
    geom_bar(stat='identity') +
    facet_wrap(~Quad) +
    ggplot2::ggtitle(paste0("Tree BA - ", unique(BA$Sampling_Frame),
                            " - Plot ", x))

  path_var <- paste0("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2024 Certification/R_output/", var_sframe, "/")
  filename_BA <- paste0("plot_", x, "_TreeBA.png")
  filename_BA
  ggsave(filename = filename_BA, path = path_var, height = 10, width = 15)
  print(paste("saved plot", x))
}


lg_tree_count_all <- lg_trees |>
  dplyr::filter(Life_Form == "Tree") |>
  dplyr::distinct(Sampling_Frame, Cycle, Plot_Number, Status, Large_Woody_ID) |>
  dplyr::mutate(count = 1) |>
  dplyr::group_by(Sampling_Frame, Cycle, Status) |>
  dplyr::summarise(tree_count_all = sum(count))

lg_tree_count_all |>
  ggplot2::ggplot(aes(x=Cycle, y=tree_count_all)) +
  geom_bar(stat='identity') +
  facet_wrap(~Status) +
  ggplot2::ggtitle(paste0("Tree Count - ", unique(lg_tree_count_all$Sampling_Frame)))


# Per Plot ----
#Get count of trees per plot/quad
var_plot_numbers <- c(1)
var_plot_number <- 60
var_plot_number

# This includes QA plots for some reason???
lg_trees <- FilterPACNVeg(data_name = "LgTrees",
                         sample_frame = var_sframe,
                         plot_number = var_plot_number) |>
  dplyr::mutate(Year = as.character(Year)) |>
  dplyr::mutate(Quad = paste("Quad", as.character(Quad))) |>
  dplyr::mutate(Life_Form = dplyr::case_when(is.na(Life_Form) & Caudex_Length == 999.0 ~ "Tree",
                                             is.na(Life_Form) & Caudex_Length != 999.0 ~ "Tree Fern",
                                             .default = as.character(Life_Form)))

lg_tree_count_quad <- lg_trees |>
  dplyr::filter(Life_Form == "Tree") |>
  dplyr::distinct(Sampling_Frame, Year, Plot_Number, Quad, Status, Large_Woody_ID) |>
  dplyr::mutate(count = 1) |>
  dplyr::group_by(Sampling_Frame, Year, Plot_Number, Quad, Status) |>
  dplyr::summarise(tree_count_quad = sum(count))

lg_tree_count_plot <- lg_tree_count_quad |>
  dplyr::group_by(Sampling_Frame, Year, Plot_Number, Status) |>
  dplyr::summarise(tree_count_plot = sum(tree_count_quad))
lg_tree_count_plot

lg_trees |>
  dplyr::filter(Plot_Number == var_plot_number) |>
  dplyr::group_by(Sampling_Frame, Cycle, Plot_Number, Life_Form) |>
  dplyr::summarise(n = n_distinct(Large_Woody_ID))



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

lg_tree_count_plot

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
  filter(Year %in% c("2012", "2017", "2024")) |>
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
  filter(Year %in% c("2012", "2017", "2024")) |>
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
var_transect_numbers <- c(51:66) #1:15,
var_transect_numbers
var_sframe
save_folder_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2024 Certification/R_output"


# If single graph needed:
qc_EIPS_spp_pres_dot_plot(sample_frame =var_sframe,
                          transect_number = 51)

# Make graphs for all transects listed in var_transect_numbers:
for (x in var_transect_numbers) {
  pacnvegetation:::qc_EIPS_spp_pres_dot_plot(sample_frame = var_sframe,
                                             transect_number = x,
                                             save_folder = save_folder_var)
  }




# ---- EIPS Cover Class frequency ------------------------------------------
var_transect_numbers
var_sframe
save_folder_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2024 Certification/R_output"


# If single graph needed:
EIPS_cover_x_freq(sample_frame =var_sframe,
                          transect_number = 51)

# Make graphs for all transects listed in var_transect_numbers:
for (x in var_transect_numbers) {
  pacnvegetation:::EIPS_cover_x_freq(sample_frame = var_sframe,
                                             transect_number = x,
                                             save_folder = save_folder_var)
}


# Final Species List ----
# Local Path to Veg Spp database
veg_species_db_folder <-  "C:/LocalDocuments/Databases_copied_local/Veg_species_db"
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


pacnvegetation::v_EIPS_map_interstation()

all_presence <- FilterPACNVeg(data_name = "Presence") |>
  left_join(Events_extra_xy) |>
  filter(Scientific_Name %in% target_species)

all_cover <- FilterPACNVeg(data_name = "Understory") |>
  filter(Scientific_Name %in% target_species)

all_EIPS <- FilterPACNVeg(data_name = "EIPS_data") |>
  filter(Scientific_Name %in% target_species)


all_locations
