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

#--- 2. variable specification -------------------------------------------------

sframe <- "Nahuku/East Rift"

#nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15, #fixed
#                  46, 49, 51, 52, 54, 55, 56, 58, #2021 rotational
#                  24, 26, #2010 rotational
#                  31, 32, 33, 34, 35, 38, 41, 45) #2015 rotational


#--- 3. status check -----------------------------------------------------------

# certification status

qaqc_status <- pacnvegetation::FilterPACNVeg(
  data_name = "Events_extra_QAQC",
  sample_frame = sframe)

needs_verified <- qaqc_status |>
  filter(Verified == FALSE)
needs_verified

needs_certified <- qaqc_status |>
  filter(Certified == FALSE)
needs_certified
nrow(needs_certified)

# have photos been exported?----------------------------------------------------


# 4. qc_presence ---------------------------------------------------------------

qc_presence_details <- pacnvegetation::qc_presence_complete(
  all_records = TRUE,
  sample_frame = sframe)

qc_presence <- pacnvegetation::qc_presence_complete(
  all_records = FALSE,
  sample_frame = sframe)

# 5. qc_sampling_lifeform-------------------------------------------------------
qc_samp_lifeform <- pacnvegetation::FilterPACNVeg(data_name = "SmWoody",
                                              sample_frame = sframe)

# Need specific example to test next time issue is encountered.

# search trees/understory datasheets for a species------------------------------
chk <- qc_sp_datasheets(sample_frame = sframe,
                 #plot_number = 51,
                 #species_code = "CIBSP.",
                 silent = FALSE)

# list of presence--------------------------------------------------------------
all_pres <- pacnvegetation::FilterPACNVeg(data_name = "Presence") |>
  filter(Sampling_Frame == sframe) |>
  filter(Plot_Number %in% nahuku_plots)

all_pres_count <- all_pres |>
  group_by(Scientific_Name, Sampling_Frame, Year) |>
  summarise(count = n())

# Presence -- spp consistency chk --------------------------------------
# TURN THIS INTO pacnvegetation FUNCTION

plot_number_variable <- 58

chk_pres <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                                          sample_frame = sframe,
                                          plot_number = plot_number_variable)


rare <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                                      sample_frame = sframe) %>%
  group_by(Sampling_Frame, Scientific_Name, Code, Plot_Number) %>%
  summarize(observed = n(), .groups = "drop") %>%
  # only count one time if found more than once in a fixed
  mutate(observed = 1) %>%
  group_by(Sampling_Frame, Code, Scientific_Name) %>%
  summarize(plots_observed = n()) %>%
  # "rare" will be 4 plots_observed or less
  filter(plots_observed < 5) %>%
  mutate(less_than_5_plots = TRUE) %>%
  right_join(chk_pres) %>%
  filter(less_than_5_plots == TRUE)

# Join rare species flags to Presence
chk_pres1 <- chk_pres %>%
  left_join(rare)

chk_pres2 <- chk_pres1 %>%
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

graph_out <- chk_pres2 %>%
  ggplot(aes(x= Scientific_Name, y=Cycle)) +
  geom_segment(aes(x=Scientific_Name,
                   xend=Scientific_Name,
                   y=min(Cycle),
                   yend=max(Cycle),
                   color = Nativity),
               linetype="dashed",
               linewidth=0.5) +
  # Draw points
  geom_point(size = 8, data = ~filter(.x, less_than_5_plots == TRUE), color = "yellow") +
  geom_point(size = 5, aes(color = Nativity)) +
  geom_point(size = 2, data = ~filter(.x, Outside_Plot == TRUE), color = "black") +
  labs(title="Check Presence",
       subtitle= (paste0(chk_pres2$Sampling_Frame[1], " Plot ", chk_pres2$Plot_Number[1])),
       caption= (paste0("QA/QC"))) +
  scale_color_manual(values = nativity_colors) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(0, max(chk_pres2$Cycle)+1)) + #breaks = integer_breaks(),
  coord_flip() +
  #facet_wrap(scales = "free", vars(chk_pres2$split)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(aspect.ratio=6) #9
graph_out

path_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output/Nahuku/"
filename_var <- paste0("spp_pres_plot-dot_", plot_number_variable, ".png")
filename_var
ggsave(filename = filename_var, path = path_var, height = 10, width = 5)

# ---- Understory spp consistency chk ------------------------------------------
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = sframe,
                                  combine_strata = FALSE,
                                  cycle = c(1,2,3),
                                  plot_number = plot_number_variable)

chk_cover <- pacnvegetation::summarize_understory(combine_strata = TRUE,
                                     plant_grouping = "Species",
                                     sample_frame = sframe)

path_var <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/2021-2022 Certification/R_output/Nahuku/"
filename_var_v_cover <- paste0("v_cover_plot_spp_", plot_number_variable, ".png")
filename_var_v_cover
ggsave(filename = filename_var_v_cover, path = path_var, height = 10, width = 15)


v_cover_bar_spp_plot(sample_frame = "Nahuku/East Rift", crosstalk_filters = TRUE, crosstalk_group = "spp_plot1")

# LG Trees --------------------------------------
# Get count of trees per plot/quad

lg_trees <- FilterPACNVeg(data_name = "LgTrees",
                          sample_frame = sframe,
                          plot_number = plot_number_variable)

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



