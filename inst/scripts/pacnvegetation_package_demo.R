# Script to test and demo some pacnvegetation R package functions:
# ----------------------------------------------------Jacob Gross 04/08/2026

#renv::init()
#renv::install("jakegross808/pacn-veg-package")

install.packages("devtools")
devtools::install_github("jakegross808/pacn-veg-package")
library(pacnvegetation)
library(tidyverse)

# Read latest dataset:
path_file_info <- file.info(list.files("data/WritePACNVeg/", full.names = T))
latest_folder <- rownames(path_file_info)[which.max(path_file_info$mtime)]
latest_folder
LoadPACNVeg(data_path = latest_folder,
            data_source = "file")
#LoadPACNVeg() call currently gives warning message related to parsing issues. 
# Code still works, looking into why 'Update_Date, Start_Date, Year, and Cycle parsers are not matching column names. 

# Get raw data ----

# Use the following csv flatfiles to access raw datasets
names(FilterPACNVeg())


# Select a sampling frame and explore the understory dataset of that sampling frame:
# Understory - Summarize ----
var_samp_frame <- "Olaa"

# the raw understory data
understory <- FilterPACNVeg(data_name = "Understory",
                              sample_frame = var_samp_frame)

?summarize_understory

# Will give warning that it expects plant grouping
summarize_understory(sample_frame = var_samp_frame)

# No plant grouping (all species/lifeforms etc. combined into one group):
under_hits <- summarize_understory(sample_frame = var_samp_frame, plant_grouping = "None")
head(under_hits)

# Can combine understory strata (0-1m and 1-2m) into one stratum if desired:
under_hits <- summarize_understory(plant_grouping = "None",
                                   sample_frame = var_samp_frame,
                                   combine_strata = TRUE)
head(under_hits)

# split by native/non-native cover:
under_native <- summarize_understory(plant_grouping = "Nativity",
                                     sample_frame = var_samp_frame,
                                     combine_strata = TRUE)
head(under_native)

# split by life form:
under_lifeform <- summarize_understory(plant_grouping = "Life_Form",
                                     sample_frame = var_samp_frame,
                                     combine_strata = TRUE)
head(under_lifeform)

# split by species
under_spp <- summarize_understory(plant_grouping = "Species",
                                       sample_frame = var_samp_frame,
                                       combine_strata = TRUE)
head(under_spp)

# Understory - Plot ----

# Generic ggplot examples
under_hits_box <- under_hits %>%
  ggplot2::ggplot(aes(x = Year, y = Cover)) +
  ggplot2::geom_boxplot()
under_hits_box

under_hits_col <- under_hits %>%
  ggplot2::ggplot(aes(x = Year, y = Cover)) +
  geom_col()
under_hits_col

under_hits_bar <- under_hits %>%
  ggplot(aes(x = Year, y = Cover)) +
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", linewidth = 1, size = 1)
under_hits_bar


# functions to graph bar stats in HTML reports:
pacnvegetation::v_cover_bar_stats(plant_grouping = "Nativity",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = FALSE)

pacnvegetation::v_cover_bar_stats(plant_grouping = "Life_Form",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE)

pacnvegetation::v_cover_bar_stats(plant_grouping = "Life_Form",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE,
                                  cycle = c(1,2))

pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  species_filter = "Metrosideros polymorpha",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = FALSE,
                                  cycle = c(1,2,3))


# ---- Detailed plot graphs can be useful for veg crew QAQC ---
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE,
                                  cycle = c(1,2,3),
                                  plot_number = 1)

# Plot 1 has been visited in 2023 so can check against other cycles:
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE,
                                  cycle = c(1,2,3),
                                  plot_number = 1)

# Change within 1 plot
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = var_samp_frame,
                                  combine_strata = TRUE,
                                  paired_change = TRUE,
                                  cycle = c(1,2,3),
                                  measurement = "Chg_Prior",
                                  plot_number = 1)

under_spp_chg <- summarize_understory(plant_grouping = "Species",
                                  sample_frame = var_samp_frame,
                                  combine_strata = TRUE,
                                  paired_change = TRUE,
                                  cycle = c("1","2"))


pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = var_samp_frame,
                                  combine_strata = TRUE,
                                  paired_change = TRUE,
                                  cycle = c("1","2"),
                                  measurement = "Chg_Prior",
                                  plot_number = "1")

pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = var_samp_frame,
                                  combine_strata = TRUE,
                                  paired_change = FALSE,
                                  measurement = "Cover",
                                  plot_number = "1")

pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = var_samp_frame,
                                  combine_strata = TRUE,
                                  paired_change = FALSE,
                                  measurement = "Cover")

# Ohia (fixed plots and rotationals)----
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  species_filter = "Metrosideros polymorpha",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE,
                                  cycle = c(1,2,3))

# Ohia (change in understory cover - just using fixed plots)----
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  species_filter = "Metrosideros polymorpha",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE,
                                  paired_change = TRUE,
                                  cycle = c(1,2,3))

# Presence ----

presence <- FilterPACNVeg(data_name = "Presence",
                              sample_frame = "Haleakala")


?FilterPACNVeg
understory_spp_trends_rank(sample_frame = "Puu Alii", paired_change = TRUE, top_n = 10, rank_by = "positive", remove_nativity = "Native")
pacnvegetation:::understory_spp_trends_rank(sample_frame = "Puu Alii", paired_change = FALSE, remove_nativity = "Native")
pacnvegetation:::understory_spp_trends_rank(sample_frame = "Puu Alii", paired_change = TRUE, remove_nativity = "Native")

pacnvegetation::understory_spp_trends_rank(sample_frame = "Puu Alii", paired_change = FALSE, top_n = 5, rank_by = "positve", remove_nativity = "Native")

understory_spp_trends_rank(sample_frame = "Puu Alii", paired_change = TRUE, top_n = 10, rank_by = "negative")
