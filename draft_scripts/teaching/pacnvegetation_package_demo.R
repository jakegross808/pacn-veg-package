# Run selected lines in start.R first

library(tidyverse)

# Get raw data ----

#pacnvegetation::FilterPACNVeg()

names(FilterPACNVeg())


# Understory - Summarize ----
var_samp_frame <- "Olaa"

understory <- FilterPACNVeg(data_name = "Understory",
                              sample_frame = var_samp_frame) |>
  filter(Scientific_Name == "Metrosideros polymorpha")

?summarize_understory

summarize_understory(sample_frame = var_samp_frame)

under_hits <- summarize_understory(sample_frame = var_samp_frame, plant_grouping = "None")

under_hits <- summarize_understory(plant_grouping = "None",
                                   sample_frame = var_samp_frame,
                                   combine_strata = TRUE)

under_native <- summarize_understory(plant_grouping = "Nativity",
                                     sample_frame = var_samp_frame,
                                     combine_strata = TRUE)

under_lifeform <- summarize_understory(plant_grouping = "Life_Form",
                                     sample_frame = var_samp_frame,
                                     combine_strata = TRUE)

under_spp <- summarize_understory(plant_grouping = "Species",
                                       sample_frame = var_samp_frame,
                                       combine_strata = TRUE)

# Understory - Plot ----

under_hits_box <- under_hits %>%
  ggplot(aes(x = Year, y = Cover)) +
  geom_boxplot()
under_hits_box

under_hits_col <- under_hits %>%
  ggplot(aes(x = Year, y = Cover)) +
  geom_col()
under_hits_col

pacnvegetation::v_cover_bar_stats(plant_grouping = "None",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE)

pacnvegetation::v_cover_bar_stats(plant_grouping = "Nativity",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE)

pacnvegetation::v_cover_bar_stats(plant_grouping = "Life_Form",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE)

pacnvegetation::v_cover_bar_stats(plant_grouping = "Life_Form",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE,
                                  cycle = c(1,2))

pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  species_filter =
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE,
                                  cycle = c(1,2))

# ---- Good one for veg crew to utilize each plot ---
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

qc <- pacnvegetation::qc_presence_complete(all_records = TRUE, sample_frame = "Haleakala")

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

# Ohia ----
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  species_filter = "Metrosideros polymorpha",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE,
                                  cycle = c(1,2,3))
# Presence ----

presence <- FilterPACNVeg(data_name = "Presence",
                              sample_frame = "Haleakala")





?FilterPACNVeg
