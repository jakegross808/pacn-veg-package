# Run selected lines in start.R first

library(tidyverse)

# Get raw data ----

#pacnvegetation::FilterPACNVeg()

names(FilterPACNVeg())


# Understory - Summarize ----
var_samp_frame <- "Puu Alii"

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


pacnvegetation::v_cover_bar_stats(plant_grouping = "None",
                                  sample_frame = paste(var_samp_frame),
                                  combine_strata = TRUE)

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
understory_spp_trends_rank(sample_frame = "Puu Alii", paired_change = TRUE, top_n = 10, rank_by = "positive", remove_nativity = "Native")
pacnvegetation:::understory_spp_trends_rank(sample_frame = "Puu Alii", paired_change = FALSE, remove_nativity = "Native")
pacnvegetation:::understory_spp_trends_rank(sample_frame = "Puu Alii", paired_change = TRUE, remove_nativity = "Native")

pacnvegetation::understory_spp_trends_rank(sample_frame = "Puu Alii", paired_change = FALSE, top_n = 5, rank_by = "positve", remove_nativity = "Native")

understory_spp_trends_rank(sample_frame = "Puu Alii", paired_change = TRUE, top_n = 10, rank_by = "negative")
