# Run selected lines in start.R first

library(tidyverse)

# Get raw data ----

#pacnvegetation::FilterPACNVeg()

names(FilterPACNVeg())


# Understory - Summarize ----
understory <- FilterPACNVeg(data_name = "Understory",
                              sample_frame = "Haleakala")

?summarize_understory

summarize_understory(sample_frame = "Haleakala")

under_hits <- summarize_understory(sample_frame = "Haleakala", plant_grouping = "None")

under_hits <- summarize_understory(plant_grouping = "None",
                                   sample_frame = "Haleakala",
                                   combine_strata = TRUE)

under_native <- summarize_understory(plant_grouping = "Nativity",
                                     sample_frame = "Haleakala",
                                     combine_strata = TRUE)

under_lifeform <- summarize_understory(plant_grouping = "Life_Form",
                                     sample_frame = "Haleakala",
                                     combine_strata = TRUE)

under_spp <- summarize_understory(plant_grouping = "Species",
                                       sample_frame = "Haleakala",
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
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE)

pacnvegetation::v_cover_bar_stats(plant_grouping = "Nativity",
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE)

pacnvegetation::v_cover_bar_stats(plant_grouping = "Life_Form",
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE)

pacnvegetation::v_cover_bar_stats(plant_grouping = "Life_Form",
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE,
                                  cycle = c(1,2))

pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE,
                                  cycle = c(1,2))

# ---- Good one for veg crew to utilize each plot ---
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE,
                                  cycle = c(1,2,3),
                                  plot_number = 3)

# Plot 1 has been visited in 2023 so can check against other cycles:
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE,
                                  cycle = c(1,2,3),
                                  plot_number = 1)

?v_cover_bar_stats

# Change within 1 plot
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE,
                                  paired_change = TRUE,
                                  cycle = c(1,2,3),
                                  measurement = "Chg_Prior",
                                  plot_number = 1)

under_spp_chg <- summarize_understory(plant_grouping = "Species",
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE,
                                  paired_change = TRUE,
                                  cycle = c("1","2"))


pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE,
                                  paired_change = TRUE,
                                  cycle = c("1","2"),
                                  measurement = "Chg_Prior",
                                  plot_number = "1")

pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = "Haleakala",
                                  combine_strata = TRUE,
                                  paired_change = FALSE,
                                  measurement = "Cover",
                                  plot_number = "1")

under_spp_chg3 <- summarize_understory(plant_grouping = "Species",
                                      sample_frame = "Haleakala",
                                      combine_strata = TRUE,
                                      paired_change = TRUE,
                                      cycle = c("1","2"),
                                      plot_number = "3")

plot_under_spp_chg3 <- under_spp_chg3 %>%
  filter(!is.na(Chg_Prior)) %>%
  mutate(Scientific_Name = fct_reorder(Scientific_Name, desc(Chg_Prior))) %>%
  ggplot(aes(x = Scientific_Name, y = Chg_Prior)) +
  coord_flip() +
  geom_col()
plot_under_spp_chg3
plot_under_spp_chg2
plot_under_spp_chg1



pacnvegetation::v_cover_bar_spp_plot(sample_frame = "Haleakala", crosstalk_group = "grp")


?any_vars
# Presence ----

presence <- FilterPACNVeg(data_name = "Presence",
                              sample_frame = "Haleakala")





?FilterPACNVeg
