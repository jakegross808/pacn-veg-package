# To run test use:
library(pacnvegetation)
library(tidyverse)
library(patchwork)


LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_1_2021_20211208.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
            cache = TRUE, force_refresh = FALSE)

cover_ <- summarize_understory(paired_change = FALSE,
                                              sample_frame = "Olaa",
                                              plant_grouping = "None")

cover_nativity <- summarize_understory(paired_change = FALSE,
                                       sample_frame = "Haleakala",
                                       plant_grouping = "Nativity")

cover_lifeform <- summarize_understory(paired_change = FALSE,
                                       sample_frame = "Olaa",
                                       plant_grouping = "Life_Form")

cover_species <- summarize_understory(paired_change = FALSE,
                                       sample_frame = "Olaa",
                                       plant_grouping = "Species")

chg_cover_ <- summarize_understory(paired_change = TRUE,
                               sample_frame = "Olaa",
                               plant_grouping = "None")

chg_cover_nativity <- summarize_understory(paired_change = TRUE,
                                       sample_frame = "Olaa",
                                       plant_grouping = "Nativity")

chg_cover_lifeform <- summarize_understory(paired_change = TRUE,
                                       sample_frame = "Olaa",
                                       plant_grouping = "Life_Form")

chg_cover_species <- summarize_understory(paired_change = TRUE,
                                      sample_frame = "Olaa",
                                      plant_grouping = "Species")


cover_nativity <- cover_nativity %>%
  group_by(Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year)) %>%
  mutate(Year = as.factor(Year)) %>%
  ungroup()

unknown_cover <- cover_nativity %>%
  filter(Nativity == "Unknown" & Cover > 0)

unk_cover_tot <- unknown_cover %>%
  pull(Cover) %>%
  sum()

cover_nativity <- cover_nativity %>%
  filter(Stratum != "No_Veg",
         Nativity != "Unknown")
print(paste0(round(unk_cover_tot,2), "% cover of species with unknown Nativity removed"))

# Nativity discrete scale Colors:
nativity_colors <- c("Native" = "#1b9e77", "No_Veg" = "grey", "Non-Native" = "#d95f02", "Unknown" = "#7570b3")

# add stats
cover_nat_stat <- add_stats(cover_nativity, Unit_Code, Sampling_Frame, Cycle, Year, Stratum, Nativity)

# sample size calculation for text
sample_size <- cover_nat_stat %>%
  select(Year, NPLOTS) %>%
  distinct() %>%
  mutate(Text = paste0(Year, " [n = ", NPLOTS, "]")) %>%
  pull(Text) %>%
  paste(collapse = ", ")
sample_size

#........BAR YEARLY MEANS----
p2 <- cover_nat_stat %>%
  filter(Parameter == "Cover") %>%
  ggplot(aes(x = Year, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Mean % Cover") +
  facet_grid(Stratum ~ Nativity, space = "free_y") +
  scale_fill_manual(values = nativity_colors, limits = force) +
  xlab("Year") +
  theme(legend.position="none") +
  labs(caption = sample_size)
p2
