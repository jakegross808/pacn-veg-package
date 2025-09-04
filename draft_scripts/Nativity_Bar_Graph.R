# To run test use:
library(pacnvegetation)
library(tidyverse)
library(tidytext)


LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_1_2021_20220120.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
            cache = TRUE, force_refresh = FALSE)

look <- FilterPACNVeg("Understory", park = "HAVO", cycle = 2)
look <- FilterPACNVeg("Understory", sample_frame = "Haleakala", cycle = 2)

chk <- look %>%
  #filter(Plot_Number == 10) %>%
  filter(Stratum == "Low") %>%
  dplyr::group_by(Sampling_Frame, Cycle, Dead, Scientific_Name, Code, Nativity) %>%
  summarise(hits = n())



v_cover_plot_bar_nativity(sample_frame = "Haleakala", paired_change = TRUE, param = "Chg_Prior")

v_cover_plot_bar_nativity(sample_frame = "Haleakala", paired_change = TRUE, param = "Chg_Per_Year")
v_cover_plot_bar_nativity(sample_frame = "Nahuku/East Rift", paired_change = TRUE, param = "Chg_Per_Year")

haleakala_nativity <- summarize_understory(sample_frame = "Haleakala",
                               paired_change = FALSE,
                               plant_grouping = "Nativity")

haleakala_nativity_paired <- summarize_understory(sample_frame = "Haleakala",
                                           paired_change = TRUE,
                                           plant_grouping = "Nativity")
p <- haleakala_nativity_paired %>%
  filter(Nativity != "Unknown") %>%
  filter(Cycle == 2) %>%
  #mutate(direction = dplyr::case_when(Chg_Prior > 0 ~ "Pos",
  #                             Chg_Prior < 0 ~ "Neg" )) %>%
  ggplot2::ggplot(aes(x = tidytext::reorder_within(Plot_Number, -Chg_Prior, list(Nativity, Stratum)), y = Chg_Prior, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  scale_x_reordered() +
  facet_wrap(Stratum ~ Nativity, scales = "free_x") +
  #ggplot2::facet_grid(Stratum ~ Sampling_Frame + Nativity,
  #                    labeller = label_parsed,
  #                    scales = "free_x") +
  ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
  #scale_fill_manual(values = c("#CC0000", "#009900")) +
  ggplot2::xlab("Plot Number") + ggplot2::ylab("Change in Total % Cover") +
  ggplot2::theme(legend.position = "none")
p


haleakala_nativity_paired_stats <- add_stats(haleakala_nativity_paired, Unit_Code, Sampling_Frame,
                         Cycle, Year, Stratum, Nativity)








olaa <- UnderNativityCover(sample_frame = "Olaa")
table(olaa$Stratum)

olaa_paired_change <- UnderNativityCover(sample_frame = "Olaa", paired_change = TRUE)

olaa_s <- summarize_understory(sample_frame = "Olaa",
                               paired_change = FALSE,
                               plant_grouping = "Nativity")
table(olaa_s$Stratum)

olaa_paired_change_s <- summarize_understory(sample_frame = "Olaa",
                                             paired_change = TRUE,
                                             plant_grouping = "Nativity")
table(olaa_paired_change$Stratum)
table(olaa_paired_change_s$Stratum)

v_cover_plot_bar_nativity (sample_frame = "Olaa",
                combine_strata = FALSE, paired_change = FALSE)

look <- summarize_understory(sample_frame = "Olaa", plant_grouping = "Nativity",
                           combine_strata = FALSE, paired_change = TRUE)
# add stats
look_stat <- add_stats(look, Unit_Code, Sampling_Frame, Cycle, Year, Stratum, Nativity)


look <- summarize_understory(sample_frame = "Puerto Rico",
                plant_grouping = "Nativity",
                combine_strata = FALSE)

cover_ <- summarize_understory(paired_change = FALSE,
                               sample_frame = "Olaa",
                               plant_grouping = "None",
                               plot_number = 2)

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
  dplyr::group_by(Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year)) %>%
  mutate(Year = as.factor(Year)) %>%
  dplyr::ungroup()

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

add_stats(cover_nativity, Unit_Code, Sampling_Frame, Cycle, Year, Stratum, !!!rlang::syms("Nativity"))

# sample size calculation for text
sample_size <- cover_nat_stat %>%
  select(Year, NPLOTS) %>%
  dplyr::distinct() %>%
  mutate(Text = paste0(Year, " [n = ", NPLOTS, "]")) %>%
  pull(Text) %>%
  paste(collapse = ", ")
sample_size

#........BAR YEARLY MEANS----
p2 <- cover_nat_stat %>%
  filter(Parameter == "Cover") %>%
  ggplot2::ggplot(aes(x = Year, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  ggplot2::labs(y = "Mean % Cover") +
  facet_grid(Stratum ~ Nativity, space = "free_y") +
  ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
  ggplot2::xlab("Year") +
  ggplot2::theme(legend.position="none") +
  ggplot2::labs(caption = sample_size)
p2
