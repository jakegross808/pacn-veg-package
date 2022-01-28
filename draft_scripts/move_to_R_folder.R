# To run test use:
library(pacnvegetation)
library(tidyverse)
library(tidytext)


LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_1_2021_20220120.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
            cache = TRUE, force_refresh = FALSE)

haleakala_nativity_paired <- summarize_understory(sample_frame = "Haleakala",
                                           paired_change = TRUE,
                                           plant_grouping = "Nativity")
p <- haleakala_nativity_paired %>%
  filter(Nativity != "Unknown") %>%
  filter(Cycle == 2) %>%
  #mutate(direction = case_when(Chg_Prior > 0 ~ "Pos",
  #                             Chg_Prior < 0 ~ "Neg" )) %>%
  ggplot(aes(x = tidytext::reorder_within(Plot_Number, -Chg_Prior, list(Nativity, Stratum)), y = Chg_Prior, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  scale_x_reordered() +
  facet_wrap(Stratum ~ Nativity, scales = "free_x") +
  #ggplot2::facet_grid(Stratum ~ Sampling_Frame + Nativity,
  #                    labeller = label_parsed,
  #                    scales = "free_x") +
  ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
  #scale_fill_manual(values = c("#CC0000", "#009900")) +
  xlab("Plot Number") + ylab("Change in Total % Cover") +
  theme(legend.position = "none")
p
