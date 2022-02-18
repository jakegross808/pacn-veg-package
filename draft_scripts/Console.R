# To run test use:
library(pacnvegetation)
library(tidyverse)
library(tidytext)


LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_1_2021_20220120.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
            cache = TRUE, force_refresh = FALSE)

MapPACNVeg2(sample_frame = "Haleakala")



MapCoverChange(sample_frame = "Haleakala", cycle = 2)


pacnvegetation:::pchIcons(pch = rep(22, nrow(cover_data)),
         width = 30,
         height = 30,
         bg = colorspace::darken(cover_data$color),
         col = cover_data$color, 0.3)

MapCoverChange(combine_strata = TRUE, park = "WAPA", cycle = 2, paired_cycle = 1)

look <- summarize_understory(combine_strata = TRUE, plant_grouping = "Nativity", paired_change = TRUE, sample_frame = "Olaa")
look_old <- UnderNativityCover(combine_strata = TRUE, paired_change = TRUE, sample_frame = "Olaa", cycle = 3)


look <- FilterPACNVeg("Understory", park = "HAVO", cycle = 2)
look <- FilterPACNVeg("Understory", sample_frame = "Haleakala", cycle = 2)

chk <- look %>%
  #filter(Plot_Number == 10) %>%
  filter(Stratum == "Low") %>%
  group_by(Sampling_Frame, Cycle, Dead, Scientific_Name, Code, Nativity) %>%
  summarise(hits = n())



v_cover_plot_bar_nativity(sample_frame = "Haleakala", paired_change = FALSE, param = "Cover")


v_cover_plot_bar_nativity(sample_frame = "Haleakala", paired_change = TRUE, param = "Chg_Per_Year")

v_cover_plot_bar_nativity(sample_frame = "Nahuku/East Rift", paired_change = TRUE, param = "Chg_Per_Year")



haleakala_nativity <- summarize_understory(sample_frame = "Haleakala",
                               paired_change = FALSE,
                               plant_grouping = "Nativity")

haleakala_nativity_stats <- add_stats(haleakala_nativity)

haleakala_nativity_paired <- summarize_understory(sample_frame = "Haleakala",
                                           paired_change = TRUE)

