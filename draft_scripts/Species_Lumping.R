# Species Lumping Table ----

library(magrittr)

Species_lump <- FilterPACNVeg("Species_extra") %>%
  dplyr::select(Lump_Park = Park,
                Lump_Nativity = Nativity,
                Lump_Life_Form = Life_Form,
                Lump_Genus = Genus,
                Lump_Species = Species,
                Lump_Subdivision = Subdivision,
                Lump_Scientific_Name = Scientific_Name,
                Lump_Code = Code)

Species_lump2 <- Species_lump %>%
  dplyr::group_by(Lump_Park, Lump_Genus, Lump_Species) %>%
  dplyr::filter(dplyr::n()>1) %>%
  dplyr::filter(!is.na(Lump_Genus)) %>%
  dplyr::filter(is.na(Lump_Subdivision))

Species_extra <- FilterPACNVeg("Species_extra") %>%
  dplyr::left_join(Species_lump2, by = c("Park" = "Lump_Park", "Genus" = "Lump_Genus", "Species" = "Lump_Species"))
