library(tidyverse)


space_and_flynn <- read_csv(file = "C:/Users/JJGross/OneDrive - DOI/Documents/Papers/2000_Space_and_Flynn_Observations on invasive plant species in American Samoa .csv")

risk_assessment_old <- read_csv(file = "C:/Users/JJGross/OneDrive - DOI/Documents/Papers/2017_Risk_Assessments_Pacific_Island_Ecosystems_at_Risk.csv")
risk_assessment <- read_csv(file = "C:/Users/JJGross/OneDrive - DOI/Documents/Papers/All_HPWRA.csv")

NPSA_Ofu_veg_classif <- read_csv(file = "C:/Users/JJGross/OneDrive - DOI/Documents/Papers/2000_Space_and_Flynn_Observations on invasive plant species in American Samoa .csv")

# First attempt with PIER list ----
space_and_flynn_PIER1 <- space_and_flynn |>
  mutate(Present = 1) |>
  select(-Appendix) |>
  pivot_wider(names_from = Island, values_from = Present, values_fill = 0) |>
  #filter(Tutuila == 1 & Ofu == 0) |>
  select(-`Invasive not present in American Samoa`) |>
  select(-`Invasive present in American Samoa`) |>
  select(-Family, -`Common Names`)

space_and_flynn_PIER2 <- space_and_flynn_PIER1 |>
  left_join(risk_assessment_old, by = join_by(`Scientific Name`)) |>
  arrange(-Tutuila, Ofu, -Score) |>
  select(-`Risk assessment`, -Recommendation) #-`Country or Area`

dups <- space_and_flynn_PIER2 |>
  group_by(`Scientific Name`) |>
  filter(n()>1)

dups_not_pacific <- dups |>
  filter(`Country or Area` != "Pacific") |>
  select(`Scientific Name`)

dups_pacific <- dups |>
  filter(`Country or Area` == "Pacific") |>
  select(`Scientific Name`)

no_pacific_rating <- anti_join(dups_not_pacific, dups_pacific) |>
  distinct() |>
  pull()

non_pacific <- space_and_flynn_PIER2 |>
  filter(`Scientific Name` %in% no_pacific_rating) |>
  arrange(-Score) |>
  distinct(`Scientific Name`, .keep_all = TRUE)

NPSA_Ofu_PIER_WRA <- space_and_flynn_PIER2 |>
  filter(`Country or Area` == "Pacific") |>
  bind_rows(non_pacific) |>
  arrange(-Tutuila, Ofu, -Score)

write_csv(NPSA_Ofu_PIER_WRA, paste0("C:/Users/JJGross/Downloads/NPSA_Ofu_PIER_WRA", Sys.Date(), ".csv"))

NPSA_Ofu_PIER_WRA


# Second attempt with HPWRA list ----
risk_assessment2 <- risk_assessment |>
  separate_wider_delim(Synonyms, ", ", names = c("A", "B", "C", "D"), too_few = "align_start")

space_and_flynn2 <- space_and_flynn |>
  mutate(Present = 1) |>
  select(-Appendix) |>
  pivot_wider(names_from = Island, values_from = Present, values_fill = 0) |>
  #filter(Tutuila == 1 & Ofu == 0) |>
  select(-`Invasive not present in American Samoa`) |>
  select(-`Invasive present in American Samoa`) |>
  select(-Family, -`Common Names`)

space_and_flynn_join1 <- space_and_flynn2 |>
  inner_join(risk_assessment, by = join_by(`Scientific Name` == Taxa))
space_and_flynn_join2 <- space_and_flynn2 |>
  inner_join(risk_assessment2, by = join_by(`Scientific Name` == A))
space_and_flynn_join3 <- space_and_flynn2 |>
  inner_join(risk_assessment2, by = join_by(`Scientific Name` == B))
space_and_flynn_join4 <- space_and_flynn2 |>
  inner_join(risk_assessment2, by = join_by(`Scientific Name` == C))
space_and_flynn_join5 <- space_and_flynn2 |>
  inner_join(risk_assessment2, by = join_by(`Scientific Name` == D))

space_and_flynn_all_joins <- bind_rows(
  space_and_flynn_join1,
  space_and_flynn_join2,
  space_and_flynn_join3,
  space_and_flynn_join4,
  space_and_flynn_join5
)

space_and_flynn_no_matchs <- space_and_flynn2 |>
  anti_join(space_and_flynn_all_joins, by = join_by(`Scientific Name`))

space_and_flynn_hpwra <- space_and_flynn_all_joins |>
  bind_rows(space_and_flynn_no_matchs)



write_csv(space_and_flynn_hpwra, paste0("C:/Users/JJGross/Downloads/space_and_flynn_hpwra_", Sys.Date(), ".csv"))


# compare PIER to HPWRA ----
comp_hpwra <- space_and_flynn_hpwra |>
  filter(!is.na(`WRA score`))

comp_pier <- NPSA_Ofu_PIER_WRA |>
  filter(!is.na(Score))

look <- anti_join(comp_hpwra, comp_pier, by = join_by(`Scientific Name`))
look2 <- anti_join(comp_pier, comp_hpwra, by = join_by(`Scientific Name`))




#---- Check Nativity of Veg Map species in Ofu
# Local Path to Veg Spp database
veg_species_db_folder <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
# If only one database in folder, this will grab full path:
veg_species_db_full_path <- list.files(veg_species_db_folder,full.names = TRUE)
veg_species_db_full_path
# Get raw data from Veg Species Database:
library(pacnvegetation)
library(tidyverse)

LoadPACNVeg(force_refresh = FALSE, eips_paths = "foo")
raw_spp_data <- read_spp_db(veg_species_db_full_path)
spp_list_NPSA <- master_spp_list(veg_species_db_full_path, park = "NPSA")

NPSA_Ofu_veg_classif <- read_csv(file = "C:/Users/JJGross/OneDrive - DOI/Documents/Papers/Ofu_classification_plots.csv")

NPSA_Ofu_vm_spp <- NPSA_Ofu_veg_classif |>
  left_join(spp_list_NPSA, by = join_by(`Scientific Name` == Scientific_name)) |>
  group_by(`Scientific Name`, Nativeness) |>
  summarise(sum = sum(`Continuous Cover Per Stratum`))
  #select(`Scientific Name`) |>
  #distinct()

NPSA_Ofu_vm_spp_total <- NPSA_Ofu_vm_spp |>
  group_by(Nativeness) |>
  summarize(all_spp = sum(`sum`))



