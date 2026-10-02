
library(pacnvegetation)
library(tidyverse)
#--- 1. Read latest cache ----

# Write/Read csv from pacnvegetation package:
pacnveg_cache_path <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/R_WritePACNVeg"

# Read
path_file_info <- file.info(list.files(pacnveg_cache_path, full.names = T))
latest_folder <- rownames(path_file_info)[which.max(path_file_info$mtime)]

LoadPACNVeg(data_path = latest_folder,
            data_source = "file")

names(FilterPACNVeg())

raw_Lg_Trees <- FilterPACNVeg(data_name = "LgTrees", is_qa_plot = FALSE)

Lg_Trees <- FilterPACNVeg(data_name = "LgTrees", is_qa_plot = FALSE) %>%
  filter(Year != 2023) |>
  select(-QA_Plot) |>
  group_by(Sampling_Frame, Cycle) |>
  mutate(Year = max(Year)) |>
  group_by(Sampling_Frame) |>
  mutate(max_cycle = max(Cycle)) |>
  ungroup() |>
  #filter(Cycle == max_cycle) |>
  filter(Status == "Live") |>
  select(-Status, -Height_Dead)

str(Lg_Trees)

Lg_Trees_sum <- Lg_Trees %>%
  mutate(bole_dbh = case_when(is.na(DBH_Bole) ~ DBH,
                             !is.na(DBH_Bole) ~ DBH_Bole,
                             .default = NA)) %>%
  filter(bole_dbh >= 10) %>%
  filter(Nativity != "Unknown") |>
  mutate(bole_ba_m2 = (bole_dbh^2)*0.00007854) %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Number, Scientific_Name, Code, Life_Form, Nativity) %>%
  summarise(stems = n(),
            sum_ba_m2 = sum(bole_ba_m2))

Lg_Trees_nativity_ratio <- Lg_Trees_sum %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Number, Nativity) %>%
  summarise(sum_ba_nativity_m2 = sum(sum_ba_m2)) %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Number) %>%
  mutate(plot_total_ba_m2 = sum(sum_ba_nativity_m2)) %>%
  mutate(ratio = round((sum_ba_nativity_m2/plot_total_ba_m2)*100,digits = 1))

Lg_Trees_species_ratio <- Lg_Trees_sum %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Number, Scientific_Name, Code, Life_Form, Nativity) %>%
  summarise(sum_ba_species_m2 = sum(sum_ba_m2)) %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Number) %>%
  mutate(plot_total_ba_m2 = sum(sum_ba_species_m2)) %>%
  mutate(ratio = round((sum_ba_species_m2/plot_total_ba_m2)*100,digits = 1))

# Visualizations
Lg_Trees_nativity_ratio |>
  filter(Nativity == "Native") |>
  filter(Sampling_Frame != "Haleakala") |>
  ggplot(aes(fct_reorder(.na_rm = TRUE, Sampling_Frame, desc(ratio), median), y = ratio, fill = Nativity)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values=c("#336600")) +
  ggtitle("Native Canopy Percentage") +
  xlab("Sampling Frame") +
  ylab("percentage (%)") +
  theme(legend.position = "none") #+
  # Add horizontal line at y = 2O
  geom_hline(yintercept=70, linetype="dashed",
             color = "red", size=1)

  #facet_grid(rows = vars(Sampling_Frame), scales = "free_y", space = "free", switch = "x") +
  #theme(strip.text.y.right = element_text(angle = 0)) +
  #facet_grid(cols = vars(Sampling_Frame), scales = "free_x")

Lg_Trees_species_ratio |>
  filter(Nativity == "Unknown") |>
  mutate(Year = as.factor(Year)) |>
  ggplot(aes(x = fct_reorder(.na_rm = TRUE, Scientific_Name, ratio, median), y = ratio, color = Nativity)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("Non-Native Canopy Species") +
  xlab("Species") +
  ylab("percentage (%)") +
  facet_grid(rows = vars(Sampling_Frame), scales = "free_y", space = "free", switch = "x") +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_color_manual(values=c("#660099"))

Lg_Trees_species_ratio |>
  filter(Nativity == "Non-Native") |>
  mutate(Year = as.factor(Year)) |>
  ggplot(aes(x = fct_reorder(.na_rm = TRUE, Scientific_Name, ratio, median), y = ratio, color = Nativity)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("Non-Native Canopy Species") +
  xlab("Species") +
  ylab("percentage (%)") +
  facet_grid(rows = vars(Sampling_Frame), scales = "free_y", space = "free", switch = "x") +
  theme(strip.text.y.right = element_text(angle = 0)) +
  theme(legend.position = "none")

Lg_Trees_species_ratio |>
  filter(Unit_Code != "NPSA") |>
  filter(Nativity == "Native") |>
  mutate(Year = as.factor(Year)) |>
  ggplot(aes(x = fct_reorder(.na_rm = TRUE, Scientific_Name, ratio, median), y = ratio, color = Nativity)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("Native Canopy Species") +
  xlab("Species") +
  ylab("percentage (%)") +
  facet_grid(rows = vars(Sampling_Frame), scales = "free_y", space = "free", switch = "x") +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_color_manual(values=c("#339900")) +
  theme(legend.position = "none")

Lg_Trees_species_ratio |>
  filter(Unit_Code == "NPSA") |>
  filter(Nativity == "Native") |>
  mutate(Year = as.factor(Year)) |>
  ggplot(aes(x = fct_reorder(.na_rm = TRUE, Scientific_Name, ratio, median), y = ratio, color = Nativity)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("Native Canopy Species") +
  xlab("Species") +
  ylab("percentage (%)") +
  facet_grid(rows = vars(Sampling_Frame), scales = "free_y", space = "free", switch = "x") +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_color_manual(values=c("#339900")) +
  theme(legend.position = "none")

Lg_Trees_sf_ratio <- Lg_Trees_plot_ratio %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Nativity) |>
  summarise(mean_ba_ratio = mean(ratio))


# Large Trees Olaa ----
# Check large tree status & vigor in Olaa
LgTrees_prep <- FilterPACNVeg(data_name = "LgTrees", is_qa_plot = FALSE) %>%
  group_by(Unit_Code, Community, Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year))

Olaa_LGstem_DBH <- LgTrees_prep %>%
  filter(Life_Form == "Tree") %>%
  filter(Sampling_Frame == "Olaa") %>%
  mutate(Stem_DBH = case_when(is.na(DBH_Bole) ~ DBH,
                              !is.na(DBH_Bole) ~ DBH_Bole,
                              .default = as.numeric(DBH))) %>%
  mutate(Stem_Status = case_when(is.na(Status_Bole) ~ Status,
                                 !is.na(Status_Bole) ~ Status_Bole,
                                 .default = as.character(Status))) %>%
  filter(Stem_DBH >= 10) %>%
  filter(is.na(Foliar)) %>%
  filter(Caudex_Length == 999) %>%
  select(Unit_Code, Community, Sampling_Frame, Year, Cycle,
         Plot_Type, Plot_Number, Quad, Scientific_Name, Code, Nativity,
         Stem_Status, Vigor, Rooting, Stem_DBH)

# Get count of large stems
Olaa_LGstem_count <- Olaa_LGstem_DBH %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle,
           Plot_Number, Plot_Type, Stem_Status) %>%
  summarise(n_stems = n())

Olaa_LGstem_METPOL_count <- Olaa_LGstem_DBH %>%
  filter(Scientific_Name == "Metrosideros polymorpha") %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle,
           Plot_Number, Plot_Type, Scientific_Name, Stem_Status) %>%
  summarise(n_stems = n())

# Graph Olaa Trees y-axis: DBH
Olaa_LGstem_DBH |>
  filter(Code == "METPOL1") |>
  filter(Plot_Type == "Fixed") |>
  mutate(Year = as.factor(Year)) |>
  mutate(Cycle = as.factor(Cycle)) |>
  #ggplot(aes(x = fct_reorder(.na_rm = TRUE, Scientific_Name, Stem_DBH), y = Stem_DBH, color = Quad))
  ggplot(aes(x = Cycle, y = Stem_DBH, fill = Stem_Status)) +
  geom_boxplot() +
  #coord_flip() +
  ggtitle("Metrosideros polymorpha") +
  xlab("") +
  ylab("") +
  facet_grid(rows = vars(Sampling_Frame), cols = vars(Stem_Status), scales = "free_y", space = "free", switch = "x") +
  theme(strip.text.y.right = element_text(angle = 0)) #+
#theme(legend.position = "none")

# Graph Olaa Trees y-axis: Count
Olaa_LGstem_count |>
  filter(Plot_Number == "1") |>
  mutate(Year = as.factor(Year)) |>
  mutate(Cycle = as.factor(Cycle)) |>
  ggplot(aes(x = Cycle, y = n_stems, fill = Stem_Status)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("") +
  ylab("") +
  facet_grid(rows = vars(Sampling_Frame), cols = vars(Stem_Status), scales = "free_y", space = "free", switch = "x") +
  theme(strip.text.y.right = element_text(angle = 0))

Olaa_LGstem_METPOL_count |>
  filter(Plot_Type == "Fixed") |>
  mutate(Year = as.factor(Year)) |>
  mutate(Cycle = as.factor(Cycle)) |>
  ggplot(aes(x = Cycle, y = n_stems, fill = Stem_Status)) +
  geom_boxplot() +
  ggtitle("Metrosideros polymorpha") +
  xlab("") +
  ylab("count") +
  facet_grid(rows = vars(Sampling_Frame), cols = vars(Stem_Status), scales = "free_y", space = "free", switch = "x") +
  theme(strip.text.y.right = element_text(angle = 0))






# Understory

library(gghighlight)

all_nat_chg_und <- pacnvegetation::summarize_understory(
  combine_strata = TRUE,
  plant_grouping = "Nativity",
  paired_change = TRUE)

all_spp_chg_und <- pacnvegetation::summarize_understory(
  combine_strata = TRUE,
  plant_grouping = "Species",
  paired_change = TRUE)

all_trees_nonnative_und <- all_spp_chg_und %>%
  #filter(Sampling_Frame %in% c("Olaa", "Kipahulu District", "Puu Alii", "Tau", "Tutuila")) %>%
  filter(Life_Form == "Grass") %>%
  filter(Nativity == "Non-Native") %>%
  filter(Cycle == 2) %>%
  filter(Cover > 0)

all_trees_nonnative_und |>
  mutate(Year = as.factor(Year)) |>
  ggplot(aes(x = fct_reorder(.na_rm = TRUE, Scientific_Name, Cover, median), y = Cover, color = Nativity)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("Understory cover of non-native trees") +
  xlab("Species") +
  ylab("percentage cover (%)") +
  facet_grid(rows = vars(Sampling_Frame), scales = "free_y", space = "free", switch = "x") +
  theme(strip.text.y.right = element_text(angle = 0)) +
  theme(legend.position = "none")

ncyc3 <- pacnvegetation:::understory_cover_boxplot(all_nat_chg_und, nativity = "Non-Native", cycle = 3, no_y_labs = TRUE)
ncyc3


