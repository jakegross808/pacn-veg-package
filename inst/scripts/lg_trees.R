LoadPACNVeg(force_refresh = FALSE, eips_paths = "foo")

# Prep Events Dataset
all_events <- FilterPACNVeg(data_name = "Events_extra_xy", is_qa_plot = FALSE) %>%
  select(-QA_Plot, -Certified, -Verified) %>%
  group_by(Unit_Code, Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year)) %>%
  ungroup()

# Prep SmWoody Dataset
LgTrees <- FilterPACNVeg(data_name = "LgTrees", is_qa_plot = FALSE) |>
  group_by(Unit_Code, Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year)) %>%
  ungroup() |>
  right_join(all_events, by = join_by(Unit_Code, Sampling_Frame, Sampling_Frame_Formal, Cycle, Year, Plot_Type, Plot_Number))

LgTrees_METPOL <- LgTrees |>
  dplyr::filter(Code == "METPOL1") |>
  dplyr::filter(Community == "Wet Forest")

# All fixed plots visited every cycle----
all_perfect_fixed_plots <- all_events |>
  select(Unit_Code, Sampling_Frame, Year, Plot_Number) |>
  group_by(Sampling_Frame, Plot_Number) |>
  tally() |>
  filter(n > 1)

LgTrees_METPOL_perfect <- LgTrees_METPOL |>
  semi_join(all_perfect_fixed_plots, by = join_by(Sampling_Frame, Plot_Number))

LgTrees_METPOL_fix <- LgTrees_METPOL_perfect |>
  mutate(DBH_Bole = case_when(is.na(DBH_Bole) ~ DBH, .default = DBH_Bole)) |>
  filter(Status == "Live")

LgTrees_METPOL_perfect_counts_by_plot <- LgTrees_METPOL_fix |>
  group_by(Unit_Code, Sampling_Frame, Cycle, Year, Plot_Number, QA_Plot, Life_Form, Status, Nativity, Scientific_Name, Code) |>
  summarise(plot_count = n(),
            sum_dbh_bole = sum(DBH_Bole),
            .groups = "drop") |>
  mutate(Year = as.factor(Year)) |>
  mutate(Cycle = as.factor(Cycle))

# Count Bar Graph by plot:
LgTrees_METPOL_by_plot <- LgTrees_METPOL_perfect_counts_by_plot |>
  filter(Sampling_Frame == "Olaa") |>
  ggplot(aes(Status, plot_count, fill = Cycle)) +
  geom_bar(position="dodge", stat="identity", color= "black") +
  scale_fill_brewer(palette="Blues") +
  facet_wrap(~Plot_Number)
LgTrees_METPOL_by_plot

# Sum DBH bole Graph by plot:
LgTrees_dbh_bole_METPOL_by_plot <- LgTrees_METPOL_perfect_counts_by_plot |>
  filter(Sampling_Frame == "Olaa") |>
  ggplot(aes(Status, sum_dbh_bole, fill = Cycle)) +
  geom_bar(position="dodge", stat="identity", color= "black") +
  scale_fill_brewer(palette="Blues") +
  facet_wrap(~Plot_Number)
LgTrees_dbh_bole_METPOL_by_plot

# Bar Graph by sampling frame:
LgTrees_METPOL_perfect_counts_by_sampling_frame <- LgTrees_METPOL_perfect_counts_by_plot |>
  group_by(Unit_Code, Sampling_Frame, Cycle, Year, QA_Plot, Life_Form, Status, Nativity, Scientific_Name, Code) |>
  summarise(total_count = sum(plot_count),
            total_sum_dbh_bole = sum(sum_dbh_bole),
            .groups = "drop")

# Count boles
bar_LgTrees_by_sampling_frame <- LgTrees_METPOL_perfect_counts_by_sampling_frame |>
  filter(Sampling_Frame == "Olaa") |>
  ggplot(aes(Status, total_count, fill = Cycle)) +
  geom_bar(position="dodge", stat="identity", color= "black") +
  scale_fill_brewer(palette="Blues") +
  facet_wrap(~Sampling_Frame)
bar_LgTrees_by_sampling_frame

# Sum dbh bole:
LgTrees_dbh_bole_by_sampling_frame <- LgTrees_METPOL_perfect_counts_by_sampling_frame |>
  filter(Sampling_Frame == "Olaa") |>
  ggplot(aes(Status, total_sum_dbh_bole, fill = Cycle)) +
  geom_bar(position="dodge", stat="identity", color= "black") +
  scale_fill_brewer(palette="Blues") +
  facet_wrap(~Sampling_Frame)
LgTrees_dbh_bole_by_sampling_frame
