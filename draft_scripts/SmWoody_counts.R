# Prep SmWoody Dataset
SmWoody <- FilterPACNVeg(data_name = "SmWoody", is_qa_plot = FALSE) |>
  group_by(Unit_Code, Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year)) %>%
  ungroup() |>
  right_join(all_events, by = join_by(Unit_Code, Sampling_Frame, Sampling_Frame_Formal, Cycle, Year, Plot_Type, Plot_Number))

SmWoody_METPOL <- SmWoody |>
  dplyr::filter(Code == "METPOL1")

# All fixed plots visited every cycle----
all_perfect_fixed_plots <- all_events |>
  select(Unit_Code, Sampling_Frame, Year, Plot_Number) |>
  group_by(Sampling_Frame, Plot_Number) |>
  tally() |>
  filter(n > 1)

SmWoody_METPOL_perfect <- SmWoody_METPOL |>
  semi_join(all_perfect_fixed_plots, by = join_by(Sampling_Frame, Plot_Number))

SmWoody_METPOL_fix <- SmWoody_METPOL_perfect |>
  mutate(LF_Sm_Woody = case_when(Sample_Area == "Transect 3" ~ "Seedling", .default = LF_Sm_Woody)) |>
  mutate(DBH = case_when(Sample_Area == "Transect 3" ~ "<1", .default = DBH)) |>
  filter(Status == "Live")

SmWoody_METPOL_perfect_counts_by_plot <- SmWoody_METPOL_fix |>
  group_by(Unit_Code, Sampling_Frame, Cycle, Year, Plot_Number, QA_Plot, Sample_Area, LF_Sm_Woody, DBH, Life_Form, Nativity, Scientific_Name, Code) |>
  summarise(plot_count = sum(Count),
            .groups = "drop") |>
  mutate(Year = as.factor(Year)) |>
  mutate(Cycle = as.factor(Cycle))

# Bar Graph:
bar_SmWoody_by_plot <- SmWoody_METPOL_perfect_counts_by_plot |>
  filter(Sampling_Frame == "Olaa") |>
  ggplot(aes(DBH, plot_count, fill = Cycle)) +
  geom_bar(position="dodge", stat="identity", color= "black") +
  scale_fill_brewer(palette="Blues") +
  facet_wrap(~Plot_Number)
bar_SmWoody_by_plot

SmWoody_METPOL_perfect_counts_by_sampling_frame <- SmWoody_METPOL_perfect_counts_by_plot |>
  group_by(Unit_Code, Sampling_Frame, Cycle, Year, QA_Plot, Sample_Area, LF_Sm_Woody, DBH, Life_Form, Nativity, Scientific_Name, Code) |>
  summarise(total_count = sum(plot_count),
            .groups = "drop")

bar_SmWoody_by_plot <- SmWoody_METPOL_perfect_counts_by_sampling_frame |>
  filter(Sampling_Frame == "Olaa") |>
  ggplot(aes(DBH, total_count, fill = Cycle)) +
  geom_bar(position="dodge", stat="identity", color= "black") +
  scale_fill_brewer(palette="Blues") +
  facet_wrap(~Sampling_Frame)
bar_SmWoody_by_plot


