library(pacnvegetation)
library(dplyr)
#--- 1. Read latest cache ----
LoadPACNVeg(force_refresh = FALSE, eips_paths = "foo")

names(pacnvegetation:::GetColSpec())

# Prep Events Dataset
all_events <- FilterPACNVeg(data_name = "Events_extra_xy", is_qa_plot = FALSE) %>%
  select(-QA_Plot, -Certified, -Verified) %>%
  group_by(Unit_Code, Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year)) %>%
  ungroup()

view_event_years <- all_events |>
  count(Unit_Code, Sampling_Frame, Cycle, Year)

# Prep SmWoody Dataset
SmWoody <- FilterPACNVeg(data_name = "SmWoody", is_qa_plot = FALSE) |>
  group_by(Unit_Code, Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year)) %>%
  ungroup() |>
  right_join(all_events, by = join_by(Unit_Code, Sampling_Frame, Sampling_Frame_Formal, Cycle, Year, Plot_Type, Plot_Number))

# QAQC Checks -----------------------------------------------------------------------

# LF_Sm_Woody does not match Life_Form:
unmatched_lifeforms <- SmWoody |>
  mutate(LF_check = case_when(LF_Sm_Woody == "Small Tree" ~ "Tree",
                              LF_Sm_Woody == "Seedling" ~ "Tree",
                              LF_Sm_Woody == "Small Tree Fern" ~ "Tree Fern",
                              .default = LF_Sm_Woody)) |>
  filter(LF_check != Life_Form)

# Simplified unmatched_lifeforms list
ulf <- unmatched_lifeforms |>
  dplyr::select(Sampling_Frame, Cycle, Year, Sample_Area, LF_Sm_Woody, LF_check, DBH,
         Scientific_Name, Code, Nativity, Life_Form) |>
  dplyr::distinct() |>
  dplyr::arrange(-Year)

# any records missing values?
no_code <- SmWoody |>
  filter(!is.na(Count) & if_any(c(1:"Code"), is.na))

shrubs_missing_data <- SmWoody |>
  filter(LF_Sm_Woody == "Shrub") |>
  select(-DBH) |>
  filter(if_any(c(1:"Code"), is.na))

seedlings_missing_data <- SmWoody |>
  filter(LF_Sm_Woody == "Seedling") |>
  select(-DBH) |>
  filter(if_any(c(1:"Code"), is.na))

# Check if anything in Quad 4 that is not a small tree
chk_not_small_tree <- SmWoody |>
  dplyr::filter(Sample_Area == "Quad 4") |>
  dplyr::filter(LF_Sm_Woody != "Small Tree")
print(chk_not_small_tree)

# Check if some plots had more than one entry per species
chk_mult_LF_Sm_Woody <- SmWoody |>
  group_by(Unit_Code, Sampling_Frame, Cycle, Plot_Number, Scientific_Name, Code, Life_Form, DBH, Status, Length, Rooting) |>
  count() |>
  filter(n > 1) |>
  left_join(SmWoody)


# ..............................................................................

# Seedling table
Seedling <- SmWoody |>
  filter(LF_Sm_Woody == "Seedling") %>%
  # All events (plot reads) without seedlings get added in here:
  full_join(all_events, by = join_by(Unit_Code, Sampling_Frame, Sampling_Frame_Formal, Year, Cycle, Plot_Type, Plot_Number)) %>%
  select(Unit_Code, Sampling_Frame, Sampling_Frame_Formal, Year, Cycle, Plot_Type, Plot_Number, Sample_Area, LF_Sm_Woody, DBH,
         Status, Length, Rooting,
         Nativity, Life_Form, Scientific_Name, Code,
         Count)




Seedling_Complete <- Seedling |>
  select(-LF_Sm_Woody)
  tidyr::complete(tidyr::nesting(Unit_Code, Sampling_Frame, Sampling_Frame_Formal,
                                 Year, Cycle, Nativity, Life_Form, Scientific_Name, Code,
                                 Plot_Type, Sample_Area, LF_Sm_Woody),
                  Plot_Number, Status, Length, Rooting)

check1 <- Seedling |>
  filter(Sampling_Frame == "Mauna Loa",
         Plot_Number == 2) |>
  arrange(Cycle, Scientific_Name)

Seedling_Plot_Count <- Seedling |>
  group_by(Sampling_Frame, Cycle, Plot_Number, Scientific_Name, Code, Status)  |>
  summarise(sum_all = sum(Count))

check2 <- Seedling_Plot_Count |>
  filter(Sampling_Frame == "Mauna Loa",
         Plot_Number == 2) |>
  arrange(Cycle, Scientific_Name)

Seedling_Frame_Count <- Seedling_Plot_Count |>
  group_by(Sampling_Frame, Cycle, Scientific_Name, Code, Status)  |>
  summarise(sum_total = sum(sum_all))

check3 <- Seedling_Frame_Count |>
  filter(Sampling_Frame == "Mauna Loa") |>
  arrange(Cycle, Scientific_Name)




# Run complete() to get zeros for Status, Length, and Rooting classes
Seedling_complete <- Seedling |>
  dplyr::ungroup() |>
  tidyr::complete(
    Status, Length, Rooting,
    tidyr::nesting(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number),
    tidyr::nesting(Nativity, Life_Form, Scientific_Name, Code),
    fill = list(Count = 0),
    explicit = FALSE
  )
)
# Select desired filtering/grouping
# Species, Length, Rooting, Status - HERE:
f_grouping <- c()
f_species <- NULL
f_rooting <- NULL
f_status <- "Live"
f_length <- NULL

seedlings_spp_filter <- seedlings_spp_complete

if (!is.null(f_species)) {
  seedlings_spp_filter <- filter(seedlings_spp_filter, Scientific_Name %in% f_species)
}
if (!is.null(f_rooting)) {
  seedlings_spp_filter <- filter(seedlings_spp_filter, Rooting %in% f_rooting)
}
if (!is.null(f_status)) {
  seedlings_spp_filter <- filter(seedlings_spp_filter, Status %in% f_status)
}
if (!is.null(f_length)) {
  seedlings_spp_filter <- filter(seedlings_spp_filter, Length %in% f_length)
}

# By Plot
seedlings_spp_byplot <- seedlings_spp_filter %>%
  drop_na() %>%
  group_by(Unit_Code, Sampling_Frame, Year, Cycle,
           Plot_Type, Plot_Number,
           Nativity, Life_Form, Scientific_Name, Code) %>%
  summarize(n_combinations = n(), # should be 30 combinations (for status, rooting, length)
            seedlings_per_plot = sum(Count)) %>%
  mutate(seedlings_per_meter = case_when(Sampling_Frame == "Kalawao" ~ seedlings_per_plot/80,
                                 Sampling_Frame == "Hoolehua" ~ seedlings_per_plot/80,
                                 Sampling_Frame == "Kaloko-Honokohau" ~ seedlings_per_plot/80,
                                 .default = seedlings_per_plot/100))

# leaflet
seedling_regen_failure <- 0.25 # per square meter
threshold <- seedling_regen_failure

pal <- colorFactor(
  c("#999999", "#FF4E50"),
  # colors depend on the count variable
  domain = seedlings_MR$seedlings_below_threshold,
)

seedlings_MR %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~pal(seedlings_below_threshold),
    # set the opacity of the circles
    opacity = 0.65,
    # set the radius of the circles
    radius = 4,) %>%
  addLegend(
    #data = pharmacies_count,
    pal = pal,
    values = ~seedlings_below_threshold,
    position = "bottomleft",
    title = "Seedlings below threshold",
    opacity = 0.9
  )

# Elucidate graphs
look <- seedlings_spp_byplot %>%
  filter(Sampling_Frame == "Olaa") #%>%
  plot_line(y = seedlings_per_plot, x = Cycle, colour_var = Nativity,
            #geom = "point",
            p_size = 3,
            add_lines = TRUE,
            dodge_width = 0,
            alpha = 0.6,
            stat = "mean",
            print_stats = TRUE)

seedlings_spp_byplot %>%
  #filter(Sampling_Frame == "Olaa") %>%
  filter(Plot_Type == "Fixed") %>%
  #filter(Scientific_Name == "Metrosideros polymorpha") %>%
  plot_stat_error(y = seedlings_per_plot, x = Cycle, colour_var = Nativity,
                  #geom = "point",
                  p_size = 3,
                  add_lines = TRUE,
                  dodge_width = 0.1,
                  alpha = 0.6,
                  stat = "mean",
                  print_stats = TRUE,
                  facet_var = Sampling_Frame)

seedlings_spp_byplot %>%
  filter(Sampling_Frame == "Puu Alii") %>%
  #filter(Plot_Type == "Fixed") %>%
  #filter(Scientific_Name == "Metrosideros polymorpha") %>%
  plot_stat_error(y = seedlings_per_plot, x = Cycle, colour_var = Nativity,
                  #geom = "point",
                  p_size = 3,
                  add_lines = TRUE,
                  dodge_width = 0.1,
                  alpha = 0.6,
                  stat = "mean",
                  print_stats = TRUE)



# By Sampling Frame
seedlings_spp_samp <- seedlings_spp_byplot %>%
  group_by(Unit_Code, Sampling_Frame, Year, Cycle,
           Status, Length, Rooting,
           Nativity, Life_Form, Scientific_Name, Code) %>%
  summarize(n_plots = n(),
            count_total = sum(Count),
            count_mean = mean(Count),
            count_median = median(Count)) %>%
  mutate(count_per_m = case_when(Sampling_Frame == "Kalawao" ~ nativity_count/80,
                                     Sampling_Frame == "Hoolehua" ~ nativity_count/80,
                                     Sampling_Frame == "Kaloko-Honokohau" ~ nativity_count/80,
                                     .default = nativity_count/100)) %>%
  drop_na()


# Group totals here:
seedlings_spp_structure_sum <- seedlings_spp_structure %>%
  group_by(Unit_Code, Sampling_Frame, Year, Cycle,
           Nativity, Life_Form, Scientific_Name, Code) %>%
  summarize(all_seedlings = sum(total_count),
            n_plots = mean(n_plots))



seedlings <- seedlings_spp %>%
  group_by(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Nativity) %>%
  summarise(nativity_count = sum(Count)) %>%
  mutate(nat_count_per_m = case_when(Sampling_Frame == "Kalawao" ~ nativity_count/80,
                                       Sampling_Frame == "Hoolehua" ~ nativity_count/80,
                                       Sampling_Frame == "Kaloko-Honokohau" ~ nativity_count/80,
                                     .default = nativity_count/100
                                     )) %>%
  complete(Cycle, Sampling_Frame, Nativity, Plot_Number)



seedlings_boxplot <- seedlings %>%
  filter(Nativity == "Native") %>%
  filter(Cycle == 3) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(nat_count_per_m), median),
             y=nat_count_per_m,
             fill = Nativity
             )) +
  geom_boxplot() +
  geom_hline(yintercept = 0.25) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  gghighlight(median(nat_count_per_m, na.rm = TRUE) < 0.25)
seedlings_boxplot

sf <- "Olaa"

library(elucidate)

plot_stat_error(seedlings, y = nat_count_per_m, x = Cycle, colour = "blue")

seedlings %>%
  filter(Unit_Code == "HAVO") %>%
  filter(Nativity == "Native") %>%
  plot_line(y = nat_count_per_m, x = Cycle, colour_var = Sampling_Frame,
            #geom = "point",
            p_size = 3,
            add_lines = TRUE,
            dodge_width = 0,
            alpha = 0.6,
            stat = "mean",
            print_stats = TRUE)

seedlings %>%
  #filter(Unit_Code == "HAVO") %>%
  filter(Nativity == "Native") %>%
  plot_stat_error(y = nat_count_per_m, x = Cycle, colour_var = Sampling_Frame,
            #geom = "point",
            p_size = 3,
            add_lines = TRUE,
            dodge_width = 0,
            alpha = 0.6,
            stat = "median",
            print_stats = TRUE)

samp_frame_boxplot <- seedlings %>%
  filter(Nativity == "Native") %>%
  filter(Sampling_Frame == sf) %>%
  ggplot(aes(x=Cycle,
             y=median(nat_count_per_m),
             grp=Cycle
  )) +
  geom_point() #+
  #geom_hline(yintercept = 0.25) #+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #gghighlight(median(nat_count_per_m, na.rm = TRUE) < 0.25)
samp_frame_boxplot
