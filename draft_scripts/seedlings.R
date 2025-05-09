library(pacnvegetation)

names(pacnvegetation:::GetColSpec())

# Prep Events Dataset
all_events <- FilterPACNVeg(data_name = "Events_extra_xy", is_qa_plot = FALSE) %>%
  select(-QA_Plot) %>%
  group_by(Unit_Code, Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year)) %>%
  ungroup()

# Prep Seedlings Dataset
seedlings_spp <- FilterPACNVeg(data_name = "SmWoody", is_qa_plot = FALSE) %>%
  filter(LF_Sm_Woody == "Seedling") %>%
  group_by(Unit_Code, Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year)) %>%
  ungroup() %>%
  # All events (plot reads) without seedlings get added in here:
  full_join(all_events, by = join_by(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number)) %>%
  select(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number,
         Status, Length, Rooting,
         Nativity, Life_Form, Scientific_Name, Code,
         Count)

# Run complete() to get zeros for Status, Length, and Rooting classes
seedlings_spp_complete <- seedlings_spp %>%
  ungroup() %>%
  complete(
    Status, Length, Rooting,
    nesting(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number),
    nesting(Nativity, Life_Form, Scientific_Name, Code),
    fill = list(Count = 0),
    explicit = FALSE
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
