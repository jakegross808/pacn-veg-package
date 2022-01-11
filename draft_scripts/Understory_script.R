##===================== UNDERSTORY EDA ================
##...................................................##
##...US National Park Service........................##
##...Pacific Island Inventory & Monitoring Network...##
##...................................................##
##...FTPC - Focal Terrestrial Plant Communities......##
##...EIPS - Established Invasive Plant Species.......##
##...................................................##
##...J. Gross .......................................##
##...................................................##
##...Exploratory Data Analysis (EDA) ................##
##...................................................##
##...Understory Cover ...............................##
##...................................................##
##...Last Update: 12/6/2021..........................##


#'* NOTE - This script connects to the SQL database in Seattle for FTPC * 
#'* And local Access databases for EIPS *
#'* All PACN vegetation R scrips are located on I drive: *
#'* 'I:\vital_signs\05_focal_terr_plant_communities\Documents\R_scripts' *


#.-----------------------------------------------------
#   Packages  ---- 
#.......................................................
library(leaflet)
library(gridExtra)
library(ggrepel)
library(tidytext)
library(lubridate)
library(here)
library(tidyverse)

library(pacnvegetation)

#.-----------------------------------------------------
#   Custom Functions  ---- 
#.......................................................

add.stats2 <- function(.data, ...){
  # dataset needs to have columns: "Unit_Code, Sampling_Frame, Stratum"
  # column is the dataset$column that shows the change between two cycles
  
  #.group_vars <- enquos(...)
  
  params <- .data %>%
    dplyr::select(where(is.numeric)) %>% 
    names() 
  
  print(params)
  
  stat_table <- tibble::tibble()
  
  for (param in params) {
    
    #col.char <- (as_label(param))
    print(param)
    
    stat_table_param <- .data %>%
      dplyr::group_by(...) %>%
      summarise(NPLOTS = sum(!is.na(.data[[param]])),
                MEAN = round(mean(.data[[param]], na.rm = TRUE),3),
                MED = round(median(.data[[param]], na.rm = TRUE),3),
                MIN = round(min(.data[[param]], na.rm = TRUE),3),
                MAX = round(max(.data[[param]], na.rm = TRUE),3),
                SD = sd(.data[[param]], na.rm = TRUE),
                ERR = qt(0.975,df=NPLOTS-1)*(SD/sqrt(NPLOTS)),
                L = MEAN - ERR,
                R = MEAN + ERR) %>%
      dplyr::mutate(PARAM = param) 
                      

    
    stat_table <- dplyr::bind_rows(stat_table, stat_table_param)
  }
  
  return(stat_table)
  
  


    
    
    # tidyr::pivot_longer(
    #   # period (.) means match any character
    #   # asterisk (*) means match zero or more times
    #   # use () to distinguish groups
    #   # therefore the following breaks apart two words separated by underscore (_)
    #   cols = MEAN_CHG:R_CHG,
    #   names_to = c(".value", "Cycle"), 
    #   names_pattern = "(.*)_(.*)") %>%
    # dplyr::arrange(Cycle)
  
}


#.-----------------------------------------------------
#   Loading Data  ---- 
#.......................................................

All_Data <- LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases//2021_established_invasives_1_2021_20211208_0844.mdb",
                         #"C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/2021_established_invasives_1_20210129.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
            cache = TRUE, force_refresh = FALSE)

#.-----------------------------------------------------
# Prep Data -----
#......................................................

Understory <- FilterPACNVeg("Understory", sample_frame = "Olaa", is_qa_plot = FALSE)

EIPS <- FilterPACNVeg("EIPS_data", sample_frame = "Olaa", is_qa_plot = FALSE)


ftpc_xy <- FilterPACNVeg("Events_extra_xy", plot_type = "Fixed", cycle = 3,
                         sample_frame = "Olaa", is_qa_plot = FALSE) %>%
  mutate(lat = Start_Lat,
         long = case_when(Start_Long > 0 ~ Start_Long * -1,
                                      TRUE ~ Start_Long)) %>%
  mutate(p_type = case_when(Plot_Number < 16 ~ "Fixed",
                          TRUE ~ "Rotational"))

Understory <- Understory %>%
  mutate(Cycle = as.factor(Cycle),
         Plot_Number = as.factor(Plot_Number),
         Stratum = replace_na(Stratum, "No_Veg"),
         Nativity = replace_na(Nativity, "No_Veg"))

#.-----------------------------------------------------
#   ***** Sections *****  ----
#......................................................

# Sections start with most general (total cover) and proceed 
#  to most specific (species x plot) 

# ""Cover" dataframe is the main dataset used at beginning of 
#   each following sections: 

# 1) Total % cover            (Tot_Cov)
# 2) Nativity Total % cover   (Nat_Cov)
# 3) Nativity Richness        (Nat_Rich)
# 4) Species % Cover          (Spp_Cov)
# 5) Species Presence         (Spp_Pres)


#.-----------------------------------------------------
# 1) Total % cover ---- 
#.......................................................

# Total can be greater Than 100%
Tot_Cov <- Understory %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, 
           Point, Stratum) %>%
  # Count hits at each cover point:
  summarise(Hits_All_Sp = n(), .groups = 'drop')  %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum) %>%
  #Total hits at each point for each Stratum for entire plot 
  # (can be > 300 points or >100%)
  summarise(tot_pct_cov = (sum(Hits_All_Sp)) / 300 * 100, .groups = 'drop')

#........DENSITY PLOT ---- 
means <- Tot_Cov %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Stratum) %>%
  summarise(mean = mean(tot_pct_cov), .groups = 'drop') %>%
  mutate(mean = round(mean,1))

 
ggplot(Tot_Cov, aes(x = tot_pct_cov, fill = Cycle)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = means, aes(xintercept = mean, color= Cycle)) +
  geom_text(data = means, aes(x = mean, label =mean), 
            y = 0.01, angle = 90, vjust = -0.2, size = 3) +
  scale_fill_brewer(palette="Accent") +
  scale_color_brewer(palette="Accent") +
  facet_grid(rows = vars(Stratum), cols = vars(Sampling_Frame)) +
  xlim(0, max(Tot_Cov$tot_pct_cov) + 20)

#........BAR COV/PLOT# ----
Tot_Cov %>%
  #filter(Plot_Type == "Fixed") %>%
  ggplot(aes(x = reorder_within(Plot_Number, desc(tot_pct_cov), Stratum), 
             y = tot_pct_cov, fill = Cycle)) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette="Accent") +
  facet_wrap(vars(Stratum), dir = "v", scales = "free_x") +
  ylab("Total % Cover") + xlab("Plot Number") +
  scale_x_reordered()

#........Trend ----
Tot_Cov %>%
  #filter(Plot_Type == "Fixed") %>%
  #filter(Plot_Number == 10) %>%
  ggplot(aes(x = Cycle, y = tot_pct_cov, group = Plot_Number)) +
  geom_line(aes(color = Plot_Number), na.rm = TRUE) +
  geom_point(aes(color = Plot_Number), size = 1, na.rm = TRUE) +
  facet_wrap(vars(Stratum), dir = "v", scales = "free_x") 

# Change ----
Tot_Cov_Chg <- Tot_Cov %>%
  filter(Plot_Type == "Fixed") %>%
  pivot_wider(names_from = Cycle, values_from = tot_pct_cov) %>%
  mutate(chg2v1 = round(`2` - `1`, 2),
         chg3v2 = round(`3` - `2`, 2),
         chg3v1 = round(`3` - `1`, 2)) %>%
  # If plot not sampled in 1st cycle (2015) use change between 3rd and 2nd Cycle:
  mutate(chg3v1 = case_when(!is.na(chg3v1) ~ chg3v1, 
         TRUE ~ chg3v2))


#........BAR CHG/PLOT# ----
Tot_Cov_Chg %>%
  mutate(direction = case_when(chg3v1 > 0 ~ "Pos",
                               chg3v1 < 0 ~ "Neg" )) %>%
  ggplot(aes(x = reorder_within(Plot_Number, desc(chg3v1), Stratum), 
             y = chg3v1, fill = direction)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("#CC0000", "#009900")) +
  #scale_fill_brewer(palette="Accent") +
  #facet_grid(rows = vars(Stratum), cols = vars(Sampling_Frame)) +
  facet_wrap(vars(Stratum), dir = "v", scales = "free_x") +
  scale_x_reordered() +
  xlab("Plot Number") + ylab("Change in Total % Cover") +
  theme(legend.position = "none")

#........JITTER PLOT ----

# Calculate range for count_ha_chg so that it can be plotted correctly in Jitter plot.
Tot_Cov_Chg_range <- Tot_Cov_Chg %>%
  group_by(Sampling_Frame, Stratum) %>%
  summarize(y_range = max(abs(chg3v1),  na.rm = TRUE)) %>%
  ungroup()
# Add range column to Chg dataset  
Tot_Cov_Chg2 <- Tot_Cov_Chg %>%
  inner_join(Tot_Cov_Chg_range)


# Total Cover Change jitter plot
Tot_Cov_Chg.jitter <- Tot_Cov_Chg2 %>%
  ggplot(aes(x =Sampling_Frame, y = chg3v1, label = Plot_Number)) +
  geom_blank(aes(y = y_range)) +
  geom_blank(aes(y = -y_range)) +
  geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  geom_jitter(width = 0.05) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 8, color = "red") +
  labs(y = "Change (% Cover)") +
  facet_wrap(vars(Stratum), nrow = 1, scales = "free") +
  # geom_text_repel(force=1, point.padding=unit(1,'lines'),
  #                 hjust=1, size = 3,
  #                 direction='x',
  #                 nudge_y=0.1,
  #                 segment.size=0.7) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
Tot_Cov_Chg.jitter

#........STRIP CHRT PAIR -----
Tot_Cov.strip <- Tot_Cov %>%
  filter(Plot_Type == "Fixed") %>%
  #select(-count_pp) %>%
  ungroup() %>%
  ggplot(aes(x=Cycle, y=tot_pct_cov, group=Plot_Number)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('') +
  ylab('% Cover') +
  facet_wrap(vars(Stratum), scales = "free", nrow = 1)
Tot_Cov.strip

#........STRIP/JITTER MULTI -----
grid.arrange(Tot_Cov.strip, Tot_Cov_Chg.jitter, nrow = 2, top = "Total Cover")


# ...Summary Stats ----

Tot_Cov_Stats <- add.stats2(
  .data =  Tot_Cov_Chg,
  Unit_Code, Sampling_Frame, Stratum)


#........BAR YEARLY MEANS----
Tot_Cov_Stats %>%
  ggplot(aes(x = PARAM, y = MEAN, fill = PARAM)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "% Cover") +
  facet_wrap(vars(Stratum), scales = "free_x") +
  #scale_fill_manual(values = nativity_colors) +
  xlab("Sample Cycle") +
  theme(legend.title = element_blank())


#........BAR CHG----
Tot_Cov_Stats %>%
  #filter(Cycle == "CHG") %>%
  #filter(Stratum == "UNDERSTORY1") %>%
  ggplot(aes(x = PARAM, y = MEAN, fill = PARAM)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Change in Total % Cover") +
  
  #scale_fill_manual(values = nativity_colors) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank())

# ....... Bar: SF compare % cover ----
#' *notes*
# Compare total percent cover of plots to other sampling frames.
#' *Note: subset options may need adjusted to add other sampling frames to graph*

# Tot_Cov_Stats %>%
#   filter(Cycle == '2') %>%
#   mutate(highlight = case_when(Unit_Code == "AMME" ~ 'AMME',
#                                Unit_Code != "AMME" ~ 'Other')) %>%
#   ggplot(aes(x = reorder_within(Sampling_Frame, -MEAN, Stratum), 
#              y = MEAN, 
#              fill = highlight)) +
#   geom_bar(stat="identity") + #, position = position_dodge()
#   geom_errorbar(aes(ymin=L, ymax=R), width=.2) + #,position=position_dodge(.9)
#   labs(x = "Sampling Frame", y = "% Cover") +  
#   scale_x_reordered() +
#   theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.33)) +
#   guides(fill=FALSE) +
#   scale_fill_manual(values = c("#FF3300", "#736F6E")) +
#   facet_wrap( ~Stratum, scales = 'free_x', dir = "v")


 

#.-----------------------------------------------------
# 2) Nativity Total % cover ----
#.......................................................

# Nativity discrete scale Colors:
nativity_colors <- c("Native" = "#1b9e77", "No_Veg" = "grey", "Non-Native" = "#d95f02", "Unknown" = "#7570b3")

# Can Total Greater Than 100%

# Calculate Total Percent Cover for Native vs. Non-native
Nat_Cov <- Understory %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, 
           Point, Stratum, Nativity) %>%
  # Count hits at each cover point:
  summarise(Hits_All_Nat = n(), .groups = 'drop')  %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum, Nativity) %>%
  #Total hits at each point for each Stratum for entire plot 
  # (can be > 300 points or >100%)
  summarise(tot_pct_cov = (sum(Hits_All_Nat)) / 300 * 100, .groups = 'drop')

#  ........ BAR COV/PLOT----
Nat_Cov %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Nativity != "Unknown") %>%
  ggplot(aes(x = reorder_within(Plot_Number, -tot_pct_cov, within = Nativity), 
             y = tot_pct_cov, fill = Cycle)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_brewer(palette="Accent") +
  facet_grid(rows = vars(Stratum), cols = vars(Nativity), scales = "free") +
  scale_x_reordered() +
  xlab("Plot Number")


# ...Change ----

# Calculate Change in Total Percent Cover for Native & Non-Native Frequency
Nat_Cov_Chg <- Nat_Cov %>%
  filter(Plot_Type == "Fixed") %>%
  complete(nesting(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number), 
           nesting(Nativity, Stratum),  
           fill = list(tot_pct_cov = 0)) %>%
  pivot_wider(names_from = Cycle, values_from = tot_pct_cov) %>%
    mutate(chg2v1 = round(`2` - `1`, 2),
           chg3v2 = round(`3` - `2`, 2),
           chg3v1 = round(`3` - `1`, 2)) %>%
  # If plot not sampled in 1st cycle (2015) use change between 3rd and 2nd Cycle:
  mutate(chg3v1 = case_when(!is.na(chg3v1) ~ chg3v1, 
                            TRUE ~ chg3v2))

#  ........ BAR CHG/PLOT----

Nat_Cov_Chg %>%
  mutate(Plot_Number = reorder_within(Plot_Number, -chg3v1, 
                                      list(Stratum, Nativity))) %>%
  ggplot(aes(x = Plot_Number, y = chg3v1, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Stratum + Nativity, scales = "free_x") +
  scale_fill_manual(values = nativity_colors) +
  scale_x_reordered() +
  xlab("Plot Number") + ylab("Change in % Cover")


#........QUAD NAT COVER----
UnderNativityCover.plot.nat_v_non(cover.stat = "tot_pct_cov", 
                                  sample_frame = "Olaa", 
                                  sample_cycle = 3, 
                                  paired_change = TRUE, 
                                  combine_strata = TRUE)

UnderNativityCover.plot.nat_v_non(cover.stat = "chg_per_cycle", 
                                  sample_frame = "Olaa", 
                                  sample_cycle = 3, 
                                  paired_change = TRUE, 
                                  combine_strata = TRUE)

#........QUAD COVER Map---------------------------------------------------------

pal <- colorFactor(
  palette = c('blue', 'red'),
  domain = ftpc_xy$p_type)

leaflet(ftpc_xy) %>% 
  addProviderTiles(providers$OpenTopoMap) %>%
  addCircleMarkers(
    radius = 8,
    color = ~pal(p_type),
    stroke = FALSE, fillOpacity = 0.4,
    label = ftpc_xy$Plot_Number,
    labelOptions = labelOptions(noHide = T, 
                                direction = "center", 
                                textOnly = T,
                                style = list("color" = "white")))

   
# ...Summary Stats ----
#Nat_Cov_Chg <- Nat_Cov_Chg %>%
#  select(Unit_Code, Sampling_Frame, Plot_Number, Nativity, Stratum, 
#         # Span of change (example 2nd vs 1st Cycle) 
#         # Currently set to 3rd Cycle vs 1st Cycle (tot_pct_cov_chg_all)
#         # If first Cycle not sampled (NA), than 3rd vs 2nd inserted
#         tot_pct_cov_chg_all) 

Nat_Cov_Stats <- add.stats2(
  .data =  Nat_Cov_Chg,
  Unit_Code, Sampling_Frame, Stratum, Nativity)


  

#........BAR YEARLY MEANS----
Nat_Cov_Stats %>%
  #filter(Cycle != "CHG") %>%
  ggplot(aes(x = PARAM, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "% Cover") +
  facet_wrap(vars(Stratum, Nativity), scales = "free_x") +
  scale_fill_manual(values = nativity_colors) +
  xlab("Sample Cycle") +
  theme(legend.title = element_blank())


#........BAR CHG----
Nat_Cov_Stats %>%
  #filter(Cycle == "CHG") %>%
  #filter(Stratum == "UNDERSTORY1") %>%
  ggplot(aes(x = PARAM, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Change in Total % Cover") +
  facet_wrap(vars(fct_rev(Stratum))) +
  scale_fill_manual(values = nativity_colors) 
  #theme(axis.title.x=element_blank(),
  #      axis.text.x=element_blank(),
  #      axis.ticks.x=element_blank(),
  #      legend.title = element_blank())

#ggsave(here("figs", "bar_mean_cov_chg_nativity.png"))  


#........JITTER PLOT ----

# Unknown Nativity Removed
unks <- tally(Nat_Cov, Nativity == "Unknown" & tot_pct_cov != 0) 
paste("Nat_Cov Unknowns Removed =", unks)
Nat_Cov_no_unks <- Nat_Cov %>%
  filter(Nativity != "Unknown")

unks <- tally(Nat_Cov_Chg, Nativity == "Unknown" & chg3v1 != 0) 
paste("Nat_Cov_Chg Unknowns Removed =", unks)
Nat_Cov_Chg_no_unks <- Nat_Cov_Chg %>%
  filter(Nativity != "Unknown")


# Calculate range for count_ha_chg so that it can be plotted correctly in Jitter plot.
Nat_Cov_Chg_range <- Nat_Cov_Chg_no_unks %>%
  group_by(Sampling_Frame, Stratum, Nativity) %>%
  summarize(y_range = max(abs(chg3v1),  na.rm = TRUE)) %>%
  ungroup()
# Add range column to Chg dataset  
Nat_Cov_Chg_no_unks <- Nat_Cov_Chg_no_unks %>%
  inner_join(Nat_Cov_Chg_range)

# Nativity Cover Change jitter plot
Nat_Cov_Chg.jitter <- Nat_Cov_Chg_no_unks %>%
  filter(Nativity != "Unknown") %>%
  ggplot(aes(x =Sampling_Frame, y = chg3v1, label = Plot_Number)) +
  geom_blank(aes(y = y_range)) +
  geom_blank(aes(y = -y_range)) +
  geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  geom_jitter(width = 0.05) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 8, color = "red") +
  labs(y = "Cycle 3v1 Change (% Cover)") +
  #facet_wrap(vars(Stratum), nrow = 1, scales = "free") +
  facet_wrap(vars(Stratum, Nativity), nrow = 1, scales = "free_x") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
Nat_Cov_Chg.jitter

#........STRIP CHRT PAIR -----
Nat_Cov.strip <- Nat_Cov_no_unks %>%
  filter(Plot_Type == "Fixed") %>%
  #select(-count_pp) %>%
  ungroup() %>%
  ggplot(aes(x=Cycle, y=tot_pct_cov, group=Plot_Number)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('') +
  ylab('% Cover') +
  facet_wrap(vars(Stratum, Nativity), nrow = 1, scales = "free_x") 
Nat_Cov.strip

#........STRIP/JITTER MULTI -----
grid.arrange(Nat_Cov.strip, Nat_Cov_Chg.jitter, nrow = 2, top = "Total Cover")


#.-----------------------------------------------------
# 3) Nativity Richness ----
#.......................................................

Nat_Rich <- Understory %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum, Nativity) %>%
  summarise(richness = n_distinct(Code)) 

unks <- tally(Nat_Rich, Nativity == "Unknown") 
paste("Nat_Cov Unknowns Removed =", unks)
Nat_Rich_no_unks <- Nat_Rich %>%
  filter(Nativity != "Unknown",
         Nativity != "No_Veg")

#........STRIP CHRT PAIR -----


Nat_Rich_no_unks %>%
  filter(Plot_Type  == "Fixed") %>%
  ggplot(aes(x=Cycle, y=richness, group=Plot_Number)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('Sample Cycle') +
  ylab('Understory Richness') +
  theme_bw() +
  facet_grid(cols = vars(Nativity), rows = vars(Stratum)) 


# ...Change ----

# Calculate Change in Total Percent Cover for Native & Non-Native Frequency
Nat_Rich_Chg <- Nat_Rich %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(Cycle, Unit_Code, Sampling_Frame, Plot_Number, Stratum, Nativity,  
           fill = list(richness = 0)) %>%
  pivot_wider(names_from = Cycle, values_from = richness) %>%
  mutate(richness_chg = round(`2` - `1`, 2))

#  ........STRIP CHRT CHG----
Nat_Rich_Chg %>%
  ggplot(aes(x = Sampling_Frame, y=richness_chg)) +
  geom_jitter(width = 0.05) +
  ylab('Change in richness') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_summary(fun=mean, geom="point", shape=95,
               size=8, color="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1) +
  facet_grid(cols = vars(Nativity), rows = vars(Stratum)) 


#........QUAD NAT RICH----
plt.r <- max(c(abs(max(Nat_Rich_Chg$richness_chg)), 
             abs(min(Nat_Rich_Chg$richness_chg))))

Nat_Rich_Chg %>%
  select(-`1`, -`2`) %>%
  pivot_wider(names_from = Nativity, values_from = richness_chg) %>%
  ggplot(aes(x = Native, y = `Non-Native`)) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "green", alpha = .25) + 
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "red", alpha = .25) +
  geom_point() +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) +
  xlim(min(-plt.r),max(plt.r)) +
  ylim(max(plt.r), min(-plt.r)) +
  facet_wrap(vars(Stratum), dir = "v") +
  ylab("Change in Non-Native Richness") +
  xlab("Change in Native Richness") 

# ...Summary Stats ----

# Use custom function at top of script to add stats to dataset
Nat_Rich_Stats <- add.stats(
  .data =  Nat_Rich_Chg,
  .summary_var = richness_chg,
  Unit_Code, Sampling_Frame, Stratum, Nativity)

#........BAR YEARLY MEANS----
Nat_Rich_Stats %>%
  ggplot(aes(x = Cycle, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "richness") +
  facet_wrap(vars(Stratum, Nativity), scales = "free_x") +
  scale_fill_brewer(palette="Dark2") 

#........BAR CHG----
nrs <- Nat_Rich_Stats %>%
  filter(Cycle == "CHG") %>%
  filter(Nativity != "Unknown") %>%
  ggplot(aes(x = Cycle, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Change in richness") +
  facet_wrap(vars(fct_rev(Stratum))) +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")
nrs
#ggsave(here("figs", "bar_mean_nat_rich_chg.png")) 
#grid.arrange(ncs, nrs, nrow = 1)

#.-----------------------------------------------------
# 4) Lifeform Total % cover ----
#.......................................................
#'* Optional Filter  * 
 Forms_Cov_Filter <- Cover %>%
  # Take 'Nativity' and 'Life_form' to make plural category (ex. 'Native Shrubs') 
   mutate(NLF = paste0(Nativity, " ", Life_form, "s")) #%>%
#   filter(Plot_Number %in% c(2,4,6,9)) %>%
#   filter(Nativity != "Unknown")
  #drop_na(Name)

# Calculate Total Percent Cover for Native vs. Non-native Lifeforms
Forms_Cov <- Forms_Cov_Filter %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Stratum, Nativity, Life_form, NLF) %>%
  # Count hits at each cover point:
  summarise(Hits_All_Forms = n(), .groups = 'drop')  %>%
  # But don't count record if entire plot had no hits: (e.g. transect is NA )
  mutate(Hits_All_Forms = replace(Hits_All_Forms, is.na(Transect), 0)) %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, Stratum, Nativity, 
           Life_form, NLF) %>%
  #Total hits at each point for each Stratum for entire plot 
  # (can be > 300 points or >100%)
  summarise(tot_pct_cov = (sum(Hits_All_Forms)) / 300 * 100, .groups = 'drop')

# Calculate lifeform Cover (same as lines above) - but combine Stratum 1 & 2
Forms_Cov_1a2 <- Forms_Cov_Filter %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number,
           Transect, Point, Nativity, Life_form, NLF) %>%
  summarise(Hits_All_Forms = n_distinct(n()), .groups = "drop") %>% #remove dbl counts
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Nativity, Life_form, NLF) %>%
  #Total hits at each point for each Stratum for entire plot 
  # (cannot be greater than 100%)
  summarise(tot_pct_cov = (sum(Hits_All_Forms)) / 300 * 100, .groups = 'drop')

#  ........ BAR COV/PLOT----
Forms_Cov %>%
  ggplot(aes(x = reorder_within(Plot_Number, -tot_pct_cov, within = NLF), 
             y = tot_pct_cov, fill = Cycle)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_brewer(palette="Accent") +
  facet_grid(rows = vars(Stratum), cols = vars(NLF), scales = "free") +
  #facet_wrap(vars(Stratum), dir = "v", scales = "free_x") +
  scale_x_reordered() +
  xlab("Plot Number")


# ...Change ----

# Calculate Change in Total Percent Cover for Native & Non-Native Frequency
Forms_Cov_Complete <- Forms_Cov %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(Cycle, nesting(Unit_Code, Sampling_Frame, Plot_Number, Stratum, 
                            Nativity, Life_form, NLF),  
           fill = list(tot_pct_cov = 0)) 

Forms_Cov_Chg <- Forms_Cov_Complete %>%
  pivot_wider(names_from = Cycle, values_from = tot_pct_cov) %>%
  mutate(tot_pct_cov_chg = round(`2` - `1`, 2))  

# Calculate Change (Stratum 1&2 Combined)
Forms_Cov_Complete_1a2 <- Forms_Cov_1a2 %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(Cycle, nesting(Unit_Code, Sampling_Frame, Plot_Number, 
                            Nativity, Life_form, NLF),  
           fill = list(tot_pct_cov = 0)) 

Forms_Cov_Chg_1a2 <- Forms_Cov_Complete_1a2 %>%
  pivot_wider(names_from = Cycle, values_from = tot_pct_cov) %>%
  mutate(tot_pct_cov_chg = round(`2` - `1`, 2))  



#  ........ BAR CHG/PLOT----
Forms_Cov_Chg %>%
  mutate(Plot_Number = reorder_within(Plot_Number, -tot_pct_cov_chg, 
                                      list(Stratum, Nativity, Life_form))) %>%
  ggplot(aes(x = Plot_Number, y = tot_pct_cov_chg, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Stratum + Life_form, scales = "free_x") +
  scale_fill_brewer(palette="Dark2") +
  scale_x_reordered() +
  xlab("Plot Number") + ylab("Change in % Cover")


#........JITTER PLOT ----
Forms_Cov_Chg %>%
  #filter(Plot_Number %in% c(2,4,6,9)) %>%
  ggplot(aes(x =Sampling_Frame, y = tot_pct_cov_chg, color=Nativity)) +
  geom_jitter(width = 0.05) +
  geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 95, size = 8, color = "red") +
  facet_grid(vars(Stratum), vars(Life_form)) +
  scale_color_brewer(palette="Dark2") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#........STRIP CHRT PAIR -----
Forms_Cov_Complete %>%
  #mutate(Status = fct_rev(Status)) %>%
  ggplot(aes(x=Cycle, y=tot_pct_cov, 
             group=interaction(Plot_Number, Nativity), 
             color = Nativity)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('Sample Cycle') +
  ylab('Total % Cover') +
  scale_color_brewer(palette="Dark2") +
  facet_grid(cols = vars(Life_form), rows = vars(Stratum))


# ...Summary Stats ----

# Use custom function at top of script to add stats to dataset
Forms_Cov_Stats <- add.stats(
  .data =  Forms_Cov_Chg,
  .summary_var = tot_pct_cov_chg,
  Unit_Code, Sampling_Frame, Stratum, Nativity, Life_form, NLF)

# Add stats to dataset (Stratum 1&2 Combined)
Forms_Cov_Stats_1a2 <- add.stats(
  .data =  Forms_Cov_Chg_1a2,
  .summary_var = tot_pct_cov_chg,
  Unit_Code, Sampling_Frame, Nativity, Life_form, NLF)


#........BAR LF CHG 1a2 ----

fcs.means1a2 <- Forms_Cov_Stats_1a2 %>%
  filter(Cycle == "CHG") %>%
  #filter(MEAN > 0.5 | MEAN < -0.5) %>%
  mutate(tot_pct_cov_chg = MEAN) %>%
  mutate(ERR = case_when(NPLOTS < 4 ~ NA_real_ , TRUE ~ ERR)) %>%
  droplevels() 
  
fcs.NLF_1a2 <- unique(fcs.means1a2$NLF)
fcs.NLF_1a2

Forms_Cov_Chg_1a2 %>%
  filter(NLF %in% fcs.NLF_1a2) %>%
  ggplot(aes(x = reorder(NLF, tot_pct_cov_chg), y = tot_pct_cov_chg, fill = Nativity)) +
  geom_linerange(data = fcs.means1a2, aes(ymin = MEAN - ERR, ymax = MEAN + ERR)) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, dodge.width = 0.8),
    shape = 21, size = 1, alpha = 0.5) +
  geom_bar(data = fcs.means1a2, colour = "black", stat="identity", alpha = 0.5) +
  geom_hline(yintercept=0, linetype="solid", 
             color = "black", size=0.5) +
  labs(title="", y="Change in % Cover", x="Nativity + Lifeform") +
  scale_fill_brewer(palette="Dark2") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  #scale_color_discrete() +
  #name = NULL, values = c("Specificity" = "black")) +
  theme(legend.title=element_blank()) +
  coord_flip()



#........BAR YEARLY MEANS----
Forms_Cov_Stats %>%
  filter(Nativity != "Unknown") %>%
  ggplot(aes(x = Cycle, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "% Cover") +
  facet_wrap(vars(Stratum, Life_form), scales = "free_x") +
  scale_fill_brewer(palette="Dark2") 


#........BAR CHG----
ncs <- Forms_Cov_Stats %>%
  filter(Nativity != "Unknown") %>%
  filter(Cycle == "CHG") %>%
  ggplot(aes(x = Cycle, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Change in Total % Cover") +
  facet_wrap(vars(fct_rev(Stratum), Life_form)) +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ncs
 


#.-----------------------------------------------------
# 5) Species % Cover ----
#.......................................................

#'* Optional Filter  * 
Spp_Cov_Filter <- Cover #%>%
  #filter(Plot_Number %in% c(2,4,6,9)) %>%
  #drop_na(Name)

# Calculate Total Percent Cover for Native vs. Non-native
Spp_Cov <- Spp_Cov_Filter %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Stratum, Nativity, Code, Name, Life_form) %>%
  # Count hits at each cover point (will be '1' for each species)
  summarise(Hits_Sp = n(), .groups = 'drop')  %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, Stratum, 
           Nativity, Code, Name, Life_form) %>%
  #Total hits at each point for each Stratum for entire plot 
  # (cannot be greater than 100%)
  summarise(pct_cov_sp = (sum(Hits_Sp)) / 300 * 100, .groups = 'drop') 

# Calculate Spp Cover (same as lines above) - but combine Stratum 1 & 2
Spp_Cov_1a2 <- Spp_Cov_Filter %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Nativity, Code, Name, Life_form) %>%
  summarise(Hits_Sp = n_distinct(n()), .groups = "drop") %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Nativity, Code, Name, Life_form) %>%
  #Total hits at each point for each Stratum for entire plot 
  # (cannot be greater than 100%)
  summarise(pct_cov_sp = (sum(Hits_Sp)) / 300 * 100, .groups = 'drop')
  



# ...Change ----


# Calculate Change in Pct Cover by Species
Spp_Cov_Chg <- Spp_Cov %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(Cycle, nesting(Unit_Code, Sampling_Frame, Plot_Number, Stratum, 
           Nativity, Code, Name, Life_form),
           fill = list(pct_cov_sp = 0)) %>%
  pivot_wider(names_from = Cycle, values_from = pct_cov_sp) %>%
  mutate(pct_cov_sp_chg = round(`2` - `1`, 3))  

# Calculate Change in Pct Cover by Species (Stratum 1 and 2 combined)
Spp_Cov_Chg_1a2 <- Spp_Cov_1a2 %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(Cycle, nesting(Unit_Code, Sampling_Frame, Plot_Number, 
                            Nativity, Code, Name, Life_form),
           fill = list(pct_cov_sp = 0)) %>%
  pivot_wider(names_from = Cycle, values_from = pct_cov_sp) %>%
  mutate(pct_cov_sp_chg = round(`2` - `1`, 3)) 

#........JITTER PLOT ----
Spp_Cov_Chg %>%
  droplevels() %>%
  ggplot(aes(x =Sampling_Frame, y = pct_cov_sp_chg, color=Name)) +
  geom_jitter(width = 0.05) +
  geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 95, size = 8, color = "red") +
  facet_grid(vars(Stratum), vars(Life_form)) 


#........STRIP CHRT PAIR -----
Spp_Cov %>%
  #mutate(Status = fct_rev(Status)) %>%
  ggplot(aes(x=Cycle, y=pct_cov_sp, 
             group=interaction(Plot_Number, Name), 
             color = Name)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('Sample Cycle') +
  ylab('Total % Cover') +
  facet_grid(cols = vars(Life_form), rows = vars(Stratum))


# ...Summary Stats ----

# Use custom function at top of script to add stats to dataset
Spp_Cov_Stats <- add.stats(
  .data =  Spp_Cov_Chg,
  .summary_var = pct_cov_sp_chg, 
  Unit_Code, Sampling_Frame, Stratum, Nativity, Code, Name, Life_form)

# Add stats to dataset (Stratum 1&2 Combined)
Spp_Cov_Stats_1a2 <- add.stats(
  .data =  Spp_Cov_Chg_1a2,
  .summary_var = pct_cov_sp_chg,
  Unit_Code, Sampling_Frame, Nativity, Code, Name, Life_form)

#........BAR SPP CHG  ----

p.means <- Spp_Cov_Stats %>%
  filter(Cycle == "CHG") %>%
  filter(MEAN > 0.5 | MEAN < -0.5) %>%
  mutate(pct_cov_sp_chg = MEAN) %>%
  mutate(ERR = case_when(NPLOTS < 4 ~ NA_real_ , TRUE ~ ERR))
p.code <- p.means$Code

p <- Spp_Cov_Chg %>%
  filter(Code %in% p.code) %>%
  mutate(Stratum = fct_rev(Stratum)) %>%
  ggplot(aes(x = reorder(Name, pct_cov_sp_chg), y = pct_cov_sp_chg, fill = Nativity)) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, dodge.width = 0.8),
    shape = 21, size = 1, alpha = 0.5) +
  geom_bar(data = p.means, colour = "black", stat="identity", alpha = 0.5) +
  geom_linerange(data = p.means, aes(ymin = MEAN - ERR, ymax = MEAN + ERR)) +
  geom_hline(yintercept=0, linetype="solid", 
             color = "black", size=0.5) +
  labs(title="", y="Change in % Cover", x="Species") +
  facet_grid(,vars(Stratum)) +
  scale_fill_brewer(palette="Dark2") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.title=element_blank()) +
  coord_flip()
p
#png("spp_cov_chg.png", width = 800, height = 375)
#plot(p)
#dev.off()


#........BAR SPP CHG 1a2 ----

p.means1a2 <- Spp_Cov_Stats_1a2 %>%
  filter(Cycle == "CHG") %>%
  filter(MEAN > 0.5 | MEAN < -0.5) %>%
  mutate(pct_cov_sp_chg = MEAN) %>%
  mutate(ERR = case_when(NPLOTS < 4 ~ NA_real_ , TRUE ~ ERR)) %>%
  droplevels() 
p.code1a2 <- p.means1a2$Code


Spp_Cov_Chg_1a2 %>%
  filter(Code %in% p.code1a2) %>%
  ggplot(aes(x = reorder(Name, pct_cov_sp_chg), y = pct_cov_sp_chg, fill = Nativity)) +
  geom_linerange(data = p.means1a2, aes(ymin = MEAN - ERR, ymax = MEAN + ERR)) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, dodge.width = 0.8),
    shape = 21, size = 1, alpha = 0.5) +
  geom_bar(data = p.means1a2, colour = "black", stat="identity", alpha = 0.5) +
  geom_hline(yintercept=0, linetype="solid", 
             color = "black", size=0.5) +
  labs(title="", y="Change in % Cover", x="Species") +
  scale_fill_brewer(palette="Dark2") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  #scale_color_discrete() +
  #name = NULL, values = c("Specificity" = "black")) +
  theme(legend.title=element_blank()) +
  coord_flip()







#........TREE MAP  ----

# Calculate Total Proportion of Hits
# Calculate Spp Cover (but combine Stratum 1 & 2)
Spp_Hits <- Cover %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Nativity, Code, Name, Life_form) %>%
  summarise(Hits_Sp = n_distinct(n()), .groups = "drop") %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(Cycle, Unit_Code, Sampling_Frame, 
           Nativity, Code, Name, Life_form) %>%
  #Total hits at each point for each Stratum for entire plot 
  # (cannot be greater than 100%)
  summarise(tot_hits = (sum(Hits_Sp)), .groups = 'drop') %>%
  filter(Cycle == "2")

Spp_Hits <- Spp_Hits %>%
  drop_na(Name)
  

library(treemapify)

tree.plot <- ggplot(Spp_Hits, aes(area = tot_hits, subgroup = Nativity, 
                     fill = Nativity, label = Name)) +
  geom_treemap(size = 2) +
  geom_treemap_subgroup_border(colour = "gray30", size = 5, alpha = 0.5) +
  geom_treemap_subgroup_text(place = "bottom", colour = "gray30", alpha = 0.5, 
                             grow = F, size = 20) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "middle",
                    grow = TRUE, reflow = T, min.size = 5) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position = "none")

tree.plot #if this throws "Error in 1:row_n : argument of length 0", than check
# for NA's in the dataset. 

# Add lifeforms to treemap
tree.plot2 <- ggplot(Spp_Hits, aes(area = tot_hits, subgroup = Nativity, 
                                  subgroup2 = Life_form, fill = Nativity, label = Name)) +
  geom_treemap(size = 1) +
  geom_treemap_subgroup_border(colour = "gray30", size = 5, alpha = 0.5) +
  geom_treemap_subgroup2_border(colour = "black", size = 3) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "middle",
                    grow = F, reflow = T, min.size = 5) +
  geom_treemap_subgroup2_text(
    colour = "black",alpha = 1, fontface = "italic", 
    size = 10, place = "bottomright") +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position="bottom", legend.title = element_blank())

tree.plot2 #if this throws "Error in 1:row_n : argument of length 0", than check
# for NA's in the dataset. 

#png(here("figures","tree_plot_230x160r180.png"), width = 230, height = 160, 
#    units = 'mm', res = 180)
#plot(tree.plot)
#dev.off()

# # Average Cover Treemap
# Spp_Cov %>%
#   filter(Cycle == "2") %>%
#   filter(Stratum == "UNDERSTORY1") %>%
#   complete(Unit_Code, Sampling_Frame, Plot_Number, nesting(Code, Nativity),
#            fill = list(pct_cov_sp = 0)) %>%
#   group_by(Unit_Code, Sampling_Frame, Code, Nativity) %>%
#   summarise(mean_cov_sp = mean(pct_cov_sp),
#             n = n()) %>%
#   ggplot(aes(area = mean_cov_sp, fill = Nativity, label = Code)) +
#   geom_treemap() +
#   geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
#                     grow = TRUE) +
#   scale_fill_brewer(palette="Dark2")

#........JITTER PLOT SPP  ----
Spp_Cov_Chg %>%
  mutate(code_lab = case_when(pct_cov_sp_chg <= -5 ~ Code,
                              pct_cov_sp_chg >= 5 ~ Code,
                              TRUE ~ "")) %>%
  filter(pct_cov_sp_chg != 0) %>%
  ggplot(aes(x = Plot_Number, y=pct_cov_sp_chg, 
             color = Nativity, label = code_lab)) +
  geom_jitter(width = 0.3) +
  geom_text(size = 3, hjust=1, vjust=0) +
  geom_hline(yintercept = 0) +
  scale_color_brewer(palette="Dark2") +
  facet_wrap(vars(Stratum), dir = "v", scales = "free_x")

# ........SLOPE PLOT ALL ----
library(ggrepel)

Spp_Slope <- Spp_Cov_Chg %>%
  filter(`1` > 0 | `2` > 0) %>%
  mutate(Plot = Plot_Number) %>%
  mutate(Understory = str_sub(Stratum,-1,-1)) %>%
  mutate(Direction = case_when(pct_cov_sp_chg < 0 ~ "DECREASE",
                               pct_cov_sp_chg >= 0  ~ "INCREASE")) %>%
  mutate(code_lab = case_when(`1` >= 5 ~ Code,
                              `2` >= 5 ~ Code,
                              TRUE ~ "")) %>%
  mutate(Direction = as.factor(Direction)) 


ggplot(Spp_Slope) +
  geom_segment(aes(x=1, xend=2, y=`1`, yend=`2`, 
                   col=Nativity), size=.75, show.legend=T) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  labs(x="", y="% Cover") +  
  xlim(.5, 2.5) + ylim(0,(1.1*(max(Spp_Slope$`1`, Spp_Slope$`2`)))) +
  facet_grid(vars(Stratum), vars(Plot), labeller = label_both) +
  geom_text_repel(label=Spp_Slope$code_lab,
                  y=Spp_Slope$`1`, x=rep(1, NROW(Spp_Slope)), hjust=1.1, size=3, direction = "y") +
  geom_text(label="2012", x=1, y=1.1*(max(Spp_Slope$`1`, Spp_Slope$`2`)), hjust=1.2, size=4.5) +
  geom_text(label="2017", x=2, y=1.1*(max(Spp_Slope$`1`, Spp_Slope$`2`)), hjust=-0.1, size=4.5) +
  guides(color=guide_legend("")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(),axis.text.x = element_blank()) +
  scale_color_brewer(palette="Dark2") 

# ........SLOPE PLOT = X ----

Spp_Slope_X <- Spp_Cov_Chg %>%
  filter(`1` > 0 | `2` > 0) %>%
  mutate(Plot = Plot_Number) %>%
  mutate(Understory = str_sub(Stratum,-1,-1)) %>%
  mutate(Direction = case_when(pct_cov_sp_chg < 0 ~ "DECREASE",
                               pct_cov_sp_chg >= 0  ~ "INCREASE")) %>%
  mutate(code_lab = case_when(`1` >= 5 ~ Code,
                              `2` >= 5 ~ Code,
                              TRUE ~ "")) %>%
  mutate(Direction = as.factor(Direction)) %>%
  filter(Plot_Number == "2" |
           Plot_Number == "3" )
#filter(Code == "CIBGLA")


ggplot(Spp_Slope_X) +
  geom_segment(aes(x=1, xend=2, y=`1`, yend=`2`, 
                   col=Nativity), size=.75, show.legend=T) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  labs(x="", y="% Cover") +  
  xlim(.5, 2.5) + ylim(0,(1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)))) +
  facet_grid(vars(Stratum), vars(Plot), labeller = label_both) +
  geom_text_repel(label=Spp_Slope_X$code_lab,
                  y=Spp_Slope_X$`1`, x=rep(1, NROW(Spp_Slope_X)), hjust=1.1, size=3, direction = "y") +
  geom_text(label="2012", x=1, y=1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)), hjust=1.2, size=4.5) +
  geom_text(label="2017", x=2, y=1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)), hjust=-0.1, size=4.5) +
  guides(color=guide_legend("")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(),axis.text.x = element_blank()) +
  scale_color_brewer(palette="Dark2") 

# ........SLOPE SP = X ----

Spp_Slope_X <- Spp_Cov_Chg %>%
  filter(`1` > 0 | `2` > 0) %>%
  mutate(Plot = Plot_Number) %>%
  mutate(Understory = str_sub(Stratum,-1,-1)) %>%
  mutate(Direction = case_when(pct_cov_sp_chg < 0 ~ "DECREASE",
                               pct_cov_sp_chg >= 0  ~ "INCREASE")) %>%
  mutate(code_lab = case_when(`1` >= 5 ~ Code,
                              `2` >= 5 ~ Code,
                              TRUE ~ "")) %>%
  mutate(Direction = as.factor(Direction)) %>%
  filter(Code == "FESRUB")


ggplot(Spp_Slope_X) +
  geom_segment(aes(x=1, xend=2, y=`1`, yend=`2`, 
                   col=Direction), size=.75, show.legend=T) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  labs(x="", y="% Cover") +  
  xlim(.5, 2.5) + ylim(0,(1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)))) +
  facet_grid(vars(Stratum), vars(Plot), labeller = label_both) +
  geom_text_repel(label=Spp_Slope_X$code_lab,
                  y=Spp_Slope_X$`1`, x=rep(1, NROW(Spp_Slope_X)), hjust=1.1, size=3, direction = "y") +
  geom_text(label="2012", x=1, y=1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)), hjust=1.2, size=4.5) +
  geom_text(label="2017", x=2, y=1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)), hjust=-0.1, size=4.5) +
  guides(color=guide_legend("")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(),axis.text.x = element_blank()) +
  scale_color_manual(values = c("#CC0000", "#009900")) 

# ........BAR SP = X ----
sp.x <- "POAPRA"
Spp_Cov %>%
  filter(Code == sp.x) %>%
  ggplot(aes(x = reorder_within(Plot_Number, desc(pct_cov_sp), Stratum), y = pct_cov_sp, fill = Cycle)) +
  geom_col(position = position_dodge()) +
  ggtitle(sp.x) +
  scale_fill_brewer(palette="Accent") +
  facet_wrap(vars(Stratum), dir = "v", scales = "free_x") +
  ylab("Total % Cover") + xlab("Plot Number") +
  scale_x_reordered()

#........TABLE DECLINES ----
declines <- Spp_Cov_Stats %>%
  filter(Cycle == "CHG") %>%
  mutate(
    chg_dir = case_when(
      MEAN > 0 ~ "Increase",
      MEAN < 0 ~ "Decrease",
      MEAN == 0 ~ "No Change")) %>%
  group_by(Unit_Code, Sampling_Frame, Stratum, Nativity, chg_dir) %>%
  summarize(chg_dir_count = n()) %>%
  add_tally(chg_dir_count) %>%
  mutate(pct_of_species = chg_dir_count/n)

head(declines)


#.-----------------------------------------------------
# 5) Species Presence ----
#.......................................................

Spp_Pres <- Cover %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number,
           Nativity, Code, Name, Life_form) %>%
  summarise(present = n_distinct(Name), .groups = 'drop') %>%
  complete(Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           nesting(Nativity, Code, Name, Life_form),
           fill = list(present = 0)) %>%
  group_by(Cycle, Unit_Code, Sampling_Frame,
           Nativity, Code, Name, Life_form) %>%
  summarise(plots_pres = sum(present), 
            n = n(), .groups = 'drop')


# ...Change ----

Spp_Pres_Chg <- Spp_Pres %>%
  pivot_wider(names_from = Cycle, values_from = plots_pres) %>%
  mutate(presence_chg = round(`2` - `1`, 2)) %>%
  mutate(prop_plts = presence_chg/n)

#........POINT CHG SPP ----
Spp_Pres_Chg %>%
  ggplot(aes(Sampling_Frame, prop_plts, colour = Nativity)) + 
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1) +
  geom_jitter(width = .3, size=1) +
  #facet_grid(,vars(Stratum)) + 
  #geom_count() +
  scale_colour_brewer(palette="Dark2") + 
  labs(title="Change in Landscape Frequency", y="Change in proportion of Plots", x="Sampling Frame", caption="(each point represents one species)") +
  guides(color=guide_legend("Plant Species")) 


#........BAR CHG SPP ----
Spp_Pres_Chg %>%
  filter(presence_chg > 0 | presence_chg < 0) %>%
  ggplot(aes(x = reorder(Name, presence_chg), presence_chg)) +   
  geom_bar(aes(fill = Nativity), 
           position = position_dodge2(width = 0.9, preserve = "single"), 
           stat="identity") +
  geom_hline(yintercept=0, linetype="solid", 
             color = "black", size=0.5) +
  scale_fill_brewer(palette="Dark2") +
  labs(title="", y="Plots (Change in Understory Presence)", x="Species", caption= paste('(',Spp_Pres_Chg$n[1],' paired plots)', sep='')) +
  guides(fill=guide_legend("")) +
  scale_y_continuous(breaks = seq(min(Spp_Pres_Chg$presence_chg),
                                  max(Spp_Pres_Chg$presence_chg), by = 1)) +  #spacebetween tick mark and tick label ('unit')
  coord_flip() 

  

rmarkdown::render("Resource_Brief.Rmd")
