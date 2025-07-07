# Load ----

library(pacnvegetation)
library(tidyverse)
library(gridExtra)
library(gghighlight)
#library(elucidate) #Error in library(elucidate) : there is no package called ‘elucidate’
#install.packages("elucidate") #package ‘elucidate’ is not available for this version of R

LoadPACNVeg(force_refresh = FALSE, eips_paths = "foo")

names_all <- readr::read_csv(here::here("data", "PACN_veg_names_excel_csv.csv"))

names_nahuku <- names_all |>
  dplyr::filter(Sampling_Frame == "Nahuku/East Rift") |>
  dplyr::mutate(Sampling_Frame = "Nahuku") |>
  dplyr::mutate(Formal_Sampling_Frame = "Nāhuku") |>
  dplyr::mutate(abbr_Sampling_Frame = "NU")

names_all <- names_all |>
  dplyr::bind_rows(names_nahuku)



# Variables ----
var_samp_frames <- c("Kahuku", "Kipahulu District", "Olaa", "Puu Alii", "Nahuku/East Rift")

# Large Trees ----

raw_Lg_Trees <- FilterPACNVeg(data_name = "LgTrees",
                              is_qa_plot = FALSE)

Lg_Trees <- FilterPACNVeg(data_name = "LgTrees",
                          is_qa_plot = FALSE,
                          sample_frame = var_samp_frames) %>%
  filter(Year != 2023) |>
  select(-QA_Plot) |>
  group_by(Sampling_Frame, Cycle) |>
  mutate(Year = max(Year)) |>
  group_by(Sampling_Frame) |>
  mutate(max_cycle = max(Cycle)) |>
  # Get just last cycle if fixed plot
  filter(!(Plot_Type == "Fixed" & Cycle != max_cycle)) |>
  filter(Status == "Live") |>
  select(-Status, -Height_Dead)

if ("Nahuku/East Rift" %in% var_samp_frames) {
  # Temporary solution until zone/mgmt layer incorporated into package:
  print("Removing -East Rift- plots from analysis. Function needs updated when Nahuku/East Rift Cycle 4 data are added")

  nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15, #fixed
                    46, 49, 51, 52, 54, 55, 56, 58, #2021 rotational
                    24, 26, #2010 rotational
                    31, 32, 33, 34, 35, 38, 41, 45)

  #nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15) #fixed only

  Lg_Trees_nahuku_only <- Lg_Trees |>
    dplyr::filter(Sampling_Frame == "Nahuku/East Rift") |>
    dplyr::filter(Plot_Number %in% nahuku_plots) |>
    dplyr::mutate(Sampling_Frame = "Nahuku") |>
    dplyr::mutate(Sampling_Frame_Formal = "Nāhuku")
  Lg_Trees <- Lg_Trees |>
    dplyr::filter(Sampling_Frame != "Nahuku/East Rift") |>
    dplyr::bind_rows(Lg_Trees_nahuku_only)
}

str(Lg_Trees)

Lg_Trees_sum <- Lg_Trees %>%
  mutate(bole_dbh = case_when(is.na(DBH_Bole) ~ DBH,
                              !is.na(DBH_Bole) ~ DBH_Bole,
                              .default = NA)) %>%
  filter(bole_dbh >= 10) %>%
  filter(Nativity != "Unknown") |>
  mutate(bole_ba_m2 = (bole_dbh^2)*0.00007854) %>%
  group_by(Unit_Code, Community, Sampling_Frame, Sampling_Frame_Formal, Year, Cycle, Plot_Number, Scientific_Name, Code, Life_Form, Nativity) %>%
  summarise(stems = n(),
            sum_ba_m2 = sum(bole_ba_m2))

Lg_Trees_nativity_ratio <- Lg_Trees_sum %>%
  group_by(Unit_Code, Community, Sampling_Frame, Sampling_Frame_Formal, Year, Cycle, Plot_Number, Nativity) %>%
  summarise(sum_ba_nativity_m2 = sum(sum_ba_m2),
            sum_nativity_stems = sum(stems)) %>%
  group_by(Unit_Code, Community, Sampling_Frame, Sampling_Frame_Formal, Year, Cycle, Plot_Number) %>%
  mutate(plot_total_ba_m2 = sum(sum_ba_nativity_m2)) %>%
  mutate(sum_total_stems = sum(sum_nativity_stems)) %>%
  mutate(BA_ratio = round((sum_ba_nativity_m2/plot_total_ba_m2)*100, digits = 1)) |>
  mutate(stems_ratio = round((sum_nativity_stems/sum_total_stems)*100, digits = 1))
  #mutate(ratio = sum_ba_nativity_m2/plot_total_ba_m2)

Lg_Trees_species_ratio <- Lg_Trees_sum %>%
  group_by(Unit_Code, Community, Sampling_Frame, Sampling_Frame_Formal, Year, Cycle, Plot_Number, Scientific_Name, Code, Life_Form, Nativity) %>%
  summarise(sum_ba_species_m2 = sum(sum_ba_m2)) %>%
  group_by(Unit_Code, Community, Sampling_Frame, Sampling_Frame_Formal, Year, Cycle, Plot_Number) %>%
  mutate(plot_total_ba_m2 = sum(sum_ba_species_m2)) %>%
  mutate(ratio = round((sum_ba_species_m2/plot_total_ba_m2)*100, digits = 1))
  #mutate(ratio = sum_ba_species_m2/plot_total_ba_m2)

Lg_Trees_nativity_ratio |>
  # Add n to sampling frame label ______________________________________________
  dplyr::group_by(Sampling_Frame) |>
  dplyr::mutate(n = dplyr::n_distinct(Cycle, Plot_Number)) |>
  dplyr::mutate(Sampling_Frame_n = paste0(Sampling_Frame_Formal, "\n [n=", n, "] ")) |>
  #_____________________________________________________________________________
  filter(Nativity == "Native") |>
  left_join(names_all, by = join_by(Sampling_Frame)) |>
  ggplot(aes(fct_reorder(.na_rm = TRUE, Sampling_Frame_n, stems_ratio, min), y = stems_ratio, fill = Nativity)) +
  geom_boxplot() +
  #coord_flip() +
  scale_fill_manual(values=c("#1b9e77")) +
  ggtitle("Proportion of Native Trees (Stems >10 cm DBH)") +
  xlab("Sampling Frame") +
  ylab("%") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  # Add horizontal line at y = 2O
  geom_hline(yintercept=70, linetype="dashed",
           color = "red", linewidth=1)


# Understory ----

## Total cover (% native) ----
nat_und <- summarize_understory(combine_strata = TRUE,
                                plant_grouping = "Nativity",
                                paired_change = FALSE,
                                sample_frame = var_samp_frames)
nat_und <- nat_und |>
  left_join(names_all, by = join_by(Unit_Code, Sampling_Frame)) |>
  mutate(max_cycle = max(Cycle))

if ("Nahuku/East Rift" %in% var_samp_frames) {
  # Temporary solution until zone/mgmt layer incorporated into package:
  print("Removing -East Rift- plots from analysis. Function needs updated when Nahuku/East Rift Cycle 4 data are added")

  nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15, #fixed
                    46, 49, 51, 52, 54, 55, 56, 58, #2021 rotational
                    24, 26, #2010 rotational
                    31, 32, 33, 34, 35, 38, 41, 45)

  #nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15) #fixed only

  nat_und_nahuku_only <- nat_und |>
    dplyr::filter(Sampling_Frame == "Nahuku/East Rift") |>
    dplyr::filter(Plot_Number %in% nahuku_plots) |>
    dplyr::mutate(Sampling_Frame = "Nahuku") |>
    dplyr::mutate(Formal_Sampling_Frame = "Nāhuku")
  nat_und <- nat_und |>
    dplyr::filter(Sampling_Frame != "Nahuku/East Rift") |>
    dplyr::bind_rows(nat_und_nahuku_only)
}

if ("Kahuku" %in% var_samp_frames) {
  # Temporary solution until zone/mgmt layer incorporated into package:
  print("Separating Kahuku plots into Kau and Paddocks. needs updated when Kahuku Cycle 4 data are added")

  paddocks_plots <- c(7:15, #fixed
                    22:30, #2011 rotational
                    32, 36, 38, 40, 42, #2016 rotational
                    47, 48, 49, 51, 53, 54, 56, 60) #2022

  #nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15) #fixed only

  nat_und_paddocks_only <- nat_und |>
    dplyr::filter(Sampling_Frame == "Kahuku") |>
    dplyr::filter(Plot_Number %in% paddocks_plots) |>
    dplyr::mutate(Sampling_Frame = "Paddocks") |>
    dplyr::mutate(Formal_Sampling_Frame = "Kahuku (Paddocks)")
  nat_und_kau_only <- nat_und |>
    dplyr::filter(Sampling_Frame == "Kahuku") |>
    dplyr::filter(!Plot_Number %in% paddocks_plots) |>
    dplyr::mutate(Sampling_Frame = "Kau") |>
    dplyr::mutate(Formal_Sampling_Frame = "Kahuku (Kaʻu)")
  nat_und <- nat_und |>
    dplyr::filter(Sampling_Frame != "Kahuku") |>
    dplyr::bind_rows(nat_und_paddocks_only) |>
    dplyr::bind_rows(nat_und_kau_only)
}


nat_und_ratio <- nat_und %>%
  pivot_wider(names_from = Nativity, values_from = Cover) %>%
  mutate(vegetated = 100 - `NA`) %>%
  mutate(Total_Veg_Cover = (Native+`Non-Native`)) %>%
  mutate(Native_Ratio = round((Native/Total_Veg_Cover)*100, digits = 1)) |>
  mutate(Non_Native_Ratio = round((`Non-Native`/Total_Veg_Cover)*100, digits = 1)) |>
  arrange(-Native_Ratio)

nat_und_ratio$Sampling_Frame <- fct_reorder(nat_und_ratio$Sampling_Frame,
                                            desc(nat_und_ratio$Native_Ratio))

levels(nat_und_ratio$Sampling_Frame)
threshold <- 0.50

# Native Ratio boxplot
nat_und_ratio %>%
  dplyr::filter(!Year %in% c(2023, 2024, 2025)) |>
  # Add n to sampling frame label ______________________________________________
  dplyr::group_by(Sampling_Frame) |>
  dplyr::mutate(n = dplyr::n_distinct(Cycle, Plot_Number)) |>
  dplyr::mutate(Sampling_Frame_n = paste0(Formal_Sampling_Frame, "\n [n=", n, "] ")) |>
  #_____________________________________________________________________________
  #group_by("Unit_Code", "Sampling_Frame", "Cycle", "Year") %>%
  #ggplot() +
  ggplot(aes(x=fct_reorder(.na_rm = TRUE, Sampling_Frame_n, desc(Native_Ratio), median),
             y=Native_Ratio)) +
  geom_boxplot(fill = "#1b9e77") +
  #ggplot(aes(fct_reorder(.na_rm = TRUE, Sampling_Frame_n, Native_Ratio, min), y = Native_Ratio, fill = Nativity)) +
  #scale_fill_manual(values=c("#336600")) +
  ggtitle("Proportion of Native Understory Cover") +
  xlab("Sampling Frame") +
  ylab("%") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) #+
  # Add horizontal line at y = 2O
  #geom_hline(yintercept=70, linetype="dashed",
  #           color = "red", linewidth=1)

# Non_Native Ratio boxplot
nat_und_ratio %>%
  #dplyr::filter(Cycle == 1) |>
  dplyr::filter(!(Plot_Type == "Fixed" & Cycle != max_cycle)) |>
  dplyr::filter(!(Unit_Code ==  "HALE" & Cycle == 3)) |>
  dplyr::filter(!(Unit_Code ==  "KALA" & Cycle == 3)) |>
  # Add n to sampling frame label ______________________________________________
  dplyr::group_by(Sampling_Frame) |>
  dplyr::mutate(n = dplyr::n_distinct(Cycle, Plot_Number)) |>
  dplyr::mutate(Sampling_Frame_n = paste0(Formal_Sampling_Frame, "\n [n=", n, "] ")) |>
  #_____________________________________________________________________________
  #group_by("Unit_Code", "Sampling_Frame", "Cycle", "Year") %>%
  #ggplot() +
  ggplot(aes(x=fct_reorder(.na_rm = TRUE, Sampling_Frame_n, desc(Non_Native_Ratio), median),
             y=Non_Native_Ratio)) +
  geom_boxplot(fill = "#d95f02") +
  #ggplot(aes(fct_reorder(.na_rm = TRUE, Sampling_Frame_n, Native_Ratio, min), y = Native_Ratio, fill = Nativity)) +
  #scale_fill_manual(values=c("#336600")) +
  ggtitle("Proportion of Non-Native Understory Cover") +
  xlab("Sampling Frame") +
  ylab("%") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
# Add horizontal line at y = 2O
  geom_hline(yintercept=17, linetype="dashed",
           color = "red", linewidth=1) +
  geom_hline(yintercept=26.5, linetype="dashed",
             color = "red", linewidth=1)


#Native Ratio x total veg point chart
 nat_und_ratio %>%
   filter(Cycle == 2) %>%
   group_by(Unit_Code, Sampling_Frame, Cycle, Year) %>%
   summarise(median_Native_Ratio = median(Native_Ratio),
             median_Total_Veg_Cover  = median(Total_Veg_Cover )) %>%
   ggplot() +
   geom_point(aes(x=Sampling_Frame, y=median_Native_Ratio, size = median_Total_Veg_Cover)) +
   geom_hline(yintercept = .50, linetype="dashed", color = "black") +
   theme(axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1)) +
   xlab("Sampling Frame") + ylab("Native Ratio (understory)") +
   geom_text(aes(1.5,threshold,label = "50%", vjust = -0.5))




# Trends ----
nat_chg_und <- summarize_understory(combine_strata = TRUE,
                                    plant_grouping = "Nativity",
                                    paired_change = TRUE,
                                    sample_frame = var_samp_frames)

nat_chg_und <- nat_chg_und %>%
  dplyr::filter(Cycle > 1) |>
  left_join(names_all, by = join_by(Sampling_Frame)) |>
  dplyr::rename(Unit_Code = Unit_Code.x) |>
  select(-Unit_Code.y) |>
  mutate(max_cycle = max(Cycle))

if ("Nahuku/East Rift" %in% var_samp_frames) {
  # Temporary solution until zone/mgmt layer incorporated into package:
  print("Removing -East Rift- plots from analysis. Function needs updated when Nahuku/East Rift Cycle 4 data are added")

  nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15, #fixed
                    46, 49, 51, 52, 54, 55, 56, 58, #2021 rotational
                    24, 26, #2010 rotational
                    31, 32, 33, 34, 35, 38, 41, 45)

  #nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15) #fixed only

  nat_chg_und_nahuku_only <- nat_chg_und |>
    dplyr::filter(Sampling_Frame == "Nahuku/East Rift") |>
    dplyr::filter(Plot_Number %in% nahuku_plots) |>
    dplyr::mutate(Sampling_Frame = "Nahuku") |>
    dplyr::mutate(Formal_Sampling_Frame = "Nāhuku")
  nat_chg_und <- nat_chg_und |>
    dplyr::filter(Sampling_Frame != "Nahuku/East Rift") |>
    dplyr::bind_rows(nat_chg_und_nahuku_only)
}

if ("Kahuku" %in% var_samp_frames) {
  # Temporary solution until zone/mgmt layer incorporated into package:
  print("Separating Kahuku plots into Kau and Paddocks. needs updated when Kahuku Cycle 4 data are added")

  paddocks_plots <- c(7:15, #fixed
                      22:30, #2011 rotational
                      32, 36, 38, 40, 42, #2016 rotational
                      47, 48, 49, 51, 53, 54, 56, 60) #2022

  #nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15) #fixed only

  nat_chg_und_paddocks_only <- nat_chg_und |>
    dplyr::filter(Sampling_Frame == "Kahuku") |>
    dplyr::filter(Plot_Number %in% paddocks_plots) |>
    dplyr::mutate(Sampling_Frame = "Paddocks") |>
    dplyr::mutate(Formal_Sampling_Frame = "Kahuku (Paddocks)")
  nat_chg_und_kau_only <- nat_chg_und |>
    dplyr::filter(Sampling_Frame == "Kahuku") |>
    dplyr::filter(!Plot_Number %in% paddocks_plots) |>
    dplyr::mutate(Sampling_Frame = "Kau") |>
    dplyr::mutate(Formal_Sampling_Frame = "Kahuku (Kaʻu)")
  nat_chg_und <- nat_chg_und |>
    dplyr::filter(Sampling_Frame != "Kahuku") |>
    dplyr::bind_rows(nat_chg_und_paddocks_only) |>
    dplyr::bind_rows(nat_chg_und_kau_only)
}

max_y<- round(max(nat_chg_und$Chg_Prior, na.rm = TRUE), digits = -1)
max_y
min_y<- round(min(nat_chg_und$Chg_Prior, na.rm = TRUE), digits = -1)
min_y

get_p <- function(chg_vector) {
  the_test <- t.test(chg_vector, mu = 0)
  the_p <- the_test$p.value
  the_p
}

nat_chg_und_grouped <- nat_chg_und %>%
  group_by(Cycle, Year, Sampling_Frame, Nativity) %>%
  dplyr::filter(!is.na(Chg_Prior)) %>%
  summarize(out_median = median(Chg_Prior, na.rm = TRUE),
            n_plots = n(),
            p_val = get_p(Chg_Prior)) %>%
  mutate(high_low = case_when(out_median > 0 ~ "high",
                              out_median <= 0 ~ "low",
                              .default = NA
  )) %>%
  mutate(Sampling_Frame2 = paste0(Sampling_Frame, " (", n_plots, ")"))

## Trends graph native ----

native_chg_und_cyc2 <- nat_chg_und %>%
  left_join(nat_chg_und_grouped) %>%
  #ungroup() %>%
  filter(Nativity == "Native") %>%
  filter(Cycle == 2) %>%
  #dplyr::filter(Cycle == 1) |>
  dplyr::filter(!(Unit_Code ==  "HALE" & Cycle == 3)) |>
  dplyr::filter(!(Unit_Code ==  "KALA" & Cycle == 3)) |>
  # Add n to sampling frame label ______________________________________________
  dplyr::group_by(Sampling_Frame) |>
  dplyr::mutate(n = dplyr::n_distinct(Cycle, Plot_Number)) |>
  dplyr::mutate(Sampling_Frame_n = paste0(Formal_Sampling_Frame, "\n [n=", n, "] ")) |>
  #_____________________________________________________________________________
  ggplot(aes(x=fct_reorder(.na_rm = TRUE, Sampling_Frame_n, desc(Chg_Prior), median),
             y=Chg_Prior,
             fill = high_low)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Cycle 2-1 (2015-19 vs. 2010-14)") +
  xlab("Sampling Frame") + ylab("NATIVE - Change in % cover") +
  scale_fill_manual(values=c("#336600", "red")) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(min_y, max_y), breaks=seq(min_y,max_y,10)) +
  gghighlight(min(p_val) < 0.05,
              unhighlighted_params = list(color = "#696969", fill = NULL, alpha = 0.15, outlier.alpha = 0.5)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black")
#geom_text(aes(1.5,0,label = "0%", vjust = -0.5))
native_chg_und_cyc2

native_chg_und_cyc3 <- nat_chg_und %>%
  left_join(nat_chg_und_grouped) #%>%
  #ungroup() %>%
  filter(Nativity == "Native") %>%
  filter(Cycle == 3) %>%
  #dplyr::filter(Cycle == 1) |>
  dplyr::filter(!(Unit_Code ==  "HALE" & Cycle == 3)) |>
  dplyr::filter(!(Unit_Code ==  "KALA" & Cycle == 3)) |>
  # Add n to sampling frame label ______________________________________________
  dplyr::group_by(Sampling_Frame) |>
  dplyr::mutate(n = dplyr::n_distinct(Cycle, Plot_Number)) |>
  dplyr::mutate(Sampling_Frame_n = paste0(Formal_Sampling_Frame, "\n [n=", n, "] ")) |>
  #_____________________________________________________________________________
  ggplot(aes(x=fct_reorder(.na_rm = TRUE, Sampling_Frame_n, desc(Chg_Prior), median),
             y=Chg_Prior,
             fill = high_low)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Cycle 3-2 (2021-23 vs. 2015-19)") +
  xlab("Sampling Frame") +
  #ylab("Change in Native cover per year") +
  scale_fill_manual(values=c("#336600", "red")) +
  theme(legend.position="none",
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_y_continuous(limits = c(min_y, max_y), breaks=seq(min_y,max_y,10)) +
  gghighlight(min(p_val) < 0.05,
              unhighlighted_params = list(color = "#696969", fill = NULL, alpha = 0.15, outlier.alpha = 0.5)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black")

native_chg_und_cyc3

# Plot both 2nd and 3rd cycle change on same graph
grid.arrange(native_chg_und_cyc2, native_chg_und_cyc3, nrow = 1, widths = c(2.1,1.3))


## Trends graph Non-native ----

native_chg_und_cyc2 <- nat_chg_und %>%
  left_join(nat_chg_und_grouped) %>%
  #ungroup() %>%
  filter(Nativity == "Non-Native") %>%
  filter(Cycle == 2) %>%
  #dplyr::filter(Cycle == 1) |>
  dplyr::filter(!(Unit_Code ==  "HALE" & Cycle == 3)) |>
  dplyr::filter(!(Unit_Code ==  "KALA" & Cycle == 3)) |>
  # Add n to sampling frame label ______________________________________________
  dplyr::group_by(Sampling_Frame) |>
  dplyr::mutate(n = dplyr::n_distinct(Cycle, Plot_Number)) |>
  dplyr::mutate(Sampling_Frame_n = paste0(Formal_Sampling_Frame, "\n [n=", n, "] ")) |>
  #_____________________________________________________________________________
  ggplot(aes(x=fct_reorder(.na_rm = TRUE, Sampling_Frame_n, desc(Chg_Prior), median),
             y=Chg_Prior,
             fill = high_low)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Cycle 2-1 (2015-19 vs. 2010-14)") +
  xlab("Sampling Frame") + ylab("NON-NATIVE - Change in % cover") +
  scale_fill_manual(values=c("red", "#d95f02")) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(min_y, max_y), breaks=seq(min_y,max_y,10)) +
  gghighlight(min(p_val) < 0.05,
              unhighlighted_params = list(color = "#696969", fill = NULL, alpha = 0.15, outlier.alpha = 0.5)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black")
#geom_text(aes(1.5,0,label = "0%", vjust = -0.5))
native_chg_und_cyc2

native_chg_und_cyc3 <- nat_chg_und %>%
  left_join(nat_chg_und_grouped) %>%
  #ungroup() %>%
  filter(Nativity == "Non-Native") %>%
  filter(Cycle == 3) %>%
  #dplyr::filter(Cycle == 1) |>
  dplyr::filter(!(Unit_Code ==  "HALE" & Cycle == 3)) |>
  dplyr::filter(!(Unit_Code ==  "KALA" & Cycle == 3)) |>
  # Add n to sampling frame label ______________________________________________
  dplyr::group_by(Sampling_Frame) |>
  dplyr::mutate(n = dplyr::n_distinct(Cycle, Plot_Number)) |>
  dplyr::mutate(Sampling_Frame_n = paste0(Formal_Sampling_Frame, "\n [n=", n, "] ")) |>
  #_____________________________________________________________________________
  ggplot(aes(x=fct_reorder(.na_rm = TRUE, Sampling_Frame_n, desc(Chg_Prior), median),
             y=Chg_Prior,
             fill = high_low)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Cycle 3-2 (2021-23 vs. 2015-19)") +
  xlab("Sampling Frame") +
  #ylab("Change in Native cover per year") +
  scale_fill_manual(values=c("red", "#d95f02")) +
  theme(legend.position="none",
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_y_continuous(limits = c(min_y, max_y), breaks=seq(min_y,max_y,10)) +
  gghighlight(min(p_val) < 0.05,
              unhighlighted_params = list(color = "#696969", fill = NULL, alpha = 0.15, outlier.alpha = 0.5)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black")

native_chg_und_cyc3

# Plot both 2nd and 3rd cycle change on same graph
grid.arrange(native_chg_und_cyc2, native_chg_und_cyc3, nrow = 1, widths = c(2.1,1.3))































alien_chg_und_cyc2 <- nat_chg_und %>%
  filter(Nativity == "Non-Native") %>%
  filter(Cycle == 2) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(Chg_Per_Year), median), y=Chg_Per_Year)) +
  geom_boxplot(fill = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  gghighlight(median(Chg_Per_Year, na.rm = TRUE) > 0) +
  ggtitle("Non-Native Understory Cover 2010-14 v. 2015-19") +
  xlab("Sampling Frame") + ylab("Change in cover per year") +
  geom_hline(yintercept = 0, linetype="dashed", color = "black")
#geom_text(aes(1.5,0,label = "0%", vjust = -0.5))
alien_chg_und_cyc2

look <- nat_chg_und %>%
  group_by(Sampling_Frame, Cycle, Nativity) %>%
  summarize(median = median(Chg_Per_Year, na.rm = TRUE))

# SF_native_und_chg_cycle2
native_chg_und <- nat_chg_und %>%
  filter(Nativity == "Native") %>%
  filter(Cycle == 2) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(Chg_Per_Year), median), y=Chg_Per_Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Native Understory Cover 2010-14 v. 2015-19") +
  xlab("Sampling Frame") + ylab("Change in % cover per year")
native_chg_und

# SF_native_und_chg_cycle2
non_native_chg_und <- nat_chg_und %>%
  filter(Nativity == "Non-Native") %>%
  filter(Cycle == 2) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(Chg_Per_Year), median), y=Chg_Per_Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Non-Native Understory Cover 2010-14 v. 2015-19") +
  xlab("Sampling Frame") + ylab("Change in % cover per year")
non_native_chg_und


non_native_chg_und <- nat_chg_und %>%
  filter(Nativity == "Native" | Nativity == "Non-Native") %>%
  filter(Cycle == 2) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(Chg_Per_Year), median), y=Chg_Per_Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Native Understory Cover 2010-14 v. 2015-19") +
  xlab("Sampling Frame") + ylab("Change in % cover per year")
non_native_chg_und

non_native_chg_und <- nat_chg_und %>%
  filter(Nativity == "Non-Native") %>%
  filter(Cycle == 3) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(Chg_Per_Year), median), y=Chg_Per_Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
non_native_chg_und

