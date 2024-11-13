# To run test use:
library(pacnvegetation)
library(tidyverse)



LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_1_2021_20211208.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
            cache = TRUE, force_refresh = FALSE)

cover_nativity <- summarize_understory(paired_change = FALSE,
                                              sample_frame = "Olaa",
                                              plant_grouping = "Nativity")
cover_nativity <- cover_nativity %>%
  dplyr::group_by(Sampling_Frame, Cycle) %>%
  mutate(Year = min(Year)) %>%
  mutate(Year = as.factor(Year)) %>%
  dplyr::ungroup()


paired_change_natvnon <- summarize_understory(paired_change = TRUE,
                                              sample_frame = "Olaa",
                                              plant_grouping = "Nativity")

# Nativity discrete scale Colors:
nativity_colors <- c("Native" = "#1b9e77", "No_Veg" = "grey", "Non-Native" = "#d95f02", "Unknown" = "#7570b3")

cover_nat_stat <- add_stats(cover_nativity, Unit_Code, Sampling_Frame, Cycle, Year, Stratum, Nativity)

cover_nat_stat_filter <- cover_nat_stat %>%
  filter(Stratum != "No_Veg",
         Nativity != "Unknown")

#........BAR YEARLY MEANS----
cover_nat_stat_filter %>%
  filter(Parameter == "Cover") %>%
  ggplot2::ggplot(aes(x = Year, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  ggplot2::labs(y = "% Cover") +
  facet_wrap(vars(Stratum, Nativity), scales = "free_x") +
  ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
  ggplot2::xlab("Sample Cycle") +
  ggplot2::theme(legend.title = ggplot2::element_blank())


nvn <- paired_change_natvnon %>%
  filter(Stratum != "No_Veg",
         Nativity != "Unknown")

#........BAR YEARLY MEANS----
Nat_Cov_Stats %>%
  #filter(Cycle != "CHG") %>%
  ggplot2::ggplot(aes(x = PARAM, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  ggplot2::labs(y = "% Cover") +
  facet_wrap(vars(Stratum, Nativity), scales = "free_x") +
  ggplot2::scale_fill_manual(values = nativity_colors) +
  ggplot2::xlab("Sample Cycle") +
  ggplot2::theme(legend.title = ggplot2::element_blank())





# Calculate range so that it can be plotted correctly in Jitter plot.
Nat_Cov_Chg_range <- Nat_Cov_Chg_no_unks %>%
  dplyr::group_by(Sampling_Frame, Stratum, Nativity) %>%
  dplyr::summarize(y_range = max(abs(chg3v1),  na.rm = TRUE)) %>%
  dplyr::ungroup()
# Add range column to Chg dataset
Nat_Cov_Chg_no_unks <- Nat_Cov_Chg_no_unks %>%
  inner_join(Nat_Cov_Chg_range)

# Nativity Cover Change jitter plot
Nat_Cov_Chg.jitter <- Nat_Cov_Chg_no_unks %>%
  filter(Nativity != "Unknown") %>%
  ggplot2::ggplot(aes(x =Sampling_Frame, y = chg3v1, label = Plot_Number)) +
  geom_blank(aes(y = y_range)) +
  geom_blank(aes(y = -y_range)) +
  ggplot2::geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  geom_jitter(width = 0.05) +
  stat_summary(fun = median, geom = "point", shape = 95, size = 8, color = "red") +
  ggplot2::labs(y = "Cycle 3v1 Change (% Cover)") +
  #facet_wrap(vars(Stratum), nrow = 1, scales = "free") +
  facet_wrap(vars(Stratum, Nativity), nrow = 1, scales = "free_x") +
  ggplot2::theme(axis.title.x=ggplot2::element_blank(),
        axis.text.x=ggplot2::element_blank(),
        axis.ticks.x=ggplot2::element_blank())
Nat_Cov_Chg.jitter

#........STRIP CHRT PAIR -----
Nat_Cov.strip <- Nat_Cov_no_unks %>%
  filter(Plot_Type == "Fixed") %>%
  #select(-count_pp) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(aes(x=Cycle, y=tot_pct_cov, group=Plot_Number)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  ggplot2::geom_point(position=position_dodge(width=0.2)) +
  ggplot2::xlab('') +
  ggplot2::ylab('% Cover') +
  facet_wrap(vars(Stratum, Nativity), nrow = 1, scales = "free_x")
Nat_Cov.strip

#........STRIP/JITTER MULTI -----
grid.arrange(Nat_Cov.strip, Nat_Cov_Chg.jitter, nrow = 2, top = "Total Cover")










summarize_understory(community = "Subalpine Shrubland", plant_grouping = "None")
summarize_understory(sample_frame = "Olaa", plant_grouping = "Nativity")
summarize_understory(sample_frame = "Olaa", plant_grouping = "Life_Form")
summarize_understory(sample_frame = "Olaa", plant_grouping = "Species")

summarize_understory(sample_frame = "Olaa", plant_grouping = "None", paired_change = TRUE)
summarize_understory(sample_frame = "Olaa", plant_grouping = "Nativity", paired_change = TRUE)
summarize_understory(sample_frame = "Olaa", plant_grouping = "Life_Form", paired_change = TRUE)


look <- summarize_understory(sample_frame = "Olaa", plant_grouping = "Species", paired_change = FALSE)

look_stats <- add_stats(look, Unit_Code, Sampling_Frame, Cycle, Stratum, Nativity, Life_Form, Code, Scientific_Name)

sp_code <- "METPOL1"
look_stats %>%
  mutate(Cycle = as.factor(Cycle)) %>%
  filter(Parameter == "Cover") %>%
  filter(Code == sp_code) %>%
  ggplot2::ggplot(aes(Cycle, MEAN, ymax = R, ymin = L, color = Stratum)) +
  geom_pointrange(position = position_dodge2(width=0.2)) +
  ggplot2::labs(title = sp_code)

look %>%
  mutate(Cycle = as.factor(Cycle)) %>%
  filter(Life_Form == "Tree") %>%
  ggplot2::ggplot(aes(Code, Cover, color = Cycle, fill = Cycle)) +
  ggplot2::geom_boxplot() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

test_All <- Cover_test(plant_grouping = "All")
test_Nat <- Cover_test(plant_grouping = "Nativity")
test_LF <- Cover_test(plant_grouping = "Life_Form")
test_SP <- Cover_test(plant_grouping = "Species")

test_All_change <- Cover_test(plant_grouping = "All", paired_change = TRUE)
test_Nat_change <- Cover_test(plant_grouping = "Nativity", paired_change = TRUE)
test_LF_change <- Cover_test(plant_grouping = "Life_Form", paired_change = TRUE)
test_SP_change <- Cover_test(plant_grouping = "Species", paired_change = TRUE)

add.stats2 <- function(.data, ...){
  # dataset needs to have columns: "Unit_Code, Sampling_Frame, Stratum"
  # column is the dataset$column that shows the change between two cycles

  #.group_vars <- enquos(...)

  params <- .data %>%
    dplyr::select(where(is.numeric)) %>%
    names()

  if (missing(...)) {
    ... <- .data %>%
      dplyr::select(-params) %>%
      names()
  }

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

}

test_SP_stats_fun <- add.stats2(.data =  test_SP_change)

test_SP_stats_fun <- add.stats2(
  .data =  test_SP_change,
  Unit_Code, Sampling_Frame, Cycle, Year, Stratum, Nativity, Life_Form, Code, Scientific_Name)

test_Nat_stats_fun <- add.stats2(
  .data =  test_Nat_change,
  Unit_Code, Sampling_Frame, Cycle, Year, Stratum, Nativity)

test_Nat <- test_Nat %>%
  select(-Year)
test_Nat_stats_fun_rotations <- add.stats2(.data =  test_Nat, Unit_Code, Sampling_Frame, Cycle, Stratum, Nativity)

test_SP_stats_fun %>%
  mutate(Year = as.factor(Year)) %>%
  filter(Code == "CIBGLA") %>%
  filter(PARAM != "Year") %>%
  filter(PARAM != "Cycle") %>%
  filter(Sampling_Frame == "Olaa") %>%
  ggplot2::ggplot(aes(x = Year, y = MEAN, fill = Cycle,)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  ggplot2::labs(y = "% Cover") +
  facet_wrap(vars(Stratum, Sampling_Frame, PARAM), scales = "free_y") +
  #scale_fill_manual(values = nativity_colors) +
  ggplot2::xlab("Sample Cycle") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

test_Nat_stats_fun %>%
  mutate(Year = as.factor(Year)) %>%
  filter(PARAM == "Cover") %>%
  #filter(PARAM != "Year") %>%
  #filter(PARAM != "Cycle") %>%
  #filter(PARAM != "Years_Prior") %>%
  filter(Sampling_Frame == "Olaa") %>%
  ggplot2::ggplot(aes(x = Year, y = MEAN, fill = Nativity,)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  ggplot2::labs(y = "% Cover") +
  facet_wrap(vars(Stratum, Sampling_Frame, PARAM), scales = "free_y") +
  #scale_fill_manual(values = nativity_colors) +
  ggplot2::xlab("Sample Cycle") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

test_Nat_stats_fun_rotations %>%
  #mutate(Year = as.factor(Year)) %>%
  filter(PARAM == "Cover") %>%
  #filter(PARAM != "Year") %>%
  #filter(PARAM != "Cycle") %>%
  #filter(PARAM != "Years_Prior") %>%
  filter(Sampling_Frame == "Olaa") %>%
  ggplot2::ggplot(aes(x = Cycle, y = MEAN, fill = Nativity,)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  ggplot2::labs(y = "% Cover") +
  facet_wrap(vars(Stratum, Sampling_Frame, PARAM), scales = "free_y") +
  #scale_fill_manual(values = nativity_colors) +
  ggplot2::xlab("Sample Cycle") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())
