EIPS_frequency <- v_EIPS_prep(sample_frame  = "Haleakala")

check <- EIPS_frequency %>%
  select(Segment) %>%
  distinct()

segs_per_transect <- EIPS_frequency %>%
  select(Unit_Code, Community, Sampling_Frame, Year, Cycle,
         Transect_Type, Transect_Number, Segment, Seg_Length_m, Tran_Length_m) %>%
  distinct() %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Transect_Type, Transect_Number, Seg_Length_m, Tran_Length_m) %>%
  summarize(actual_segs_per_tran = n()) %>%
  mutate(expected_segs_per_tran = Tran_Length_m/Seg_Length_m)

if (any(segs_per_transect$actual_segs_per_tran < segs_per_transect$expected_segs_per_tran)) {
  warning("Some transects in dataset are incomplete (see records above)")
  print(segs_per_transect[segs_per_transect$actual_segs_per_tran < segs_per_transect$expected_segs_per_tran, ])
}

EIPS_frequency2 <- EIPS_frequency %>%
  dplyr::filter(Cover_Class != "OUT") %>%
  dplyr::filter(Dead != "TRUE")

EIPS_frequency3 <- EIPS_frequency2 %>%
  dplyr::select(Unit_Code, Community, Sampling_Frame, Year, Cycle,
                Transect_Type, Transect_Number, Segment, Seg_Length_m, Tran_Length_m,
                Code, Scientific_Name, Life_Form, Nativity,
                Cov_Range_Min, Cov_Range_Max) %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle,
           Transect_Type, Transect_Number, #Seg_Length_m, Tran_Length_m,
           Code, Scientific_Name, Life_Form, Nativity) %>%
  summarise(Segs_Present = n(),
            Total_Cover_Min = sum(Cov_Range_Min),
            Total_Cover_Max = sum(Cov_Range_Max), .groups = "drop") %>%
  dplyr::left_join(segs_per_transect, by = c("Unit_Code", "Community", "Sampling_Frame", "Year", "Cycle", "Transect_Type", "Transect_Number")) %>%
  dplyr::mutate(Tran_Spp_Freq = Segs_Present/actual_segs_per_tran,
                Tran_Mean_Cover_Min = Total_Cover_Min/Segs_Present,
                Tran_Mean_Cover_Max = Total_Cover_Max/Segs_Present)

trans_per_sf <- EIPS_frequency3 %>%
  select(Unit_Code, Community, Sampling_Frame, Year, Cycle,
         Transect_Type, Transect_Number) %>%
  distinct() %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle) %>%
  summarize(actual_trans_per_sf = n(), .groups = "drop") %>%
  mutate(expected_trans_per_sf = 20)

EIPS_frequency4 <- EIPS_frequency3 %>%
  select(Unit_Code, Community, Sampling_Frame,
         Code, Scientific_Name, Life_Form, Nativity,
         Transect_Type, Transect_Number,
         Year, Cycle,
         Spp_Freq, Mean_Cover_Min, Mean_Cover_Max) %>%
  # Combine Year and Cycle to make it easier to run tidyr::complete() below
  tidyr::unite(col = Cycle_Year, c("Cycle", "Year"))


EIPS_freq_rotationals <- EIPS_frequency4 %>%
  filter(Transect_Type == "Rotational")

# Wanted grp_vars to work in tidyr::nesting() but did not... see below
grp_vars <- c("Unit_Code", "Community", "Sampling_Frame",
              "Transect_Type", "Transect_Number",
              "Code", "Scientific_Name", "Life_Form", "Nativity")

EIPS_freq_fixed <- EIPS_frequency4 %>%
  filter(Transect_Type == "Fixed") %>%
  # if species recorded one year, than any other year it was not recorded...
  # it will show up as zero cover
  tidyr::complete(Cycle_Year, tidyr::nesting(Unit_Code, Community, Sampling_Frame,
                                             Transect_Type, Transect_Number,
                                             Code, Scientific_Name, Life_Form, Nativity),
                  fill = list(Spp_Freq = 0,
                              Mean_Cover_Min = 0,
                              Mean_Cover_Max = 0)) %>%
  group_by(dplyr::across(grp_vars)) %>%
  dplyr::arrange(Cycle_Year, .by_group = TRUE) %>%
  dplyr::mutate(Chg_Prior_Freq = Spp_Freq - dplyr::lag(Spp_Freq, order_by = Cycle_Year),
                Chg_Prior_Cov_Min = Mean_Cover_Min - dplyr::lag(Mean_Cover_Min, order_by = Cycle_Year),
                Chg_Prior_Cov_Max = Mean_Cover_Max - dplyr::lag(Mean_Cover_Max, order_by = Cycle_Year)) %>%
  ungroup()


EIPS_frequency5 <- dplyr::bind_rows(EIPS_freq_fixed, EIPS_freq_rotationals) %>%
  tidyr::separate(Cycle_Year, c("Cycle", "Year"))

# Summarize from transect to sampling frame
EIPS_frequency6 <- EIPS_frequency5 %>%
  group_by(Cycle, Year, Unit_Code, Community, Sampling_Frame,
           Code, Scientific_Name, Life_Form, Nativity) %>%
  summarize(Frequency = mean(Spp_Freq),
            Transects_Present = sum(!is.na(Spp_Freq)),
            Cover_Min = mean(Mean_Cover_Min),
            #Cover_Min_n = sum(!is.na(Mean_Cover_Min)),
            Cover_Max = mean(Mean_Cover_Max),
            #Cover_Max_n = sum(!is.na(Mean_Cover_Max)),
            Paired_Transects = sum(!is.na(Chg_Prior_Freq)),
            Chg_Frequency = mean(Chg_Prior_Freq, na.rm = TRUE),
            Chg_Cover_Min = mean(Chg_Prior_Cov_Min, na.rm = TRUE),
            #Chg_Cover_Min_n = sum(!is.na(Chg_Prior_Cov_Min)),
            Chg_Cover_Max = mean(Chg_Prior_Cov_Max, na.rm = TRUE))
            #Chg_Cover_Max_n = sum(!is.na(Chg_Prior_Cov_Max)))




write_csv(EIPS_frequency5, file = "C:/Users/JJGross/Downloads/EIPS_summary_table_transects.csv")

write_csv(EIPS_frequency6, file = "C:/Users/JJGross/Downloads/EIPS_summary_table.csv")
