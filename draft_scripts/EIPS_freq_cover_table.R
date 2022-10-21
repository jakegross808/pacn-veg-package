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
         Tran_Spp_Freq, Tran_Mean_Cover_Min, Tran_Mean_Cover_Max) %>%
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
                  fill = list(Tran_Spp_Freq = 0,
                              Tran_Mean_Cover_Min = 0,
                              Tran_Mean_Cover_Max = 0)) %>%
  group_by(dplyr::across(grp_vars)) %>%
  dplyr::arrange(Cycle_Year, .by_group = TRUE) %>%
  dplyr::mutate(Chg_Prior_Freq = Tran_Spp_Freq - dplyr::lag(Tran_Spp_Freq, order_by = Cycle_Year),
                Chg_Prior_Cov_Min = Tran_Mean_Cover_Min - dplyr::lag(Tran_Mean_Cover_Min, order_by = Cycle_Year),
                Chg_Prior_Cov_Max = Tran_Mean_Cover_Max - dplyr::lag(Tran_Mean_Cover_Max, order_by = Cycle_Year)) %>%
  ungroup()


EIPS_frequency5 <- dplyr::bind_rows(EIPS_freq_fixed, EIPS_freq_rotationals) %>%
  tidyr::separate(Cycle_Year, c("Cycle", "Year"))

# Summarize from transect to sampling frame
EIPS_frequency6 <- EIPS_frequency5 %>%
  group_by(Cycle, Year, Unit_Code, Community, Sampling_Frame,
           Code, Scientific_Name, Life_Form, Nativity) %>%
  summarize(Frequency = mean(Tran_Spp_Freq),
            Transects_Present = sum(!is.na(Tran_Spp_Freq)),
            Cover_Min = mean(Tran_Mean_Cover_Min),
            #Cover_Min_n = sum(!is.na(Mean_Cover_Min)),
            Cover_Max = mean(Tran_Mean_Cover_Max),
            #Cover_Max_n = sum(!is.na(Mean_Cover_Max)),
            Paired_Transects = sum(!is.na(Chg_Prior_Freq)),
            Chg_Frequency = mean(Chg_Prior_Freq, na.rm = TRUE),
            Chg_Cover_Min = mean(Chg_Prior_Cov_Min, na.rm = TRUE),
            #Chg_Cover_Min_n = sum(!is.na(Chg_Prior_Cov_Min)),
            Chg_Cover_Max = mean(Chg_Prior_Cov_Max, na.rm = TRUE))
            #Chg_Cover_Max_n = sum(!is.na(Chg_Prior_Cov_Max)))




write_csv(EIPS_frequency5, file = "C:/Users/JJGross/Downloads/EIPS_summary_table_transects.csv")

write_csv(EIPS_frequency6, file = "C:/Users/JJGross/Downloads/EIPS_summary_table.csv")

#https://github.com/renkun-ken/formattable/issues/95#issuecomment-792387356
bg <- function(start, end, color, ...) {
  paste("linear-gradient(90deg, transparent ",formattable::percent(start),",",
        color, formattable::percent(start), ",", formattable::percent(end),
        ", transparent", formattable::percent(end),")")
}

#https://github.com/renkun-ken/formattable/issues/95#issuecomment-792387356
pm_color_bar2 <- function(color1 = "pink", color2 = "lightgreen", text_color1 = "darkred", text_color2 = "darkgreen", text_color3 = "grey", transform_fn = function(x) {x}, ...){
  formattable::formatter("span",
            style = function(x) {
              x <- transform_fn(x)
              formattable::style(
              display = "inline-block",
              color = ifelse(x > 0,text_color1,ifelse(x < 0,text_color2,text_color3)),
              "text-align" = ifelse(x > 0, 'left', ifelse(x < 0, 'right', 'center')),
              "width"='100%',
              "background" = bg(ifelse(x >= 0, 0.5, 0.5 + (x/2)),
                                ifelse(x >= 0, (x/2) + 0.5, 0.5),
                                ifelse(x >= 0, color1, color2))
            )})
}

# Table w/conditional formatting
EIPS_frequency6 %>%
  ungroup() %>%
  dplyr::select(Unit_Code, Community, Sampling_Frame, Scientific_Name, Life_Form, Chg_Frequency, Chg_Cover_Max) %>%
  dplyr::arrange(Chg_Frequency) %>%
  dplyr::mutate(Chg_Frequency = ifelse(Chg_Frequency == "NaN", NA, Chg_Frequency),
                Chg_Frequency = round(Chg_Frequency, 2),
                Chg_Frequency = formattable::percent(Chg_Frequency, 1)) %>%
  dplyr::mutate(Chg_Cover_Max = ifelse(Chg_Cover_Max == "NaN", NA, Chg_Cover_Max),
                Chg_Cover_Max = round(Chg_Cover_Max, 2),
                Chg_Cover_Max = formattable::percent(Chg_Cover_Max, 1)) %>%
  dplyr::filter(!is.na(Chg_Frequency)) %>%
  formattable::formattable(list(
    Chg_Frequency = pm_color_bar2(na.rm = TRUE),
    Chg_Cover_Max = pm_color_bar2(na.rm = TRUE)
)) %>%
  formattable::as.datatable(rownames = FALSE,
               selection = "multiple",
               options = list(dom = "tif",
                              paging = FALSE,
                              scrollY = "200px",
                              scrollCollapse = TRUE))
