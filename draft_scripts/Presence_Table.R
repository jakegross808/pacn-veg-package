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

# Create Table for Native species Plot Presence ----

Presence <- FilterPACNVeg("Presence", sample_frame  = "Olaa") %>%
  dplyr::filter(QA_Plot == FALSE) %>%
  dplyr::group_by(Sampling_Frame, Cycle) %>%
  dplyr::mutate(Year = min(Year)) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Plot_Number = as.factor(Plot_Number))

# For Table, change all "Rotational Alternate" to "Rotational"
Presence <- Presence %>%
  dplyr::mutate(Plot_Type = dplyr::case_when(Plot_Type == "Rotational Alternate" ~ "Rotational",
                                 TRUE ~ as.character(Plot_Type)))

Presence2 <- Presence %>%
  dplyr::select(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Scientific_Name, Code, Life_Form, Nativity) %>%
  tidyr::unite(col = Cycle_Year, c("Cycle", "Year"))

# Calculate n for fixed, rotational, and all plots per sample_frame per year
calc_n <- Presence2 %>%
  dplyr::select(Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Number, Plot_Type) %>%
  dplyr::distinct() %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Type) %>%
  dplyr::summarise(Plot_Type_N = n())
calc_n2 <- calc_n %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year) %>%
  dplyr::summarize(All = sum(Plot_Type_N)) %>%
  tidyr::pivot_longer(cols = All, names_to = "Plot_Type", values_to = "Plot_Type_N") %>%
  dplyr::bind_rows(calc_n) %>%
  dplyr::ungroup()

Presence2_rotational <- Presence2 %>%
  dplyr::filter(Plot_Type == "Rotational") %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Type, Scientific_Name, Code, Life_Form, Nativity) %>%
  dplyr::summarise(Rotational_Plots = n()) %>%
  dplyr::left_join(calc_n2) %>%
  dplyr::mutate(Rotational_Prop = Rotational_Plots/Plot_Type_N)

Presence2_all<- Presence2 %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Scientific_Name, Code, Life_Form, Nativity) %>%
  dplyr::summarise(All_Plots = n()) %>%
  dplyr::mutate(Plot_Type = "All") %>%
  dplyr::left_join(calc_n2) %>%
  dplyr::mutate(All_Prop = All_Plots/Plot_Type_N)

fixed <- Presence2 %>%
  dplyr::filter(Plot_Type == "Fixed") %>%
  droplevels()

fixed_plts_not_sampled <- fixed %>%
  dplyr::ungroup() %>%
  select (Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Number) %>%
  dplyr::distinct() %>%
  mutate(Sampled = 1) %>%
  tidyr::complete(Plot_Number, tidyr::nesting(Unit_Code, Community, Sampling_Frame, Cycle_Year)) %>%
  dplyr::filter(is.na(Sampled)) %>%
  dplyr::mutate(Sampled = FALSE)


Presence2_fixed <- fixed %>%
  ##dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Scientific_Name, Code, Life_Form, Nativity, Plot_Type) %>%
  ##dplyr::summarise(Plots = n()) %>%
  # if species recorded one year, than any other year it was not recorded...
  # it will show up as zero cover
  dplyr::mutate(Present = 1) %>%
  tidyr::complete(Cycle_Year, tidyr::nesting(Unit_Code, Community, Sampling_Frame,
                                             Plot_Type, Plot_Number,
                                             Scientific_Name, Code, Life_Form, Nativity),
                  fill = list(Present = 0)) %>%
  dplyr::left_join(fixed_plts_not_sampled) %>%
  mutate(Present = dplyr::case_when(Sampled == FALSE ~ as.numeric(NA),
                                    TRUE ~ as.numeric(Present))) %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame,
           Plot_Type, Plot_Number,
           Scientific_Name, Code, Life_Form, Nativity) %>%
  dplyr::arrange(Cycle_Year, .by_group = TRUE) %>%
  dplyr::mutate(Chg_Paired_Plot = Present - dplyr::lag(Present, order_by = Cycle_Year)) %>%
  dplyr::mutate(Chg_Paired_Plot_Pos = dplyr::case_when(Chg_Paired_Plot >= 0 ~ as.numeric(Chg_Paired_Plot),
                                                TRUE ~ as.numeric(NA))) %>%
  dplyr::mutate(Chg_Paired_Plot_Neg = dplyr::case_when(Chg_Paired_Plot <= 0 ~ as.numeric(Chg_Paired_Plot),
                                                TRUE ~ as.numeric(NA))) %>%
  dplyr::ungroup()


# Summarize from plot to sampling frame
Presence3_fixed <- Presence2_fixed %>%
  dplyr::group_by(Cycle_Year, Unit_Code, Community, Sampling_Frame, Plot_Type,
           Code, Scientific_Name, Life_Form, Nativity) %>%
  dplyr::summarize(Fix_Plots = sum(Present, na.rm = TRUE),
            Chg_Paired_Net = sum(Chg_Paired_Plot, na.rm = TRUE),
            Chg_Paired_Pos = sum(Chg_Paired_Plot_Pos, na.rm = TRUE),
            Chg_Paired_Neg = sum(Chg_Paired_Plot_Neg, na.rm = TRUE), .groups = "drop") %>%
  select(-Plot_Type)

Presence4 <- Presence2_all %>%
  dplyr::left_join(Presence2_rotational) %>%
  full_join(Presence3_fixed) %>%
  mutate(All_Plots = dplyr::case_when(is.na(All_Plots) ~ 0, TRUE ~ as.numeric(All_Plots))) %>%
  mutate(Rotational_Plots = dplyr::case_when(is.na(Rotational_Plots) ~ 0, TRUE ~ as.numeric(Rotational_Plots)))
# not running code below because any 'Fixed_Plots == NA' means the species has never been
# found in a fixed plot, only a rotational plot.
  #mutate(Fixed_Plots = dplyr::case_when(is.na(Fixed_Plots) ~ 0, TRUE ~ as.numeric(Fixed_Plots)))

Presence5 <- Presence4 %>%
  tidyr::pivot_wider(names_from = Cycle_Year,
                     values_from = c(All_Plots, Rotational_Plots, Fix_Plots,
                                     Chg_Paired_Net, Chg_Paired_Pos, Chg_Paired_Neg)) %>%
  dplyr::mutate(Cumulative_Net = sum(across(dplyr::starts_with("Chg_Paired_Net")), na.rm = TRUE)) %>%
  dplyr::select(!dplyr::starts_with("Chg_Paired_Pos")) %>%
  dplyr::select(!dplyr::starts_with("Chg_Paired_Neg"))

write_csv(Presence5, file = "C:/Users/JJGross/Downloads/Spp_Presence_Table.csv")


presence_table <- Presence4 %>%
  dplyr::select(-Chg_Paired_Neg, -Chg_Paired_Pos) %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Scientific_Name, Code, Life_Form, Nativity) %>%
  dplyr::summarise(All_Plots = as.character(htmltools::as.tags(sparkline::sparkline(All_Plots))),
                   Rotational_Plots = as.character(htmltools::as.tags(sparkline::sparkline(Rotational_Plots))),
                   Fix_Plots = as.character(htmltools::as.tags(sparkline::sparkline(Fix_Plots))),
                   Cumulative_Net = sum(Chg_Paired_Net, na.rm = TRUE))

presence_table$All_Plots <- gsub('values\":null,','values\":[0,0],',presence_table$All_Plots)
presence_table$Rotational_Plots <- gsub('values\":null,','values\":[0,0],',presence_table$Rotational_Plots)
presence_table$Fix_Plots <- gsub('values\":null,','values\":[0,0],',presence_table$Fix_Plots)

tbl <- formattable::formattable(presence_table,
                                list(Cumulative_Net = pm_color_bar2(color1 = "lightblue", color2 = "lightblue",
                                                               text_color1 = "darkblue", text_color2 = "darkblue", text_color3 = "gray",
                                                               transform_fn = function(x) {
                                                                 max_x <- max(abs(max(x, na.rm = TRUE)), abs(min(x, na.rm = TRUE)))
                                                                 x <- x/max_x
                                                               },
                                                               na.rm = TRUE))) %>%
  formattable::as.datatable(rownames = TRUE,
               selection = "multiple",
               options = list(dom = "tif",
                              paging = FALSE,
                              scrollY = "200px",
                              scrollCollapse = TRUE)) %>%
  htmltools::tagList() %>%
  htmltools::attachDependencies(htmlwidgets:::widget_dependencies("sparkline","sparkline")) %>%
  htmltools::browsable()
tbl
