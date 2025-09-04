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
  dplyr::summarise(N = n())
calc_n2 <- calc_n %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year) %>%
  dplyr::summarize(All = sum(N)) %>%
  tidyr::pivot_longer(cols = All, names_to = "Plot_Type", values_to = "N") %>%
  dplyr::bind_rows(calc_n) %>%
  dplyr::ungroup()

Presence2_all<- Presence2 %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Scientific_Name, Code, Life_Form, Nativity) %>%
  dplyr::summarise(Plots = n()) %>%
  dplyr::mutate(Plot_Type = "All") %>%
  dplyr::left_join(calc_n2) %>%
  dplyr::mutate(Prop = Plots/N) %>%
  dplyr::ungroup()

Presence2_rotational <- Presence2 %>%
  dplyr::filter(Plot_Type == "Rotational") %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Type, Scientific_Name, Code, Life_Form, Nativity) %>%
  dplyr::summarise(Plots = n()) %>%
  dplyr::left_join(calc_n2) %>%
  dplyr::mutate(Prop = Plots/N) %>%
  dplyr::ungroup()

Presence2_fixed <- Presence2 %>%
  dplyr::filter(Plot_Type == "Fixed") %>%
  dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Type, Scientific_Name, Code, Life_Form, Nativity) %>%
  dplyr::summarise(Plots = n()) %>%
  dplyr::left_join(calc_n2) %>%
  dplyr::mutate(Prop = Plots/N) %>%
  dplyr::ungroup()

fixed_plts_not_sampled <- fixed %>%
  dplyr::ungroup() %>%
  select (Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Number) %>%
  dplyr::distinct() %>%
  mutate(Sampled = 1) %>%
  tidyr::complete(Plot_Number, tidyr::nesting(Unit_Code, Community, Sampling_Frame, Cycle_Year)) %>%
  dplyr::filter(is.na(Sampled)) %>%
  dplyr::mutate(Sampled = FALSE)

Presence4 <- Presence2_all %>%
  #dplyr::bind_rows(Presence2_rotational) %>% # Can add column for rotationals specifically here
  dplyr::bind_rows(Presence2_fixed)

Presence5 <- Presence4 %>%
  tidyr::separate(Cycle_Year, into = c("Cycle", "Year")) %>%
  dplyr::select(-Unit_Code, -Community, -Sampling_Frame, -Cycle) %>%
  tidyr::pivot_wider(names_from = Plot_Type,
                     values_from = c(Plots, N, Prop),
                     names_glue = "{Plot_Type}_{.value}",
                     values_fill = 0)

names(Presence5)

Presence6 <- Presence5 %>%
  dplyr::select(Scientific_Name, Code, Nativity, Life_Form, Year, -All_Plots, -All_N, All_Prop, -Fixed_Plots, -Fixed_N, Fixed_Prop)

Presence7 <- Presence6 %>%
  tidyr::pivot_wider(names_from = Year,
                     values_from = c(All_Prop, Fixed_Prop),
                     names_glue = "{Year}_{.value}",
                     values_fill = 0) %>%
  dplyr::rename_with(~stringr::str_remove(., '_Prop'))

#percentage <- function(x) {round(x * 100, digits = 0)}
percentage <- function(x) {round(x, digits = 2)}
percentage2 <- function(x) {formattable::percent(x,digits = 0)}

# Formatting
Presence8 <- Presence7 %>%
  dplyr::mutate(across(where(is.numeric),percentage)) %>%
  dplyr::mutate(across(where(is.numeric),percentage2)) %>%
  dplyr::arrange(Scientific_Name)
names(Presence8)


#tbl <- formattable::formattable(Presence8)

tbl <- Presence8
tbl
tbl$`2015_All` <- apply(tbl[, 5:7], 1,
                          FUN = function(x) as.character(
                            htmltools::as.tags(sparkline::sparkline(as.numeric(x),
                                                         type = "line"))))
names(tbl)[5] <- "|2010"
names(tbl)[6] <- "All Plots"
names(tbl)[7] <- "2022|"

tbl$`2015_Fixed` <- apply(tbl[, 8:10], 1,
                        FUN = function(x) as.character(
                          htmltools::as.tags(sparkline::sparkline(as.numeric(x),
                                                       type = "line"))))
names(tbl)[8] <- "||2010"
names(tbl)[9] <- "Fixed Plots"
names(tbl)[10] <- "2022||"
len <- length(names(tbl))
#new_tbl <- tbl[, c(1, 2, 3, 4, 5, 7, 8, 9, 10)]

out <- formattable::as.htmlwidget(formattable::formattable(tbl,
                                align = c("l",rep("c", NCOL(tbl) - 1)),
                                list(`Scientific_Name` = formattable::formatter("span", style = ~ formattable::style(color = "grey", font.weight = "bold"))#,
                                     #" " = formatter("span",
                                    #                 style = ~ style(color = ifelse(`2016` > `2011`, "green", "red")),
                                    #                ~ icontext(sapply(` `, function(x) if (x < -1.96) "arrow-down" else if (x> 1.96) "arrow-up" else "")))))
                                    )))
out$dependencies <- c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out

# mess with other table
out2 <- formattable::formattable(tbl,
                                 list(
                                   `Scientific_Name` =
                                     formattable::formatter(
                                       "span",style = ~ formattable::style(
                                         color = "grey", font.weight = "bold")))) %>%
  formattable::as.datatable(rownames = FALSE,
                            selection = "multiple",
                            #filter = "bottom", # filter each column individually
                            options = list(
                              dom = "ltif", # t = table, i = information summary, f = filtering input
                              paging = FALSE,
                              scrollY = "400px",
                              scrollX = TRUE,
                              scrollCollapse = TRUE,
                              columnDefs = list(list(className= 'dt-center',
                                                     targets = -1:-(len-1))))) %>%
  htmltools::tagList() %>%
  htmltools::attachDependencies(htmlwidgets:::widget_dependencies("sparkline","sparkline")) %>%
  htmltools::browsable()

out2





out <- formattable::formattable(tbl,
                                align = c("l",rep("c", NCOL(tbl) - 1)),
                                list(`Scientific_Name` = formattable::formatter("span", style = ~ formattable::style(color = "grey", font.weight = "bold"))))

out2 <- formattable::as.datatable(out,
                                  rownames = TRUE,
                                  selection = "multiple",
                                  options = list(dom = "tif",
                                                 paging = FALSE,
                                                 scrollY = "200px",
                                                 scrollCollapse = TRUE))


out3 <- formattable::as.htmlwidget(out2)
out$dependencies <- c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out

formattable::
sparkline::sparkline()

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
