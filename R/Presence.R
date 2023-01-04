#' FTPC Species Presence Table
#'
#' Species presence per plot per year for all years available within a sampling frame.
#' min and max plot percentage shown with sparkline showing presence trend for all years
#'
#' @param sample_frame Name of sampling frame
#' @param table_type default is "html" (other option is "tibble")
#'
#' @return A html table
#' @export
#'
#' @examples

v_presence_table <- function(sample_frame, table_type = "html") {
  # Table Prep ----
  # Stadard table prep (should consider moving into FiterPACNVeg)
  Presence <- FilterPACNVeg("Presence", sample_frame = sample_frame) %>%
    dplyr::filter(QA_Plot == FALSE) %>% # Make sure no QA plots
    dplyr::group_by(Sampling_Frame, Cycle) %>%
    dplyr::mutate(Year = min(Year)) %>%
    dplyr::mutate(Year = as.factor(Year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Plot_Number = as.factor(Plot_Number))

  # Front-end preparation for summary
  Presence1 <- Presence %>%
    #change all "Rotational Alternate" to "Rotational"
    dplyr::mutate(Plot_Type = dplyr::case_when(Plot_Type == "Rotational Alternate" ~ "Rotational",
                                   TRUE ~ as.character(Plot_Type))) %>%
    dplyr::select(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Scientific_Name, Code, Life_Form, Nativity) %>%
    # Concatenate Cycle & Year
    tidyr::unite(col = Cycle_Year, c("Cycle", "Year"))

  # Calculate n (total number of plots sampled for fixed, rotational, and all plots
  # per sample_frame per year
  calc_n <- Presence1 %>%
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

  Presence2_all<- Presence1 %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Scientific_Name, Code, Life_Form, Nativity) %>%
    dplyr::summarise(Plots = n()) %>%
    dplyr::mutate(Plot_Type = "All") %>%
    dplyr::left_join(calc_n2) %>%
    dplyr::mutate(Prop = Plots/N) %>%
    dplyr::ungroup()

  Presence2_rotational <- Presence1 %>%
    dplyr::filter(Plot_Type == "Rotational") %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Type, Scientific_Name, Code, Life_Form, Nativity) %>%
    dplyr::summarise(Plots = n()) %>%
    dplyr::left_join(calc_n2) %>%
    dplyr::mutate(Prop = Plots/N) %>%
    dplyr::ungroup()

  Presence2_fixed <- Presence1 %>%
    dplyr::filter(Plot_Type == "Fixed") %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Type, Scientific_Name, Code, Life_Form, Nativity) %>%
    dplyr::summarise(Plots = n()) %>%
    dplyr::left_join(calc_n2) %>%
    dplyr::mutate(Prop = Plots/N) %>%
    dplyr::ungroup()

  #fixed_plts_not_sampled <- Presence2_fixed %>%
  #  select(Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Number) %>%
  #  distinct() %>%
  #  mutate(Sampled = 1) %>%
  #  tidyr::complete(Plot_Number, tidyr::nesting(Unit_Code, Community, Sampling_Frame, Cycle_Year)) %>%
  #  dplyr::filter(is.na(Sampled)) %>%
  #  dplyr::mutate(Sampled = FALSE)

  # Bind tables for "all", "fixed", and "rotational"
  Presence3 <- Presence2_all %>%
    #dplyr::bind_rows(Presence2_rotational) %>% # Can add column for rotationals specifically here
    dplyr::bind_rows(Presence2_fixed)

  # Pivot Values
  Presence4 <- Presence3 %>%
    tidyr::separate(Cycle_Year, into = c("Cycle", "Year")) %>%
    dplyr::select(-Unit_Code, -Community, -Sampling_Frame, -Cycle) %>%
    tidyr::pivot_wider(names_from = Plot_Type,
                       values_from = c(Plots, N, Prop),
                       names_glue = "{Plot_Type}_{.value}",
                       values_fill = 0)

  # Custom functions for calculating percentage in apply() below
  percentage <- function(x) {round(x, digits = 2)}
  percentage2 <- function(x) {formattable::percent(x,digits = 0)}

  # Pivot Years
  Presence5 <- Presence4 %>%
    dplyr::select(Scientific_Name, Code, Nativity, Life_Form, Year, -All_Plots, -All_N, All_Prop, -Fixed_Plots, -Fixed_N, Fixed_Prop) %>%
    tidyr::pivot_wider(names_from = Year,
                       values_from = c(All_Prop, Fixed_Prop),
                       names_glue = "{Year}_{.value}",
                       values_fill = 0) %>%
    dplyr::rename_with(~stringr::str_remove(., '_Prop')) %>%
    dplyr::mutate(across(where(is.numeric),percentage)) %>%
    dplyr::mutate(across(where(is.numeric),percentage2)) %>%
    dplyr::arrange(Scientific_Name)

  if (table_type == "tibble") { #if tibble option selected then code stops here
    out_table <- Presence5
    }

  if (table_type == "html") { #if html option selected then prepare formattable
    # Table Create ----

    tbl <- Presence5
    col_names <- names(Presence5)

    all_years <- as.integer(stringr::str_extract(col_names, "\\d+"))
    all_years <- all_years[!is.na(all_years)]

    min_year <- min(all_years)
    min_year

    max_year <- max(all_years)
    max_year

    other_years <- as.character(all_years) %>%
      stringr::str_remove(as.character(min_year)) %>%
      stringr::str_remove(as.character(max_year)) %>%
      as.integer() %>%
      unique() %>%
      purrr::discard(is.na)
    other_years

    #other_years <- c(2016,2017) # use this to test future cycles
    other_years_min <- min(other_years)
    other_years
    str(other_years_min)

    length(other_years) == 1

    # "_All" Plots Columns ----

    # Get index for "All Plots" data
    all_plots_columns <- grep("_All", colnames(tbl))
    #all_plots_columns <- str_locate(names(tbl), "_All")
    All_index_min <- min(all_plots_columns)
    All_index_max <- max(all_plots_columns)

    # Get name of the first year column between min and max year, use that for the sparkline data
    other_years_min_All_name <- paste0(other_years_min, "_All")
    # Get column name for max and min year
    max_year_All_name <- paste0(max_year, "_All")
    min_year_All_name <- paste0(min_year, "_All")

    # add sparkline data to the one column (sparkline data contains data from all years available)
    tbl[[other_years_min_All_name]] <- apply(tbl[, All_index_min:All_index_max], 1,
                                             FUN = function(x) as.character(
                                               htmltools::as.tags(sparkline::sparkline(as.numeric(x),
                                                                                       type = "line"))))

    # Rename "_All" (all plots) columns
    # Min Year
    names(tbl)[names(tbl) == min_year_All_name] <- paste0("|", min_year)
    # Sparkline data (minimum of other between years)
    names(tbl)[names(tbl) == other_years_min_All_name] <- "All Plots"
    # Max Year
    names(tbl)[names(tbl) == max_year_All_name] <- paste0(max_year, "|")


    #tbl$`2015_Fixed` <- apply(tbl[, 8:10], 1,
    #                        FUN = function(x) as.character(
    #                          htmltools::as.tags(sparkline::sparkline(as.numeric(x),
    #                                                       type = "line"))))

    # "_Fixed" Plots Columns ----

    # Get index for "Fixed Plots" data
    fixed_plots_columns <- grep("_Fixed", colnames(tbl))
    fixed_index_min <- min(fixed_plots_columns)
    fixed_index_max <- max(fixed_plots_columns)

    # Get name of the first year column between min and max year, use that for the sparkline data
    other_years_min_Fixed_name <- paste0(other_years_min, "_Fixed")
    # Get column name for max and min year
    max_year_Fixed_name <- paste0(max_year, "_Fixed")
    min_year_Fixed_name <- paste0(min_year, "_Fixed")

    # add sparkline data to the one column (sparkline data contains data from all years available)
    tbl[[other_years_min_Fixed_name]] <- apply(tbl[, fixed_index_min:fixed_index_max], 1,
                                               FUN = function(x) as.character(
                                                 htmltools::as.tags(sparkline::sparkline(as.numeric(x),
                                                                                         type = "line"))))

    # Rename "_All" (all plots) columns
    # Min Year
    # Use double "||" to distinguish from previous column
    names(tbl)[names(tbl) == min_year_Fixed_name] <- paste0("||", min_year)
    # Sparkline data (minimum of other between years)
    names(tbl)[names(tbl) == other_years_min_Fixed_name] <- "Fixed Plots"
    # Max Year
    # Use double "||" to distinguish from previous column
    names(tbl)[names(tbl) == max_year_Fixed_name] <- paste0(max_year, "||")

    # get column number to use in creation of table below
    len <- length(names(tbl))


    # If only two years in dataset, then move sparkline column to the middle
    # in between the two years:
    if (length(other_years) == 0) {
      tbl <- tbl %>%
        dplyr::relocate("All Plots", .after = paste0("|", min_year)) %>%
        dplyr::relocate("Fixed Plots", .after = paste0("||", min_year))
      }

    tbl_html <- formattable::formattable(tbl,
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

  out_table <- tbl_html

  }
  return(out_table)
}

