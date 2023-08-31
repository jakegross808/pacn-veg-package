#' PACN Master Plant Species List
#'
#' Create a master species list with FTPC species occurrence by plot
#' Column "Field_ID" is also generated in table and can be used in Field Maps Plant Layer
#'
#' @details park filter is required. All other filters are optional. To ignore a filter, omit it or set it to NA.
#'
#' @inheritParams FilterOne
#' @param veg_species_db_path Full path to local copy of PACN_veg_species_list.accdb
#' @param park Four letter unit code of park or can be "All"
#' @param presence_matrix stop function early to return sampling frame vs. species occurrence matrix
#' @param sample_frame Name of sample frame
#' @param community Name of plant community
#' @param year Monitoring year
#' @param cycle Monitoring cycle
#' @param plot_type Type of plot (fixed vs. rotational)
#' @param plot_number Plot number
#' @param nativity Whether species are native (TRUE/FALSE)
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' HALE_pacn_veg_spp_list <- master_spp_list(park = "HALE")
#' native_kaho_spp <- master_spp_list(park = "KAHO", nativity = "Native")
#' HAVO_spp_olaa_only <- master_spp_list(park = "HAVO", sample_frame = "Olaa")
#' }

master_spp_list <- function(db_path, park, presence_matrix = FALSE, sample_frame, community, year, cycle, plot_type, plot_number, nativity) {
  if (missing(park)) {
    stop("'park' argument must be supplied, can be set to park = 'All' or park = 'HAVO', etc.")
  }

  if (park == "All") {
    filter_park = NA
    #filter_park = c("AMME", "HALE", "HAVO", "KAHO", "KALE", "NPSA", "PUHE", "PUHO", "WAPA")
  } else {
    filter_park = park
  }

  #if (length(park)>1) {
  #  stop("only 1 park can be queried at a time")
  #}

  # Create Abbrev Dataframe
  Sampling_Frame <- c("Olaa", "Nahuku/East Rift", "Mauna Loa", "Kahuku", "Kaloko-Honokohau",
                      "Kipahulu District", "Haleakala", "Puu Alii", "Kalawao", "Hoolehua",
                      "Tutuila", "Tau", "Guam", "Muchot")
  SF_Abbrev <- c("OL", "ER",	"ML",	"KU",	"KH",	"KD",	"HA",	"PA",	"KW",	"HO",	"TT",	"TA",	"GU",	"MU")

  Abbrev <- data.frame(Sampling_Frame, SF_Abbrev)

  # Code occurrence by Plant Community
  ftpc_occ <- pacnvegetation::FilterPACNVeg(data_name = "Presence", park = filter_park, sample_frame = sample_frame, community = community, year = year, cycle = cycle, plot_type = plot_type, plot_number = plot_number, is_qa_plot = FALSE, nativity = nativity) %>%
    dplyr::left_join(Abbrev, by = join_by(Sampling_Frame)) %>%
    dplyr::mutate(SF = SF_Abbrev) %>%
    dplyr::select(SF, Sampling_Frame, Plot_Number, Cycle, Code) %>%
    # Only count a species once for a fixed plot
    dplyr::group_by(SF, Sampling_Frame, Code, Plot_Number) %>%
    dplyr::summarise(count = dplyr::n_distinct(Code), .groups = "drop") %>%
    # Get total across each plant community per park
    dplyr::group_by(SF, Code) %>%
    dplyr::summarise(plots = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = SF, values_from = plots, values_fill = 0)

  # Get master_list from Veg Spp DB using park (Unit_Code)

  pacnveg_master_spp_list <- read_spp_db(db_path)

  # handling all species records:
  if (park == "All") {
    master_list <- pacnveg_master_spp_list %>%
      dplyr::arrange(desc(Update_date_park)) %>%
      dplyr::distinct(Species_ID, .keep_all = TRUE)
    warning("dupicate species records removed, if species is present in more than one park, only the most recently edited record is selected. for example, common name in table may be specific to NPSA and missing Hawaii common name")
  } else {
    master_list <- pacnveg_master_spp_list %>%
      dplyr::filter(Park == park)
  }

  master_list <- master_list %>%
    dplyr::arrange(Taxonomic_Family, Genus, Species) %>%
    dplyr::left_join(ftpc_occ, by = "Code")

  # optionally return sampling frame occurrence matrix:
  if (presence_matrix == TRUE) {
    return(master_list)
  }

  # get new ftpc occurrence columns as a vector
  new_cols <- dplyr::setdiff(names(master_list), names(pacnveg_master_spp_list))
  new_cols

  master_list2 <- master_list %>%
    dplyr::mutate_at(.vars = new_cols, ~ replace_na(., 0)) %>%
    # Take first common name
    tidyr::separate(col = Park_common_name, into = "common1", sep = ",",
                    extra = "drop", remove = FALSE ) %>%
    dplyr::mutate(LF = tidyr::replace_na(Life_form_park, replace = "_NO LIFEFORM_")) %>%
    dplyr::mutate(presence_rank = rowSums(across(all_of(new_cols)))) #%>%
  #dplyr::mutate(pres_rank_text = stringr::str_pad(presence_rank, 3, pad = "0"))


  # Add column name to each value of each occurrence column

  # var <- "mpg"
  # Doesn't work: mtcars$var
  # These both work, but note that what they return is different
  # the first is a vector, the second is a data.frame
  # mtcars[[var]]   dataframe
  # mtcars[var]     vector

  for (col in new_cols) {
    master_list2[new_cols][col] <- paste0(col, ":", master_list2[new_cols][[col]])
  }

  # Unit the new ftpc occurrence columns into one column:
  master_list3 <- master_list2 %>%
    tidyr::unite("PC_presence", all_of(new_cols), sep = ", ")


  if (length(new_cols) > 1) {
    master_list4 <- master_list3 %>%
      dplyr::mutate(FTPC_pres = paste0(presence_rank, " (", PC_presence, ")"))
  } else {
    master_list4 <- master_list3 %>%
      dplyr::mutate(FTPC_pres = paste0(" (", PC_presence, ")"))
  }

  master_list5 <- master_list4 %>%
    dplyr::mutate(Field_ID = paste0(Scientific_name, " (", Code, ") ",
                                    Taxonomic_Family, " / ", Nativeness, " / ", LF,
                                    " / " , common1, " / ", FTPC_pres, " [syn: ", Synonym, "]")) %>%
    dplyr::arrange(desc(presence_rank), Field_ID) %>%
    dplyr::select(-Species_ID, -TSN, -common1, -Life_form, -Life_cycle, -Omit_in_NPSpecies, -Complete,
                  -TSN_park, -PC_presence, -LF)

  master_list5

  }


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
#' \dontrun{
#' FTPC_Olaa_Presence <- v_presence_table(sample_frame, table_type = "html")
#' }
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
    dplyr::summarise(N = dplyr::n())
  calc_n2 <- calc_n %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year) %>%
    dplyr::summarize(All = sum(N)) %>%
    tidyr::pivot_longer(cols = All, names_to = "Plot_Type", values_to = "N") %>%
    dplyr::bind_rows(calc_n) %>%
    dplyr::ungroup()

  Presence2_all<- Presence1 %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Scientific_Name, Code, Life_Form, Nativity) %>%
    dplyr::summarise(Plots = dplyr::n()) %>%
    dplyr::mutate(Plot_Type = "All") %>%
    dplyr::left_join(calc_n2, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle_Year", "Plot_Type")) %>%
    dplyr::mutate(Prop = Plots/N) %>%
    dplyr::ungroup()

  Presence2_rotational <- Presence1 %>%
    dplyr::filter(Plot_Type == "Rotational") %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Type, Scientific_Name, Code, Life_Form, Nativity) %>%
    dplyr::summarise(Plots = dplyr::n()) %>%
    dplyr::left_join(calc_n2, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle_Year", "Plot_Type")) %>%
    dplyr::mutate(Prop = Plots/N) %>%
    dplyr::ungroup()

  Presence2_fixed <- Presence1 %>%
    dplyr::filter(Plot_Type == "Fixed") %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle_Year, Plot_Type, Scientific_Name, Code, Life_Form, Nativity) %>%
    dplyr::summarise(Plots = dplyr::n()) %>%
    dplyr::left_join(calc_n2, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle_Year", "Plot_Type")) %>%
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
  goo <- function(x) {tidyr::replace_na(x, replace = 0)}

  Presence4 <- Presence3 %>%
    tidyr::separate(Cycle_Year, into = c("Cycle", "Year")) %>%
    dplyr::select(-Unit_Code, -Community, -Sampling_Frame, -Cycle) %>%
    tidyr::pivot_wider(names_from = Plot_Type,
                       values_from = c(Plots, N, Prop),
                       names_glue = "{Plot_Type}_{.value}") %>% #"values_fill = 0" does not work because then sometimes n will be zero
    dplyr::mutate(dplyr::across(dplyr::ends_with("_Prop"), goo))


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
    out_table <- Presence4
    }

  if (table_type == "html") { #if html option selected then prepare formattable
    # Table Create ----

    tbl <- Presence5
    col_names <- names(Presence5)

    all_years <- as.integer(stringr::str_extract(col_names, "\\d+"))
    all_years <- all_years[!is.na(all_years)]

    min_year <- min(all_years)

    max_year <- max(all_years)

    other_years <- as.character(all_years) %>%
      stringr::str_remove(as.character(min_year)) %>%
      stringr::str_remove(as.character(max_year)) %>%
      as.integer() %>%
      unique() %>%
      purrr::discard(is.na)

    other_years_min <- min(other_years)


    # "_All" Plots Columns ----

    # Get index for "All Plots" data
    all_plots_columns <- grep("_All", colnames(tbl))
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
                                                 color = "grey",
                                                 "padding-right" = "4px",
                                                 font.weight = "bold")))) %>%
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


