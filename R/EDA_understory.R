
#' Combine Strata low & high
#'
#' @param data An Understory tibble/dataframe with Species Point Data
#'
#' @return The Understory tibble/dataframe with one stratum "combined"
#' @export
#'
#' @examples
#' \dontrun{
#' data <- FilterPACNVeg("Understory")
#' understory_with_one_stratum <- CombineStrata(data)
#' }
UnderCombineStrata <- function(data) {
  if(!"Point" %in% colnames(data)){
    stop("Invalid data table. Point data is missing")
  }

  data_strat_comb <- data %>%
    #Stratum removed from grouping variable
    dplyr::select(-Stratum) %>%
    unique() %>%
    dplyr::mutate(Stratum = "High and Low")
  return(data_strat_comb)
}


#' Calculate Total Native & Nonnative Cover in Understory
#'
#' @inheritParams FilterPACNVeg
#' @param combine_strata If `TRUE`, don't split data into high and low strata. Otherwise, keep as is.
#' @param paired_change If `TRUE`, calculate change in percent cover across two cycles. If `cycle` is not specified, it will default to the last cycle.
#' @param paired_cycle Only required if `paired_change == TRUE`. The cycle number to compare to when calculating paired change.
#'
#' @return Summary table of total Percent Cover by Nativity
#' @export
#'
#' @examples
#' \dontrun{
#' data <- FilterPACNVeg("Understory")
#' Native_Cover_Summary_table <- UnderNativityCoverTotal(data)
#' }
UnderNativityCover <- function(combine_strata = FALSE, paired_change = FALSE, park, sample_frame, community, year, cycle, plot_type, paired_cycle = 1, silent = FALSE) {
  # Make sure paired cycle is length 1 if calculating paired change
  if (paired_change && length(paired_cycle) != 1) {
        stop("When paired_change == TRUE, paired_cycle must be a single cycle. Leave it empty to default to the first cycle.")
  }
  # Make sure cycle is length 1 if calculating paired change
  if (paired_change && !missing(cycle)) {
    if (length(cycle) != 1) {
      stop("When paired_change == TRUE, you must specify exactly one cycle or leave cycle blank to default to last cycle.")
    }
    cycle <- c(cycle, paired_cycle)
  }

  # Get raw data
  raw_data <- FilterPACNVeg("Understory", park, sample_frame, community, year, cycle, plot_type, is_qa_plot = FALSE, silent = silent)

  # If calculating paired change and no cycle specified, default to the most recent cycle
  if (paired_change && missing(cycle)) {
    raw_data %<>% filter(Cycle %in% c(paired_cycle, max(raw_data$Cycle)))
  }

  if (combine_strata == TRUE) {
    raw_data <- UnderCombineStrata(raw_data)
  }

  # Calculate Total Native & Nonnative Cover by stratum
  Nat_Cov <- raw_data %>%
    ## Dead Still present in Understory Data!!  - change default filter settings? Or remove entirely from database
    dplyr::filter(Dead==FALSE)  %>%
    ## Drop point records if point had no hits: (drop if 'Code == NA')
    tidyr::drop_na(Code)  %>%
    # Count Species at each cover point (for Native & Non-native within each Strata):
    dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Point, Stratum, Nativity, Year, Cycle) %>%
    dplyr::summarise(Hits_All_Nat = dplyr::n(), .groups = 'drop')  %>%
    # group hits by plot (remove Point from grouping variable)
    dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum, Nativity, Year, Cycle) %>%
    # Total hits at each point for each strata for entire plot
    #   (can be > 300 points or >100% because more than one native species can be present per point)
    dplyr::summarise(tot_pct_cov = (sum(Hits_All_Nat)) / 300 * 100, .groups = 'drop') %>%
    # Insert "0" for cover if category does not exsist (for example no hits for non-natives in High Stratum)
    tidyr::complete(tidyr::nesting(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle), Stratum, Nativity,
                    fill = list(tot_pct_cov = 0)) %>%
    # Arrange table so that difference in cover between cycles can be calculated easily (example - cycle 1 value for
    #   cover is followed by cycle 2 value for cover).
    dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum, Nativity) %>%
    dplyr::arrange(Cycle, .by_group = TRUE) %>%
    dplyr::ungroup()

  if (paired_change == TRUE) {
    paired_plots <- RemoveSingleVisits(raw_data)
    p <- paired_plots$Plot_Number %>% unique()
    Nat_Cov <- Nat_Cov %>%
      # remove plots that were only sampled once (this removes all rotationals and possibly some fixed plots if only sampled once)
      dplyr::filter(Plot_Number %in% p) %>%
      dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum, Nativity) %>%
      # Calculate the change in cover per cycle
      dplyr::mutate(chg_per_cycle = tot_pct_cov - dplyr::lag(tot_pct_cov, order_by = Cycle))

  }

  return(Nat_Cov)
}



#' Native V. Non-native Cover Plot for Understory data
#'
#' @inheritParams UnderNativityCover
#'
#' @return Nat-v-Non Plot comparing Native and Non-native Cover (or Change in Cover)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' Native_v_Nonnative_Plot <- UnderNativityCover.plot.nat_v_non(sample_frame = "Haleakala", sample_cycle = 2, paired_change = TRUE)
#' }

UnderNativityCover.plot.nat_v_non <- function(cover.stat, combine_strata = FALSE, paired_change = FALSE, park, sample_frame, community, year, cycle, plot_type, paired_cycle, silent = FALSE) {

  data <- UnderNativityCover(combine_strata = combine_strata, paired_change = paired_change,
                             park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle,
                             plot_type = plot_type, paired_cycle = paired_cycle, silent = silent)

  # Count records with Nativity == Unknown
  unks <- data %>%
    dplyr::filter(Nativity=="Unknown")
  unks <- sum(unks$tot_pct_cov, na.rm = T)
  # Remove unknowns
  data <- data %>%
    dplyr::filter(Nativity != "Unknown")
  # Statement of unknowns removed
  message(paste0(unks, "% of [Unknown] nativity cover removed."))

  # Remove alternate cover.stat so data can pivot
  if (paired_change == TRUE) {
    cover.stat.options <- c("tot_pct_cov", "chg_per_cycle")
    remove.cover.stat <- cover.stat.options[!cover.stat.options %in% cover.stat]

    toplot <- data %>%
      dplyr::filter(Cycle == cycle) %>%
      dplyr::select(-remove.cover.stat) %>%
      tidyr::pivot_wider(names_from = Nativity, values_from = cover.stat)
  }

  if (paired_change == FALSE) {
    toplot <- data %>%
      tidyr::pivot_wider(names_from = Nativity, values_from = cover.stat)
  }

  #Get max value for plotting data
  toplot.max <- toplot %>%
    dplyr::ungroup() %>%
    dplyr::select(Native, `Non-Native`)
  toplot.max <- max(c(abs(max(toplot.max, na.rm = TRUE)), abs(min(toplot.max, na.rm = TRUE))))

  if (cover.stat == "tot_pct_cov") {
    d <- expand.grid(x=0:(toplot.max + 5), y=0:(toplot.max + 5))

    plot.nat_v_non <- ggplot2::ggplot(d, ggplot2::aes(x,y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = atan(y/x), alpha=(0.15))) +
      ggplot2::scale_fill_gradient(high="red", low="green") +
      ggplot2::theme(legend.position="none") +
      ggplot2::geom_point(data = toplot,
                          mapping = ggplot2::aes(x = Native, y = `Non-Native`),
                          color = "black",
                          size = 2) +
      ggrepel::geom_text_repel(data = toplot,
                               mapping = ggplot2::aes(x = Native, y = `Non-Native`, label = Plot_Number),
                               min.segment.length = 0, seed = 42, box.padding = 0.5) +
      ggplot2::ylab("Total Non-Native Cover") +
      ggplot2::xlab("Total Native Cover") +
      ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
      ggplot2::coord_cartesian(xlim = c(0,toplot.max), ylim = c(0,toplot.max)) +
      ggplot2::facet_wrap(~Stratum + Sampling_Frame)

    return(plot.nat_v_non)
  }

  if (cover.stat == "chg_per_cycle") {
    ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3"))

    values <- data.frame(
      id = ids,
      value = c("1 Native (-)\nNon-native (-)\n",
                "2 Native (-)\nNon-native (+)\n",
                "3 Native (+)\nNon-native (++)\n",
                "4 Native (++)\nNon-native (+)\n",
                "5 Native (++)\nNon-native (-)\n"))

    #Create a custom color scale
    quad_c <- c("#cccccc","#d11141","#f37735","#C8E52A","#00b159")

    positions <- data.frame(
      id = rep(ids, each = 4),
      x = c(-100, 0, 0, -100,
            -100, -100, 0, 0,
            #-100, 0, 0, 0,
            0, 0, 0, 100,
            0, 0, 100, 100,
            0, 100, 100, 0),
      y = c(-100, -100, 0, 0,
            0, 100, 100, 0,
            #-100, 0, 0, -100,
            0, 0, 100, 100,
            0, 0, 0, 100,
            -100, -100, 0, 0))
    datapoly <- merge(values, positions, by = c("id"))

    plot.nat_v_non <- ggplot2::ggplot(data = datapoly,
                                      mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_polygon(ggplot2::aes(fill = value)) + # cannot do alpha w/coord_cartesian???
      ggplot2::scale_fill_manual(values = quad_c) +
      ggplot2::geom_point(data = toplot,
                          mapping = ggplot2::aes(x = Native, y = `Non-Native`),
                          color = "black",
                          size = 2) +
      ggrepel::geom_text_repel(data = toplot,
                               mapping = ggplot2::aes(x = Native, y = `Non-Native`, label = Plot_Number),
                               min.segment.length = 0, seed = 42, box.padding = 0.5) +
      ggplot2::ylab("Change in Non-Native Cover") +
      ggplot2::xlab("Change in Native Cover") +
      ggplot2::geom_vline(xintercept = 0) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
      ggplot2::coord_cartesian(xlim = c(-toplot.max, toplot.max), ylim = c(-toplot.max, toplot.max)) +
      ggplot2::facet_wrap(~Stratum + Sampling_Frame)

    return(plot.nat_v_non)
  }
}

#' Summarize Understory Cover Data by Nativity, Life_Form, or Species
#'
#' @inheritParams FilterPACNVeg
#' @param combine_strata If `TRUE`, combine high and low strata into one stratum.
#' @param paired_change If `TRUE`, calculate change in percent cover. Note that this drops "Rotational" plots and uses "Fixed" plots only.
#' @param plant_grouping "None" = No Groups, all vegetation (native & non-native) is lumped together,
#' "Nativity" = Cover values lumped by native and non-native vegetation.
#' "Life_Form" = Cover values grouped into native and non-native lifeforms (e.g. Trees, Shrubs, Ferns).
#' "Species" = Cover values for individual plant species.
#'
#' @return Summary table of total Percent Cover by grouping or change in total Percent Cover by grouping.
#' @export
#'
#' @examples
#' \dontrun{
#' native_and_non-native_cover <- summarize_understory(plant_grouping = "Nativity")
#' species_cover_change <- summarize_understory(plant_grouping = "Species", paired_change = TRUE)
#'
#' }

summarize_understory <- function(combine_strata = FALSE, plant_grouping, paired_change = FALSE, park, sample_frame, community, year, cycle, plot_type, plot_number, species_code, silent = FALSE) {
  if (missing(plant_grouping)) {
      stop("plant_grouping variable is missing")
  }


  # Get raw data
  understory <- FilterPACNVeg("Understory", park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle, plot_type = plot_type, plot_number = plot_number, species_code = species_code, is_qa_plot = FALSE, silent = silent)

  if (combine_strata == TRUE) {
    understory <- UnderCombineStrata(understory)
  }

  understory2 <- understory %>%
    dplyr::group_by(Sampling_Frame, Cycle) %>%
    dplyr::mutate(Year = min(Year)) %>%
    dplyr::mutate(Year = as.factor(Year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Plot_Number = as.factor(Plot_Number),
           Stratum = tidyr::replace_na(Stratum, "No_Veg"))

  base_vars <- c("Unit_Code", "Sampling_Frame", "Cycle", "Year",
                 "Plot_Type", "Plot_Number", "Point", "Stratum")

  if (plant_grouping == "None") {
    new_vars <- c()
  }

  if (plant_grouping == "Nativity") {
    new_vars <- c("Nativity")
  }

  if (plant_grouping == "Life_Form") {
    new_vars <- c("Nativity", "Life_Form")
  }

  if (plant_grouping == "Species") {
    new_vars <- c("Nativity", "Life_Form", "Code", "Scientific_Name")
  }

  all_vars <- c(base_vars, new_vars)
  all_vars_minus_point <- all_vars[all_vars != "Point"]

  var_nest1 <- c("Unit_Code", "Sampling_Frame", "Cycle", "Year", "Plot_Type", "Plot_Number")
  var_nest2 <- c("Stratum", new_vars)

  understory3 <- understory2 %>%
    dplyr::group_by(dplyr::across(all_vars)) %>%
    dplyr::summarise(Hits = dplyr::n(), .groups = 'drop') %>%
    # group hits by plot (remove Point from grouping variable)
    dplyr::group_by(dplyr::across(all_vars_minus_point)) %>%
    # Total hits at each point for each strata for entire plot
    #   (can be > 300 points or >100% because more than one 'Hit' can be present per point-strata)
    dplyr::summarise(Cover = (sum(Hits)) / 300 * 100, .groups = 'drop') %>%
    # Insert "0" for cover if category does not exist (for example no hits for non-natives in High Stratum)
    tidyr::complete(tidyr::nesting(!!!rlang::syms(var_nest1)),
                    tidyr::nesting(!!!rlang::syms(var_nest2)),
                    fill = list(Cover = 0)) # This should work now!


  if (paired_change == FALSE) {

    return(understory3)

  }

    understory4 <- understory3 %>%
      filter(Plot_Type == "Fixed")

    arrange_remove <- c("Cycle", "Year", "Point")
    arrange_vars <- all_vars[!all_vars %in% arrange_remove]

    max_cycle <- understory4 %>% dplyr::pull(Cycle) %>% max()
    max_cycle_lable <- rlang::as_label(max_cycle)
    max_cycle_lable <- paste0("Cycle", max_cycle_lable, "vs1")


    understory4 <- understory4 %>%
      # Arrange table so that difference in cover between cycles can be calculated easily (example - cycle 1 value for
      #   cover is followed by cycle 2 value for cover).
      dplyr::group_by(dplyr::across(arrange_vars)) %>%
      dplyr::arrange(Cycle, Year, .by_group = TRUE) %>%
      # Calculate the change in cover per cycle
      dplyr::mutate(Chg_Prior = Cover - dplyr::lag(Cover, order_by = Cycle)) %>%
      dplyr::mutate(Year = as.numeric(as.character(Year))) %>%
      dplyr::mutate(Years_Prior = Year - dplyr::lag(Year, order_by = Cycle)) %>%
      dplyr::mutate(Year = as.factor(Year)) %>%
      dplyr::mutate(Chg_Per_Year = Chg_Prior / Years_Prior) %>%
      dplyr::mutate(!!max_cycle_lable := Cover - dplyr::lag(Cover, order_by = Cycle,
                                                            n = max_cycle-1)) %>%
      dplyr::ungroup()

    return(understory4)



}


#' Add stats to vegetation data
#'
#' @param .data dataset with at least 1 numeric parameter
#' @param ... grouping variables
#'
#' @return datatable and numeric parameter with stats
#' @export
#'
#' @examples
#' \dontrun{
#' Cover <- summarize_understory(plant_grouping = "Nativity")
#' Cover_Stats <- add_stats(.data = Cover, Unit_Code, Sampling_Frame, Cycle, Stratum, Nativity)
#'
#' }
add_stats <- function(.data, ...){

  params <- .data %>%
    dplyr::select(where(is.numeric)) %>%
    names()

  #if (missing(...)) {
  #  ... <- .data %>%
  #    dplyr::select(-params) %>%
  #    names()
  #}

  print(params)

  stat_table <- tibble::tibble()

  for (param in params) {

    print(param)

    stat_table_param <- .data %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(NPLOTS = sum(!is.na(.data[[param]])),
                MEAN = round(mean(.data[[param]], na.rm = TRUE),3),
                MED = round(median(.data[[param]], na.rm = TRUE),3),
                MIN = round(min(.data[[param]], na.rm = TRUE),3),
                MAX = round(max(.data[[param]], na.rm = TRUE),3),
                SD = sd(.data[[param]], na.rm = TRUE),
                ERR = qt(0.975,df=NPLOTS-1)*(SD/sqrt(NPLOTS)),
                L = MEAN - ERR,
                R = MEAN + ERR) %>%
      dplyr::mutate(Parameter = param)



    stat_table <- dplyr::bind_rows(stat_table, stat_table_param)
  }

  return(stat_table)

}


#' Stats Bar Plot for Understory Cover by Nativity
#'
#' @inheritParams summarize_understory
#'
#' @return graph (gglot) of total Percent Cover by Nativity
#' @export
#'
#' @examples
#' \dontrun{
#' subalpine_mean_nativity_cover <- v_cover_plot_bar_nativity(community = "Subalpine Shrubland")
#'
#' }

v_cover_plot_bar_nativity <- function(combine_strata = FALSE,
                            paired_change = FALSE, param = "Cover", park, sample_frame, community,
                            year, cycle, plot_type, plot_number, filter_Code, silent = FALSE) {

  # Get raw data
  understory2 <- summarize_understory(combine_strata = combine_strata,
                                     plant_grouping = "Nativity",
                                     paired_change = paired_change,
                                     park = park, sample_frame = sample_frame,
                                     community = community, year = year,
                                     cycle = cycle, plot_type = plot_type,
                                     plot_number = plot_number, silent = silent)

    unknown_cover <- understory2 %>%
      dplyr::filter(Nativity == "Unknown" & Cover > 0)

    unk_cover_tot <- unknown_cover %>%
      dplyr::pull(Cover) %>%
      sum()

    understory3 <- understory2 %>%
      dplyr::filter(Stratum != "No_Veg",
                    Nativity != "Unknown")
    print(paste0(round(unk_cover_tot,2), "% cover of species with unknown Nativity removed"))


  # Nativity discrete scale Colors:
  nativity_colors <- c("Native" = "#1b9e77",
                       "No_Veg" = "grey",
                       "Non-Native" = "#d95f02",
                       "Unknown" = "#7570b3")

  # add stats
  understory_stats <- add_stats(understory3, Unit_Code, Sampling_Frame,
                                Cycle, Year, Stratum, Nativity)

  # sample size calculation for text (output is on graph caption)
  sample_size <- understory_stats %>%
    dplyr::filter(NPLOTS != 0) %>%
    dplyr::filter(Parameter == param) %>%
    dplyr::select(Sampling_Frame, Year, NPLOTS) %>%
    dplyr::distinct() %>%
    dplyr::group_by(Sampling_Frame) %>%
    dplyr::mutate(count_cycles = match(Year, unique(Year))) %>%
    dplyr::mutate(SF = paste0("; ", Sampling_Frame, ": ")) %>%
    dplyr::mutate(SF = dplyr::case_when(count_cycles == 1 ~ SF,
                                        TRUE ~ "")) %>%
    dplyr::mutate(Text = paste0(SF, Year, " [n = ", NPLOTS, "]")) %>%
    dplyr::pull(Text) %>%
    paste(collapse = ", ") %>%
    stringr::str_sub(3) %>%
    stringr::str_replace_all(", ;", ";")
  sample_size

  #........BAR YEARLY MEANS
  label_param <- stringr::str_replace_all(param, "_", " ")

  plot <- understory_stats %>%
    dplyr::mutate(SF_no_space = stringr::str_replace_all(Sampling_Frame, " ", "_")) %>%
    dplyr::filter(NPLOTS != 0) %>%
    dplyr::filter(Parameter == param) %>%
    ggplot2::ggplot(ggplot2::aes(x = Year, y = MEAN, fill = Nativity)) +
    ggplot2::geom_col(position = position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=L, ymax=R), width=.2,
                           position=position_dodge(.9)) +
    geom_hline(yintercept = 0) +
    labs(y = paste(label_param, "(Total % Cover)")) +
    #ggh4x package allows nested facets:
    #ggh4x::facet_nested(Stratum ~ Sampling_Frame + Nativity, scales = "free_x") +
    ggplot2::facet_grid(Stratum ~ SF_no_space + Nativity,
                        labeller = label_parsed,
                        scales = "free_x") +
    ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
    ggplot2::xlab("Year") +
    ggplot2::theme(legend.position="none") +
    ggplot2::labs(caption = sample_size) +
    theme(axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5))


  return(plot)

}
