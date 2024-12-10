
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
#' @param rm_unk_natv Remove
#'
#' @return Summary table of total Percent Cover by Nativity
#' @export
#'
#' @examples
#' \dontrun{
#' data <- FilterPACNVeg("Understory")
#' Native_Cover_Summary_table <- UnderNativityCoverTotal(data)
#' }
UnderNativityCover <- function(combine_strata = FALSE, paired_change = FALSE, rm_unk_natv = TRUE, crosstalk = FALSE, crosstalk_group = "cover", park, sample_frame, community, year, cycle, plot_type, paired_cycle = 1, silent = FALSE) {
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
  raw_data <- FilterPACNVeg(data_name = "Understory", park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle, plot_type = plot_type, is_qa_plot = FALSE, silent = silent)

  # If calculating paired change and no cycle specified, default to the most recent cycle
  if (paired_change && missing(cycle)) {
    cycle <- max(raw_data$Cycle)
    raw_data %<>% filter(Cycle %in% c(paired_cycle, cycle))
  }

  if (combine_strata == TRUE) {
    raw_data <- UnderCombineStrata(raw_data)
  }

  # Calculate Total Native & Nonnative Cover by stratum
  Nat_Cov <- raw_data %>%
    # Change so all sampling cyles have same year (ie first year of new cycle)
    dplyr::group_by(Sampling_Frame, Cycle) %>%
    dplyr::mutate(Year = min(Year)) %>%
    dplyr::ungroup() %>%
    ## Dead Still present in Understory Data!!  - change default filter settings? Or remove entirely from database
    #dplyr::filter(Dead==FALSE)  %>%
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
    # Round total pct cover to a reasonable number of digits
    dplyr::mutate(tot_pct_cov = round(tot_pct_cov, 1)) %>%
    # Insert "0" for cover if category does not exsist (for example no hits for non-natives in High Stratum)
    tidyr::complete(tidyr::nesting(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle), Stratum, Nativity,
                    fill = list(tot_pct_cov = 0)) %>%
    # Arrange table so that difference in cover between cycles can be calculated easily (example - cycle 1 value for
    #   cover is followed by cycle 2 value for cover).
    dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum, Nativity) %>%
    dplyr::arrange(Cycle, .by_group = TRUE) %>%
    dplyr::ungroup()

  if (rm_unk_natv == TRUE) {
    # Count records with Nativity == Unknown
    unks <- Nat_Cov %>%
      dplyr::filter(Nativity=="Unknown")
    unks <- sum(unks$tot_pct_cov, na.rm = T)
    # Remove unknowns
    Nat_Cov <- Nat_Cov %>%
      dplyr::filter(Nativity != "Unknown")
    # Statement of unknowns removed
    message(paste0(unks, "% of [Unknown] nativity cover removed."))
  }

  # Remove alternate cover.stat so data can pivot
  if (paired_change == TRUE) {
    # cover.stat.options <- c("tot_pct_cov", "chg_per_cycle")
    # remove.cover.stat <- cover.stat.options[!cover.stat.options %in% cover.stat]
    paired_plots <- RemoveSingleVisits(raw_data)
    p <- paired_plots$Plot_Number %>% unique()
    Nat_Cov <- test <- Nat_Cov %>%
      # remove plots that were only sampled once (this removes all rotationals and possibly some fixed plots if only sampled once)
      dplyr::filter(Plot_Number %in% p) %>%
      dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum, Nativity) %>%
      # Calculate the change in cover per cycle
      dplyr::mutate(chg_per_cycle = tot_pct_cov - dplyr::lag(tot_pct_cov, order_by = Cycle)) %>%
      dplyr::filter(Cycle == max(cycle)) %>%
      dplyr::select(-tot_pct_cov) %>%
      dplyr::mutate(Nativity = paste0(gsub("-", "", Nativity), "_Cover_Change_pct")) %>%
      tidyr::pivot_wider(names_from = Nativity, values_from = chg_per_cycle)
  }

  if (paired_change == FALSE) {
    Nat_Cov <- Nat_Cov %>%
      dplyr::mutate(Nativity = paste0(gsub("-", "", Nativity), "_Cover_Total_pct")) %>%
      tidyr::pivot_wider(names_from = Nativity, values_from = tot_pct_cov)
  }

  Nat_Cov <- dplyr::ungroup(Nat_Cov)

  if (crosstalk) {
    Nat_Cov <- dplyr::mutate(Nat_Cov, key = paste0(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle))
    Nat_Cov <- crosstalk::SharedData$new(Nat_Cov, group = crosstalk_group, key = ~key)
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

UnderNativityCover.plot.nat_v_non <- function(combine_strata = FALSE, paired_change = FALSE, crosstalk = FALSE, crosstalk_group = "cover", interactive = FALSE, year_filter = TRUE, park, sample_frame, community, year, cycle, plot_type, paired_cycle, silent = FALSE, data_table) {
  if (crosstalk && !interactive) {
    stop("Crosstalk only works with interactive plots. Change `crosstalk` to FALSE or change `interactive` to TRUE.")
  }

  if (!missing(data_table)) {
    data <- data_table  # Allow a custom data table to be passed in
  } else {
    data <- UnderNativityCover(combine_strata = combine_strata, paired_change = paired_change, crosstalk = FALSE,
                               park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle,
                               plot_type = plot_type, paired_cycle = paired_cycle, silent = silent) %>%
        dplyr::group_by(Sampling_Frame, Cycle) %>%
        dplyr::mutate(Year = min(Year))
  }

  # If data not paired-change, then add columns to help plot total cover
  if (!paired_change) {
      data <- data %>%
      dplyr::mutate(nat_ratio = Native_Cover_Total_pct / (NonNative_Cover_Total_pct + Native_Cover_Total_pct) * 100) %>%
      dplyr::mutate(tot_cover = Native_Cover_Total_pct + NonNative_Cover_Total_pct) %>%
      dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number) %>%
      dplyr::mutate(last_cycle = max(Cycle)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(last_visit = dplyr::case_when(Cycle == last_cycle ~ TRUE,
                                                    TRUE ~ FALSE))
  } else {data <- data}

  # If data is a crosstalk object, extract just the data so we can work with it
  if (interactive && crosstalk) {
    data <- dplyr::mutate(data, key = paste0(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle)) #crosstalk recommends using unique key
    data <- crosstalk::SharedData$new(data, group = crosstalk_group, key = ~key)
    data_table <- data$data()
  } else {data_table <- data}

  # Get max value for plotting data
  toplot.max <- data_table %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::starts_with(c("Native", "NonNative")))
  toplot.max <- max(c(abs(max(toplot.max, na.rm = TRUE)), abs(min(toplot.max, na.rm = TRUE))))

  # Total cover plots
  if (!paired_change) {
    if (interactive) {
      plot.nat_v_non <- totalCover_plotly(data, toplot.max, year_filter = year_filter)  # Plot total cover in plotly
    } else {
      plot.nat_v_non <- totalCover_ggplot(data, toplot.max)  # Plot total cover in ggplot
    }
  }

  # Paired change plots
  if (paired_change) {
    if (interactive) {
      plot.nat_v_non <- changeInCover_plotly(data, toplot.max)  # Plot paired change in plotly
    } else {
      plot.nat_v_non <- changeInCover_ggplot(data, toplot.max)  # Plot paired change in ggplot
    }
  }

  return(plot.nat_v_non)
}

#' Helper function for plotting total cover in ggplot
#'
#' @param data Data to plot
#' @param max_lim maximum value in data
#'
#' @return ggplot object
totalCover_ggplot <- function(data, max_lim) {
  d <- expand.grid(x=0:(max_lim + 5), y=0:(max_lim + 5))

  plot.nat_v_non <- ggplot2::ggplot(d, ggplot2::aes(x,y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = atan(y/x), alpha=(0.15))) +
    ggplot2::scale_fill_gradient(high="red", low="green") +
    ggplot2::theme(legend.position="none") +
    ggplot2::geom_point(data = data,
                        mapping = ggplot2::aes(x = Native_Cover_Total_pct, y = NonNative_Cover_Total_pct),
                        color = "black",
                        size = 2) +
    ggrepel::geom_text_repel(data = data,
                             mapping = ggplot2::aes(x = Native_Cover_Total_pct, y = NonNative_Cover_Total_pct, label = Plot_Number),
                             min.segment.length = 0, seed = 42, box.padding = 0.5) +
    ggplot2::ylab("Total Non-Native Cover") +
    ggplot2::xlab("Total Native Cover") +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
    ggplot2::coord_cartesian(xlim = c(0,max_lim), ylim = c(0,max_lim)) +
    ggplot2::facet_wrap(~Stratum + Sampling_Frame)
}

#' Helper function for plotting total cover in plotly
#'
#' @param data Data to plot
#' @param max_lim maximum value in data
#'
#' @return html widget
totalCover_plotly <- function(data, max_lim, year_filter = TRUE) {

  nat_ratio_cols <- data$data() %>%
    dplyr::pull(nat_ratio)

  # pal <- grDevices::colorRampPalette(c("red", "orange", "orange", "yellow", "yellow", "green"))(length(unique(nat_ratio_cols)))
  #
  # plt <- plotly::plot_ly(data = data,
  #         x = ~ Native_Cover_Total_pct,
  #         y = ~ NonNative_Cover_Total_pct,
  #         color = ~ nat_ratio,
  #         colors = pal,
  #         size = ~ tot_cover,
  #         type = "scatter",
  #         mode = "markers",
  #         marker = list(line = list(color = "black"),
  #                       width = 2))

  pal <- grDevices::colorRampPalette(c("red", "orange", "yellow", "green"))(length(unique(nat_ratio_cols)))

  plt <- data %>%
    plotly::plot_ly(colors = pal) %>%
    #plotly::highlight_key(~Plot_Number) %>%
    plotly::add_segments(x = 0, xend = max_lim, y = 0, yend = max_lim,
                         showlegend = TRUE,
                         name = "1:1",
                         line = list(color = "gray")) %>%
    plotly::add_markers(data = data,
                        x = ~ Native_Cover_Total_pct,
                        y = ~ NonNative_Cover_Total_pct,
                        hoverinfo = "text",
                        #groups = ~Plot_Number,
                        color = ~ nat_ratio,
                        type = "scatter",
                        marker = list(line = list(color = "black"), width = 2, size = ~ tot_cover*.1),
                        text = ~paste('</br> Plot: ', Plot_Number,
                                      '</br> Year: ', Year,
                                      '</br> Native cover: ', round(Native_Cover_Total_pct, 1),
                                      '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1)),
                        showlegend = TRUE,
                        name = "Plot") %>%
    plotly::highlight(on = "plotly_hover", off = "plotly_doubleclick") %>%
    plotly::layout(xaxis = list(title = "Native cover"), #, range = lims
                   yaxis = list(title = "Non-native cover")) %>% #, range = lims
    plotly::colorbar(title = "% Native", limits = c(0,100))

  # If 'year_filter == TRUE' then add filter checkbox:
  if (year_filter) {
    box_filter <- crosstalk::filter_checkbox("html_year_id", "Year", data, ~Year, inline = TRUE)

    plt <- crosstalk::bscols(
      widths = c(12, 12),
      plt, box_filter)
  }

  return(plt)
}

#' Helper function for plotting cover change in ggplot
#'
#' @param data Data to plot
#' @param max_lim maximum value in data
#'
#' @return ggplot object
changeInCover_ggplot <- function(data, max_lim) {
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
    ggplot2::geom_point(data = data,
                        mapping = ggplot2::aes(x = Native_Cover_Change_pct, y = NonNative_Cover_Change_pct),
                        color = "black",
                        size = 2) +
    ggrepel::geom_text_repel(data = data,
                             mapping = ggplot2::aes(x = Native_Cover_Change_pct, y = NonNative_Cover_Change_pct, label = Plot_Number),
                             min.segment.length = 0, seed = 42, box.padding = 0.5) +
    ggplot2::ylab("Change in Non-Native Cover") +
    ggplot2::xlab("Change in Native Cover") +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
    ggplot2::coord_cartesian(xlim = c(-max_lim, max_lim), ylim = c(-max_lim, max_lim)) +
    ggplot2::facet_wrap(~Stratum + Sampling_Frame)
}

#' Helper function for plotting change in cover in plotly
#'
#' @param data Data to plot
#' @param max_lim maximum value in data
#'
#' @return html widget
changeInCover_plotly <- function(data, max_lim) {
  lims <- c(-(1.1 * max_lim), 1.1 * max_lim)  # Expand the x and y lims a little so that points don't end up on the very edge of the plot

  ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3"))

  values <- data.frame(
    id = ids,
    value = c("1 Native (-)\nNon-native (-)\n",
              "2 Native (-)\nNon-native (+)\n",
              "3 Native (+)\nNon-native (++)\n",
              "4 Native (++)\nNon-native (+)\n",
              "5 Native (++)\nNon-native (-)\n"))

  #Create a custom color scale
  quad_c <- data.frame(id = ids,
                       color = c("#cccccc", "#C8E52A", "#d11141", "#00b159", "#f37735"))

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
  datapoly <- merge(datapoly, quad_c, by = c("id")) %>%
    dplyr::group_by(value)
  plt <- plotly::plot_ly() %>%
    plotly::add_polygons(data = datapoly,
                         x = ~x,
                         y = ~y,
                         hoverinfo='skip',
                         color = ~value,
                         colors = ~color,
                         inherit = FALSE,
                         type = "contour") %>%
    plotly::add_trace(data = data,
                      x = ~ Native_Cover_Change_pct,
                      y = ~ NonNative_Cover_Change_pct,
                      hoverinfo = "text",
                      type = "scatter",
                      mode = "markers",
                      showlegend = FALSE,
                      marker = list(color = "#212121"),
                      text = ~paste('</br> Plot: ', Plot_Number,
                                    '</br> Non-native change: ', round(NonNative_Cover_Change_pct, 1),
                                    '</br> Native change: ', round(Native_Cover_Change_pct, 1))) %>%
    # plotly::add_text(data = data,
    #                  x = ~ Native_Cover_Change_pct,
    #                  y = ~ NonNative_Cover_Change_pct,text = ~Plot_Number, textposition = "top right") %>%
    plotly::highlight(on = "plotly_hover") %>%
    plotly::layout(xaxis = list(title = "Native cover change", range = lims),
                   yaxis = list(title = "Non-native cover change", range = lims),
                   showlegend = FALSE)
  return(plt)
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
    #dplyr::group_by(across(all_vars)) %>%
    dplyr::group_by(across(all_of(all_vars))) %>%
    dplyr::summarise(Hits = dplyr::n(), .groups = 'drop') %>%
    # group hits by plot (remove Point from grouping variable)
    dplyr::group_by(across(all_of(all_vars_minus_point))) %>%
    # Total hits at each point for each strata for entire plot
    #   (can be > 300 points or >100% because more than one 'Hit' can be present per point-strata)
    dplyr::summarise(Cover = (sum(Hits)) / 300 * 100, .groups = 'drop') %>%
    # Insert "0" for cover if category does not exist (for example no hits for non-natives in High Stratum)
    tidyr::complete(tidyr::nesting(!!!syms(var_nest1)),
                    tidyr::nesting(!!!syms(var_nest2)),
                    fill = list(Cover = 0)) # This should work now!


  if (paired_change == FALSE) {

    return(understory3)

  } else {

    understory4 <- understory3 %>%
      dplyr::filter(Plot_Type == "Fixed")

    arrange_remove <- c("Cycle", "Year", "Point")
    arrange_vars <- all_vars[!all_vars %in% arrange_remove]

    max_cycle <- understory4 %>% dplyr::pull(Cycle) %>% max()
    max_cycle_lable <- rlang::as_label(max_cycle)
    max_cycle_lable <- paste0("Cycle", max_cycle_lable, "vs1")


    understory4 <- understory4 %>%
      # Arrange table so that difference in cover between cycles can be calculated easily (example - cycle 1 value for
      #   cover is followed by cycle 2 value for cover).
      dplyr::group_by(across(all_of(arrange_vars))) %>%
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

  message(params)

  stat_table <- tibble::tibble()

  for (param in params) {

    #print(param)

    stat_table_param <- .data %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(NPLOTS = sum(!is.na(.data[[param]])),
                MEAN = round(mean(.data[[param]], na.rm = TRUE),3),
                MED = round(median(.data[[param]], na.rm = TRUE),3),
                MIN = suppressWarnings(round(min(.data[[param]], na.rm = TRUE),3)),
                MAX = suppressWarnings(round(max(.data[[param]], na.rm = TRUE),3)),
                SD = sd(.data[[param]], na.rm = TRUE),
                ERR = suppressWarnings(qt(0.975,df=NPLOTS-1)*(SD/sqrt(NPLOTS))),
                L = MEAN - ERR,
                R = MEAN + ERR) %>%
      dplyr::mutate(Parameter = param)



    stat_table <- dplyr::bind_rows(stat_table, stat_table_param)
  }

  return(stat_table)

}

#' Stats Bar Plot for Understory Cover by Species, Life form, Nativity, or no grouping (all cover lumped)
#'
#' @inheritParams summarize_understory
#'
#' @param plant_grouping "None" = No Groups, all vegetation (native & non-native) is lumped together,
#' "Nativity" = Cover values lumped by native and non-native vegetation.
#' "Life_Form" = Cover values grouped into native and non-native lifeforms (e.g. Trees, Shrubs, Ferns).
#' "Species" = Cover values for individual plant species.
#'
#' @param measurement "Cover" = Total % Cover for plant_grouping.
#' "Chg_Prior = Difference between Cover for a selected cycle and the previous cycle.
#' "Years_Prior" = Number of years between sampling cycles.
#' "Chg_Per_Year" = Chg_Prior divided by Years_Prior.
#'
#' @param remove_unknown "default == TRUE. Removes any Life_form with value == "Unknown".
#' Also removes the non-vegetated points from final display.
#'
#' @return graph (gglot) of total Percent Cover by Nativity
#' @export
#'
#' @examples
#' \dontrun{
#' subalpine_mean_nativity_cover <- v_cover_bar_stats(community = "Subalpine Shrubland")
#'
#' }

v_cover_bar_stats <- function(combine_strata = FALSE, plant_grouping = "Nativity", species_filter,
                              paired_change = FALSE, measurement = "Cover", park, sample_frame, community,
                              year, cycle, plot_type, plot_number, filter_Code, silent = FALSE, return_n = FALSE, remove_unknown = TRUE) {


  # Set plant_grouping_vars used for calculating stats based on plant_grouping argument
  if (plant_grouping == "Species") {
    summary_param <- "Scientific_Name"
  } else if (plant_grouping == "None") {
    summary_param <- "Sampling_Frame"
  } else {
    summary_param <- plant_grouping

  }


  if (plant_grouping == "None") {
    plant_grouping_vars <- c()
  }

  if (plant_grouping == "Nativity") {
    plant_grouping_vars <- c("Nativity")
  }

  if (plant_grouping == "Life_Form") {
    plant_grouping_vars <- c("Nativity", "Life_Form")
  }

  if (plant_grouping == "Species") {
    plant_grouping_vars <- c("Nativity", "Life_Form", "Code", "Scientific_Name")
  }

  # Get raw data
  understory2 <- summarize_understory(combine_strata = combine_strata,
                                      plant_grouping = plant_grouping,
                                      paired_change = paired_change,
                                      park = park, sample_frame = sample_frame,
                                      community = community, year = year,
                                      cycle = cycle, plot_type = plot_type,
                                      plot_number = plot_number, silent = silent)

  if (!missing("species_filter") && plant_grouping == "Species") {
    understory2 <- understory2 %>%
      dplyr::filter(Scientific_Name %in% species_filter)
  } else if (!missing("species_filter") && plant_grouping != "Species") {
    stop("Invalid plant_grouping selection. If species_filter is used, plant_grouping must = 'Species'")
  } # may want to change this in the future - can filter species as part of summarize_understory,
  # then summarize by Life_form, Nativity, etc. This could be useful.



  if (remove_unknown == TRUE) {

    unknown_cover <- understory2 %>%
      filter_all(any_vars(. == "Unknown"))

    unk_cover_tot <- unknown_cover %>%
      dplyr::pull("Cover") %>%
      sum()

    understory3 <- understory2 %>%
      dplyr::filter(Stratum != "No_Veg",
                    Nativity != "Unknown")
    message(paste0(round(unk_cover_tot,2), "% cover of species with unknown ", plant_grouping, " removed"))
  } else {
    understory3 <- understory2
  }


  # Nativity discrete scale Colors:
  nativity_colors <- c("Native" = "#1b9e77",
                       "No_Veg" = "grey",
                       "Non-Native" = "#d95f02",
                       "Unknown" = "#7570b3")


  # add stats
  base_vars <- c("Unit_Code", "Sampling_Frame", "Cycle", "Year", "Stratum")

  add_stats_vars <- c(base_vars, plant_grouping_vars)

  understory_stats <- add_stats(understory3,
                                across(all_of(add_stats_vars)))

  # sample size calculation for text (output is on graph caption)
  sample_size <- understory_stats %>%
    dplyr::filter(NPLOTS != 0) %>%
    dplyr::filter(Parameter == measurement) %>%
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

  if (return_n == TRUE){
    return(sample_size)
  }

  #........BAR YEARLY MEANS


  label_param <- stringr::str_replace_all(measurement, "_", " ")

  # If only one plot than make SD, ERR, L, and R columns 0 (instead of NA)
  understory_stats <- understory_stats %>%
    dplyr::mutate(SD = dplyr::case_when(NPLOTS == 1 ~ 0, .default = as.numeric(SD)),
           ERR = dplyr::case_when(NPLOTS == 1 ~ 0, .default = as.numeric(ERR)),
           L = dplyr::case_when(NPLOTS == 1 ~ 0, .default = as.numeric(L)),
           R = dplyr::case_when(NPLOTS == 1 ~ 0, .default = as.numeric(R)))

  understory_stats <- understory_stats[complete.cases(understory_stats),]

  understory_stats <- understory_stats %>%
    dplyr::filter(Parameter == measurement)

  understory_stats[[summary_param]] <- as.factor(understory_stats[[summary_param]])

  understory_stats[[summary_param]] <- reorder(understory_stats[[summary_param]],
                                               understory_stats$MEAN, .fun = max,
                                               decreasing = TRUE)
  if (plant_grouping == "None") {
    understory_stats$Nativity <- "Unknown"
  }

  plot <- understory_stats %>%
    #forcats::fct_reorder(Scientific_Name, MEAN, .fun = max) %>%
    dplyr::mutate(SF_no_space = stringr::str_replace_all(Sampling_Frame, " ", "_")) %>%
    dplyr::filter(NPLOTS != 0) %>%
    dplyr::filter(Parameter == measurement) %>%
    ggplot2::ggplot(ggplot2::aes(x = Year, y = MEAN, fill = Nativity)) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=L, ymax=R), width=.2,
                           position=ggplot2::position_dodge(.9)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(y = paste(label_param, "(Total % Cover)")) +
    #ggh4x package allows nested facets:
    ggplot2::facet_grid(Stratum ~ .data[[summary_param]] + SF_no_space,
                        #labeller = ggplot2::label_parsed,
                        scales = "free_x") +
    ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
    ggplot2::xlab("Year") +
    ggplot2::theme(legend.position="none") +
    ggplot2::labs(caption = sample_size) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5))

  num_facets <- understory_stats %>%
    dplyr::select(Sampling_Frame, .data[[summary_param]]) %>%
    dplyr::distinct()

  if (nrow(num_facets) > 6) {
    plot <- plot +
      ggplot2::coord_flip() +
      ggplot2::facet_grid(.data[[summary_param]] + SF_no_space ~ ., switch = "y") +
      ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 0))
  }

  # This should be changed to if (distinct(Sampling_Frame) > 1)
  if (length(unique(understory_stats$Sampling_Frame)) > 2) {
    plot <- plot +
      ggplot2::coord_flip() +
      ggplot2::facet_grid(SF_no_space ~ ., switch = "y", scales = "free_y") +
      ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 0))
  }


  return(plot)

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
    message(paste0(round(unk_cover_tot,2), "% cover of species with unknown Nativity removed"))


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
    dplyr::mutate(SF_no_space = stringr::str_replace_all(Sampling_Frame, "\\p{WHITE_SPACE}", "_")) %>%
    dplyr::filter(NPLOTS != 0) %>%
    dplyr::filter(Parameter == param) %>%
    ggplot2::ggplot(ggplot2::aes(x = Year, y = MEAN, fill = Nativity)) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=L, ymax=R), width=.2,
                           position=ggplot2::position_dodge(.9)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(y = paste(label_param, "(Total % Cover)")) +
    #ggh4x package allows nested facets:
    ggplot2::facet_grid(Stratum ~ SF_no_space + Nativity,
                        labeller = ggplot2::label_parsed,
                        scales = "free_x") +
    ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
    ggplot2::xlab("Year") +
    ggplot2::theme(legend.position="none") +
    ggplot2::labs(caption = sample_size) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5))


  return(plot)

}


#' Bar plot of species % cover for each sampling plot
#'
#' Can be placed side-by-side to compare two plots
#'
#' @param sample_frame (required)
#' @param crosstalk_filters Include dropdowns to filter on species, nativity, and mgmt unit?
#' @param crosstalk_group Help distinguish crosstalk iterations
#' @return An HTML object (if `crosstalk_filters == TRUE`) or a plotly object.
#' @export
#'
v_cover_bar_spp_plot <- function(sample_frame, crosstalk_filters = TRUE, crosstalk_group) {

  if (missing(sample_frame)) {
    stop("`sample_frame` is required")
  }

  sum_und1 <- summarize_understory(combine_strata = TRUE,
                                   plant_grouping = "Species",
                                   paired_change = FALSE,
                                   sample_frame = sample_frame)

  sum_und2 <-sum_und1 %>%
    dplyr::mutate(Cycle = as.factor(Cycle),
                  all = "Select all",
                  hovertext = paste(Scientific_Name, Nativity, paste0(round(Cover, digits = 1), "%"), sep = "\n")) %>%
    # Filter zero cover
    dplyr::filter(Cover > 0) %>%
    # Filter No Veg Cover
    dplyr::filter(!is.na(Scientific_Name))

  message_nas <- sum_und2 %>%
    dplyr::filter_all(dplyr::any_vars(is.na(.)))

  if (nrow(message_nas) > 0) {
    warning(paste(nrow(message_nas), "rows with 'NA' in dataset", sep = " "))
    message(paste0(capture.output(message_nas), collapse = "\n"))
  }

  sp_w_cover <- sum_und2 %>%
    crosstalk::SharedData$new(group = crosstalk_group)

  cover_bar <- sp_w_cover %>%
    plotly::plot_ly(type = "bar", x = ~Life_Form, y = ~Cover, color = ~Nativity, strokes = "white", stroke = ~Scientific_Name,
                    text = ~hovertext,
                    hoverinfo = 'text',
                    textposition = "none") %>%
    plotly::layout(xaxis = list(title = "Life Form"),
                   yaxis = list(title = "% cover"),
                   legend = list(itemclick = FALSE,
                                 itemdoubleclick = FALSE),
                   clickmode = "none")

  cover_bar

  if (crosstalk_filters) {
    filters <- crosstalk::bscols(
      crosstalk::filter_select(id = "select-year", label = "Filter on year", sharedData = sp_w_cover, group = ~Year, allLevels = FALSE, multiple = FALSE),
      crosstalk::filter_select("select-plot", "Filter on plot #", sp_w_cover, ~Plot_Number, allLevels = FALSE, multiple = FALSE),
      widths = c(4,4))

    plot <- suppressWarnings(crosstalk::bscols(filters,
                                               cover_bar,
                                               widths = c(12, 12)))
  } else {
    plot <- cover_bar
  }

  return(plot)

}

#' Sunburst plot of understory percent cover
#'
#' @inheritParams FilterPACNVeg
#' @param mgmt_unit Include management unit?
#' @param colors Character vector of color names or hex values. This can be a named vector with names corresponding to the values in the center of the plot (the first column specified in `group_by`), or you can provide an unnamed vector of any number of colors to create a gradient.
#'
#' @return A plotly object
#' @export
#'
understorySunburst <- function(sample_frame, cycle, mgmt_unit = TRUE, colors = "default") {
  if (missing(sample_frame)) {
    stop("`sample_frame` is required")
  }

  if (missing(cycle)) {
    stop("`cycle` is required")
  }

  if (length(colors) == 1 && colors == "default") {
    nativity_colors <- c("Native" = "#1b9e77", "No Veg" = "grey", "Non-Native" = "#d95f02", "Unknown" = "#7570b3")
    mgmt_unit_colors <- c("#F8573A", "#F4C47B", "#28468B", "#AED5CB")
    if (mgmt_unit) {
      colors <- mgmt_unit_colors
    } else {
      colors <- nativity_colors
    }
  }

  plot_levels <- c("Nativity", "Life_Form", "Code")
  if (mgmt_unit) {
    plot_levels <- c("GROUP_COL", plot_levels)
  }


  # und <- FilterPACNVeg("Understory", sample_frame = sample_frame, cycle = cycle)  # Only get data from most recent cycle
  #
  # # PLACEHOLDER
  # # TODO: replace this with actual grouping column
  # und <- dplyr::mutate(und, GROUP_COL = sample(LETTERS[c(1, 2, 2, 3, 5, 5, 5)], size = dplyr::n(), replace = TRUE))
  #
  # und <- und %>%
  #   dplyr::mutate(Life_Form=replace(Life_Form, Code=="SOPCHR", "Shrub"))
  #
  # # prep data for sunburst plot
  # und <- UnderCombineStrata(und) %>%
  #   dplyr::mutate(across(where(is.character), replace_na, "No Veg")) %>%
  #   dplyr::group_by(across(tidyselect::all_of(c("Cycle", "Unit_Code", "Sampling_Frame", "Plot_Number", group_by)))) %>%
  #   dplyr::summarize(Hits_Sp = dplyr::n(), .groups = "drop") %>%
  #   tidyr::complete(tidyr::nesting(!!!syms(c("Cycle", "Unit_Code", "Sampling_Frame", "Plot_Number", group_by))),
  #            fill = list(Hits_Sp = 0)) %>%
  #   dplyr::mutate(Plot_Percent = Hits_Sp/300) %>%
  #   dplyr::group_by(across(tidyselect::all_of(c("Cycle", "Unit_Code", "Sampling_Frame", group_by)))) %>%
  #   dplyr::summarize(n = dplyr::n(),
  #             plots_present = sum(Hits_Sp > 0),
  #             Avg_Cover = round(mean(Plot_Percent), 3),
  #             Std_Dev = round(sd(Plot_Percent), 3),
  #             .groups = "drop")

  # Set colors - use first col in summarize_by
  # If names of colors vector match values in the first grouping level, then assign colors based on that.
  # Otherwise, create a palette with the provided colors

  group_by <- c("Cycle")
  if (mgmt_unit) {
    group_by <- c("GROUP_COL", group_by)
  }

  und <- understorySpeciesCover2(sample_frame = sample_frame, cycle = cycle, group_by = group_by)

  if (is.null(names(colors))) {
    pal <- grDevices::colorRampPalette(colors)
    n_colors <- length(unique(und[[plot_levels[1]]]))
    colors <- pal(n_colors)
    names(colors) <- sort(unique(und[[plot_levels[1]]]))
  } else if (!all(unique(und[[plot_levels[1]]]) %in% names(colors))) {
    stop("If `colors` is a named vector, its names must match the values of the first column in `group_by`")
  }

  # Create sunburst plot
  sb <- dplyr::select(und, tidyselect::all_of(c(plot_levels, "Avg_Cover")))
  sb <- as.sunburstDF(sb, value_column = "Avg_Cover")
  sb$color <- colors[stringr::str_replace(sb$ids, " - .*", "")]

  und_species <- und %>%
    dplyr::select(Code, Scientific_Name)

  sb <- sb %>%
    dplyr::left_join(und_species, by = c("labels" = "Code")) %>%
    dplyr::mutate(Scientific_Name = tidyr::replace_na(Scientific_Name, ""))

  sunburst <- plotly::plot_ly(sb,
                              ids = ~ids,
                              labels = ~labels,
                              parents = ~parents,
                              values = ~values,
                              type = 'sunburst',
                              branchvalues = 'total',
                              marker = list(colors = ~color),
                              hoverinfo = "text",
                              hovertext = paste0(sb$labels,
                                                "<br>Avg: ", sb$values, "%",
                                                "<br>", sb$Scientific_Name))

  return(sunburst)
}

#' Bar plot of avg. % cover vs cycle
#'
#' Broken down by management unit
#'
#' @inheritParams LoadPACNVeg
#' @param crosstalk_filters Include dropdowns to filter on species, nativity, and mgmt unit?
#' @param colors Character vector of color names or hex values. This can be a named vector with names corresponding to each management unit, or you can provide an unnamed vector of any number of colors to create a gradient.
#' @return An HTML object (if `crosstalk_filters == TRUE`) or a plotly object.
#' @export
#'
understoryBarCover <- function(sample_frame, crosstalk_filters = TRUE,
                               colors = c("#F8573A", "#F4C47B", "#28468B", "#AED5CB")) {

  if (missing(sample_frame)) {
    stop("`sample_frame` is required")
  }

  und <- understorySpeciesCover2(sample_frame = sample_frame) %>%
    dplyr::mutate(Cycle = as.factor(Cycle),
                  all = "Select all",
                  hovertext = paste(Scientific_Name, Nativity,
                                    paste0(Avg_Cover, "%"), sep = "\n")) %>%
    dplyr::filter(Avg_Cover > 0) %>%
    dplyr::group_by(Zone)

  sp_w_cover <- und %>%
    crosstalk::SharedData$new()

  cover_bar <- sp_w_cover %>%
    plotly::plot_ly(type = "bar", x = ~Cycle, y = ~Avg_Cover, color = ~Zone, strokes = "white", stroke = ~Scientific_Name,
                    colors = grDevices::colorRamp(colors),
                    text = ~hovertext,
                    hoverinfo = 'text',
                    textposition = "none") %>%
    plotly::layout(xaxis = list(title = "Cycle"),
                   yaxis = list(title = "Average % cover"),
                   legend = list(itemclick = FALSE,
                                 itemdoubleclick = FALSE),
                   clickmode = "none")

  if (crosstalk_filters) {
    sp_filter <- crosstalk::filter_select("select-sp", "Filter on species", sp_w_cover, ~Scientific_Name, allLevels = FALSE)
    nat_filter <- crosstalk::filter_select("select-nat", "Filter on nativity", sp_w_cover, ~Nativity, allLevels = FALSE)
    mgmt_filter <- crosstalk::filter_select("select-sp", "Filter on management unit", sp_w_cover, ~Zone, allLevels = FALSE)
    nat_filter <- crosstalk::filter_select("select-nat", "Filter on nativity", sp_w_cover, ~Nativity, allLevels = FALSE)

    show_all <- crosstalk::filter_select("show_all", "", sp_w_cover, ~all, allLevels = FALSE, multiple = FALSE)

    plot <- crosstalk::bscols(list(sp_filter, nat_filter, mgmt_filter), cover_bar, show_all, widths = c(3, 9, 0))
  } else {
    plot <- cover_bar
  }

  return(plot)
}

#' Title
#'
#' @inheritParams LoadPACNVeg
#' @param group_by Columns to group by. Must be a subset of `c("GROUP_COL", "Cycle", "Nativity")`
#'
#' @return A tibble
#' @export
#'
understorySpeciesCover <- function(sample_frame, cycle, group_by = c("GROUP_COL", "Cycle", "Nativity")) {
  col_order <- c("Unit_Code",
                 "GROUP_COL",
                 "Sampling_Frame",
                 "Cycle",
                 "Code",
                 "Scientific_Name",
                 "Life_Form",
                 "Nativity",
                 "Avg_Cover",
                 "Std_Dev",
                 "n",
                 "plots_present")

  und <- FilterPACNVeg("Understory", sample_frame = sample_frame, cycle = cycle)  # Only get data from most recent cycle

  # PLACEHOLDER
  # TODO: replace this with actual grouping column
  set.seed(11) # Set random number generation seed so that GROUP_COL is the same each time
  und <- und %>%
    dplyr::group_by(Cycle, Sampling_Frame, Plot_Number) %>%
    dplyr::mutate(GROUP_COL = sample(LETTERS[c(1:5)], size = dplyr::n(), replace = TRUE))

  und <- und %>%
    dplyr::mutate(Life_Form=replace(Life_Form, Code=="SOPCHR", "Shrub"))

  # prep data for sunburst plot
  und <- UnderCombineStrata(und) %>%
    dplyr::mutate(across(where(is.character), replace_na, "No Veg")) %>%
    dplyr::group_by(across(tidyselect::all_of(c("Unit_Code", "Sampling_Frame", "Plot_Number", "Life_Form", "Scientific_Name", "Code", group_by)))) %>%
    dplyr::summarize(Hits_Sp = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(tidyr::nesting(!!!syms(c("Unit_Code", "Sampling_Frame", "Plot_Number", "Life_Form", "Scientific_Name", "Code", group_by))),
             fill = list(Hits_Sp = 0)) %>%
    dplyr::mutate(Plot_Percent = Hits_Sp/300) %>%
    dplyr::group_by(across(tidyselect::all_of(c("Unit_Code", "Sampling_Frame", "Life_Form", "Scientific_Name", "Code", group_by)))) %>%
    dplyr::summarize(n = dplyr::n(),
                     plots_present = sum(Hits_Sp > 0),
                     Avg_Cover = round(mean(Plot_Percent), 3),
                     Std_Dev = round(sd(Plot_Percent), 3),
                     .groups = "drop")

  # Reorder columns
  col_order <- col_order[col_order %in% names(und)]
  und <- und[, col_order]

  return(und)
}

#' Title
#'
#' @inheritParams LoadPACNVeg
#' @param group_by Columns to group by. Must be a subset of `c("GROUP_COL", "Cycle")`
#'
#' @return A tibble
#' @export
#'
understorySpeciesCover2 <- function(sample_frame, cycle,
                                    group_by = c("Zone", "Cycle")) {
  col_order <- c("Unit_Code",
                 "Zone",
                 "Sampling_Frame",
                 "Cycle",
                 "Code",
                 "Scientific_Name",
                 "Life_Form",
                 "Nativity",
                 "Avg_Cover",
                 "Std_Dev",
                 "n",
                 "plots_present")

  und <- FilterPACNVeg("Understory", sample_frame = sample_frame, cycle = cycle)  # Only get data from most recent cycle

  # PLACEHOLDER
  # replace this with actual grouping column
  #set.seed(11) # Set random number generation seed so that GROUP_COL is the same each time
  #und <- dplyr::mutate(und, GROUP_COL = sample(LETTERS[c(1, 2, 2, 3, 5, 5, 5)], size = dplyr::n(), replace = TRUE))
  #und <- und %>%
  #  dplyr::group_by(Cycle, Sampling_Frame, Plot_Number) %>%
  #  tidyr::nest() %>%
  #  dplyr::mutate(GROUP_COL = sample(LETTERS[1:5], size = dplyr::n(), replace = TRUE)) %>%
  #  tidyr::unnest(data) %>%
  #  dplyr::ungroup()

  # NEW PLACEHOLDER
  # TODO: fix location of mgmt unit (ie where should it first appear?)
  mgmt <- readr::read_csv(file = paste0(getwd(),"/R/Events_extra_xy_mgmt.csv"))
  und <- und %>%
    dplyr::left_join(dplyr::select(mgmt, Zone, Unit_Code, Sampling_Frame, Cycle, Plot_Number),
                     by = c("Unit_Code", "Sampling_Frame", "Cycle", "Plot_Number"))


  #und <- und %>%
  #  dplyr::mutate(Life_Form=replace(Life_Form, Code=="SOPCHR", "Shrub"))

  # prep data for sunburst plot
  und <- UnderCombineStrata(und) %>%
    dplyr::mutate(across(where(is.character), tidyr::replace_na, "No Veg")) %>%
    dplyr::group_by(across(tidyselect::all_of(c("Unit_Code", "Sampling_Frame", "Plot_Number", "Nativity", "Life_Form", "Scientific_Name", "Code", group_by)))) %>%
    dplyr::summarize(Hits_Sp = dplyr::n(), .groups = "drop") %>%
    #complete(tidyr::nesting(!!!syms(c("Unit_Code", "Sampling_Frame", "Plot_Number", "Life_Form", "Scientific_Name", "Code", group_by))),
    #         fill = list(Hits_Sp = 0)) %>%
    tidyr::complete(tidyr::nesting(!!!syms(c("Unit_Code", "Sampling_Frame", "Plot_Number", group_by))),
             tidyr::nesting(!!!syms(c("Nativity", "Code", "Scientific_Name", "Life_Form"))),
             fill = list(Hits_Sp = 0)) %>%
    dplyr::mutate(Plot_Percent = Hits_Sp/300) %>%
    dplyr::group_by(across(tidyselect::all_of(c("Unit_Code", "Sampling_Frame", "Nativity", "Life_Form", "Scientific_Name", "Code", group_by)))) %>%
    dplyr::summarize(n = dplyr::n(),
                     plots_present = sum(Hits_Sp > 0),
                     Avg_Cover = round(mean(Plot_Percent), 3) *100,
                     Std_Dev = round(sd(Plot_Percent), 3) *100,
                     .groups = "drop")

  # Reorder columns
  col_order <- col_order[col_order %in% names(und)]
  und <- und[, col_order]

  return(und)
}

#' Species understory cover trends, ranked.
#'
#' @inheritParams summarize_understory
#' @param combine_strata If `TRUE`(default), combine high and low strata into one stratum.
#' @param paired_change If `FALSE` (default) analysis utilizes both "Fixed" and "Rotational" plots in jitter plot and trend.
#' If `TRUE`, drops "Rotational" plots and uses only "Fixed" plots for analysis, in addition, the geom_jitter points are connected with lines to show trends for each plot across cycles.
#' @param plant_grouping set to "Species" (default), Cover values for individual plant species.
#' Note that the other options in summarize_understory() have not been tested yet for understory_spp_trends_rank():
#'
#' "None" = No Groups, all vegetation (native & non-native) is lumped together,
#' "Nativity" = Cover values lumped by native and non-native vegetation.
#' "Life_Form" = Cover values grouped into native and non-native lifeforms (e.g. Trees, Shrubs, Ferns).
#' @param top_n default set to top_n = 5. The number of ranked species to include in output (e.g. top 5, top 10, etc.)
#' @param rank_by Used with paired_change = TRUE. Should the final graph be ranked by "positive" trend (default) or "negative" trend.
#' @param remove_nativity option to remove a Nativity category. For example remove_nativity = "Native" will only show non-native species in final graph.
#'
#' @return Figure showing species cover per plot as geom_jitter graph with mean trend over all cycles
#' @export
#'
#' @examples
#' \dontrun{
#' understory_spp_trends_rank(sample_frame = "Puu Alii", top_n = 5, rank_by = "positive", remove_nativity = "Native", paired_change = FALSE)
#'
#' }


understory_spp_trends_rank <- function(combine_strata = TRUE,
                                       plant_grouping = "Species",
                                       paired_change = TRUE,
                                       sample_frame = sample_frame,
                                       cycle = cycle,
                                       top_n = 5,
                                       rank_by = "positive",
                                       remove_nativity = NULL) {

  if (missing(sample_frame)) {
    stop("sample_frame variable is missing")
  }

  # Understory Dataset
  und_spp <- pacnvegetation::summarize_understory(
    combine_strata = combine_strata,
    plant_grouping = plant_grouping,
    paired_change = paired_change,
    sample_frame = sample_frame,
    cycle = cycle)

  if ("Nahuku/East Rift" %in% sample_frame) {
    # Temporary solution until zone/mgmt layer incorporated into package:
    print("Removing -East Rift- plots from analysis. Function needs updated when Nahuku/East Rift Cycle 4 data are added")

    nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15, #fixed
                      46, 49, 51, 52, 54, 55, 56, 58, #2021 rotational
                      24, 26, #2010 rotational
                      31, 32, 33, 34, 35, 38, 41, 45)

    if (paired_change == TRUE) {
      nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15) #fixed only
    }

    nahuku_only <- und_spp |>
      filter(Sampling_Frame == "Nahuku/East Rift") |>
      filter(Plot_Number %in% nahuku_plots)
    und_spp <- und_spp |>
      filter(Sampling_Frame != "Nahuku/East Rift") |>
      bind_rows(nahuku_only)
  }

  # plot count
  plot_counts_table <- und_spp |>
    dplyr::group_by(Cycle, Year, Sampling_Frame, Scientific_Name) |>
    dplyr::summarise(plots = n_distinct(Plot_Number)) |>
    dplyr::mutate(plots_text = paste0(Year, " [n=", plots, "]"))

  plot_counts_vector <- plot_counts_table |>
    dplyr::pull(plots_text) |>
    unique()

  plot_counts_vector

  # rank by max change per year in a plot
  if (!is.null(remove_nativity)) {
    und_spp <- und_spp |>
      dplyr::filter(Nativity != remove_nativity)
  }


  if (paired_change == TRUE) {
    ranking_stat <- "Chg_Per_Year"
  } else {
    ranking_stat <- "Cover"
  }


  if (rank_by == "positive") {
    max_chg_rank <- und_spp |>
      dplyr::group_by(Sampling_Frame, Scientific_Name, Code) |>
      dplyr::slice_max(.data[[ranking_stat]], n = 1, with_ties = FALSE) |>
      dplyr::arrange(desc(.data[[ranking_stat]])) |>
      head(top_n)
  }

  if (rank_by == "negative") {
    max_chg_rank <- und_spp |>
      dplyr::group_by(Sampling_Frame, Scientific_Name, Code) |>
      dplyr::slice_min(.data[[ranking_stat]], n = 1, with_ties = FALSE) |>
      dplyr::arrange(.data[[ranking_stat]]) |>
      head(top_n)
  }

  # Get top species for selection
  top_n_baddies <- max_chg_rank |> #increasing fastest in any plot
    dplyr::pull(Scientific_Name)



  #############################################################################-
  # All Plots ----
  #############################################################################-

  if (paired_change == FALSE) {

    print_baddie <- max_chg_rank |>
      dplyr::mutate(print_baddie = paste0(Code, " had ", round(Cover, digits = 1), "% cover in plot ", Plot_Number, " in ", Year))
    print_baddie$print_baddie


    # make table that can be used to highlight the max period of change
    max_chg_highlight <- max_chg_rank |>
      dplyr::mutate(highlight_this = 'light blue')

    library(gghighlight)

    und_spp_filter <- und_spp |>
      dplyr::filter(Scientific_Name %in% top_n_baddies)

    #library(ggpubr)
    #ggdensity(und_spp_filter$Cover)
    #ggqqplot(und_spp_filter$Cover)
    #shapiro.test(und_spp_filter$Cover)
    out_graph <- und_spp_filter |>
      dplyr::left_join(max_chg_highlight) |>
      dplyr::mutate(Scientific_Name = factor(Scientific_Name, top_n_baddies)) |>
      ggplot2::ggplot(aes(x = Year, y = Cover, group = Code)) +
      ggplot2::geom_jitter(position=position_jitter(0.1, seed = 1), colour = "grey40", size = 1) +
      ggplot2::geom_jitter(position=position_jitter(0.1, seed = 1), aes(colour = highlight_this), size = 1) +
      stat_summary(geom = "line", fun = mean) +
      # Because shapiro.test shows data not normally distributed use mean_cl_boot for confidence limits
      ggplot2::stat_summary(mapping=aes(group=Code), fun.data = ggplot2::mean_cl_boot, geom="pointrange", position = position_dodge(.2)) +
      ggplot2::ggtitle(paste("Understory Cover")) +
      ggplot2::xlab("Year") +
      ylab("percent cover (%)") +
      ggplot2::facet_grid(cols = vars(Scientific_Name), rows = vars(Sampling_Frame)) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(caption = paste(plot_counts_vector, collapse = ", "))
  }

  #############################################################################-
  # Fixed Plots ----
  #############################################################################-

  if (paired_change == TRUE) {
    print_baddie <- max_chg_rank |>
      dplyr::mutate(print_baddie = paste0(Code, " increased by ", round(Chg_Per_Year, digits = 1), "% cover (per year) in plot ", Plot_Number, " in ", Year))
    print_baddie$print_baddie

    # Get year prior observation values for highlighting time period on graph
    prior_year_join_geom_seg <- max_chg_rank |>
      dplyr::mutate(prior_year = as.factor(as.integer(as.character(Year)) - Years_Prior)) |>
      dplyr::select(-Cycle) |> # Keep info the same but select prior year
      dplyr::select(Unit_Code, Sampling_Frame, Year_End = Year, Year_Start = prior_year,
             Plot_Number, Code, Scientific_Name) |>
      dplyr::left_join(und_spp, by = join_by(Unit_Code, Year_Start == Year, Sampling_Frame, Plot_Number, Code, Scientific_Name)) |>
      dplyr::left_join(und_spp, by = join_by(Unit_Code, Year_End == Year, Sampling_Frame, Plot_Number, Code, Scientific_Name)) |>
      dplyr::select(Unit_Code, Sampling_Frame, Year_End, Year_Start,
             Plot_Number, Code, Scientific_Name, Cover_Start = Cover.x, Cover_End = Cover.y)

    prior_year_join <- max_chg_rank |>
      dplyr::mutate(prior_year = as.factor(as.integer(as.character(Year)) - Years_Prior)) |>
      dplyr::select(-Cycle, -Year) |> # Keep info the same but select prior year
      dplyr::select(Unit_Code, Sampling_Frame, Year = prior_year,
             Plot_Number, Code, Scientific_Name) |>
      dplyr::left_join(und_spp, by = join_by(Unit_Code, Year, Sampling_Frame, Plot_Number, Code, Scientific_Name))

    max_chg_highlight <- max_chg_rank |>
      dplyr::bind_rows(prior_year_join) |>
      dplyr::mutate(highlight_this = 'yellow')

    und_spp_filter <- und_spp |>
      dplyr::filter(Scientific_Name %in% top_n_baddies)

    common_names <- FilterPACNVeg(data_name = "Species_extra") |>
      dplyr::filter(Park == "HAVO") |>
      dplyr::select(Park_Common_Name, Code)

    new_und_spp_filter <- und_spp_filter |>
      dplyr::left_join(max_chg_highlight) |>
      dplyr::mutate(Scientific_Name = factor(Scientific_Name, top_n_baddies)) |>
      dplyr::mutate(highlight_this = case_when(is.na(highlight_this) ~ NA, .default = highlight_this)) |>
      dplyr::mutate(plot_line = paste0(Sampling_Frame, "_", Code, "_", Plot_Number)) |>
      dplyr::arrange(Scientific_Name, plot_line, Year) |>
      dplyr::left_join(common_names, by = join_by(Code))

    out_graph <- new_und_spp_filter |>
      ggplot2::ggplot(aes(x = Year, y = Cover, group = Code)) +
      ggplot2::geom_line(position=position_jitter(width = 0.1, height = 0.1, seed = 1), aes(group = plot_line), colour = "grey65", size = 0.5) +
      ggplot2::geom_line(position=position_jitter(width = 0.1, height = 0.1, seed = 1), aes(group = plot_line, colour = highlight_this), size = 1, alpha = 0.5) +
      ggplot2::geom_jitter(position=position_jitter(width = 0.1, height = 0.1, seed = 1), aes(group = plot_line, colour = highlight_this), size = 2) +
      ggplot2::geom_jitter(position=position_jitter(width = 0.1, height = 0.1, seed = 1), colour = 'grey65', size = 1) +
      ggplot2::scale_colour_identity() +
      ggplot2::stat_summary(geom = "line", fun = mean) +
      ggplot2::stat_summary(mapping=aes(group=Code), fun.data = ggplot2::mean_cl_boot, geom="pointrange", position = position_dodge(.2)) +
      ggplot2::facet_grid(cols = vars(Scientific_Name), rows = vars(Sampling_Frame)) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(y = "Understory Cover (%)", caption = paste(plot_counts_vector, collapse = ", "))

  }

  return(out_graph)

}

