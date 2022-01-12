
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


