#' Plot Understory Cover by Nativity, Life_Form, or Species
#'
#' @inheritParams summarize_understory
#'
#' @return graph (gglot) of total Percent Cover by grouping or change in total Percent Cover by grouping.
#' @export
#'
#' @examples
#' \dontrun{
#' bar_plot_native_vs_nonnative <- plot_understory(plant_grouping = "Nativity")
#' bar_plot_spp_cover_chg <- plot_understory(plant_grouping = "Species", paired_change = TRUE)
#'
#' }

plot_understory <- function(combine_strata = FALSE, plant_grouping,
                            paired_change = FALSE, park, sample_frame, community,
                            year, cycle, plot_type, plot_number, filter_Code, silent = FALSE) {
  if (missing(plant_grouping)) {
    stop("plant_grouping variable is missing")
  }


  # Get raw data
  understory <- summarize_understory(combine_strata = combine_strata,
                                     plant_grouping = plant_grouping,
                                     paired_change = paired_change,
                                     park = park, sample_frame = sample_frame,
                                     community = community, year = year,
                                     cycle = cycle, plot_type = plot_type,
                                     plot_number = plot_number, silent = silent)

  understory2 <- understory %>%
    dplyr::group_by(Sampling_Frame, Cycle) %>%
    dplyr::mutate(Year = min(Year)) %>%
    dplyr::mutate(Year = as.factor(Year)) %>%
    dplyr::ungroup()

  if (plant_grouping == "None") {
    x_var <- "Year"
    fill_var <- "Year"
    understory3 <- understory2
  }

  if (plant_grouping == "Nativity") {
    x_var <- "Nativity"
    fill_var <- "Nativity"

    unknown_cover <- understory2 %>%
      dplyr::filter(!!!rlang::sym(plant_grouping) == "Unknown" & Cover > 0)

    unk_cover_tot <- unknown_cover %>%
      dplyr::pull(Cover) %>%
      sum()

    understory3 <- understory2 %>%
      dplyr::filter(Stratum != "No_Veg",
                    Nativity != "Unknown")
    print(paste0(round(unk_cover_tot,2), "% cover of species with unknown Nativity removed"))

  }

  if (plant_grouping == "Life_Form") {
    x_var <- "Life_Form"
    fill_var <- "Nativity"
    understory3 <- understory2
  }

  if (plant_grouping == "Species") {
    x_var <- "Code"
    fill_var <- "Nativity"
    understory3 <- understory2
  }


  # Nativity discrete scale Colors:
  nativity_colors <- c("Native" = "#1b9e77",
                       "No_Veg" = "grey",
                       "Non-Native" = "#d95f02",
                       "Unknown" = "#7570b3")

  # add stats
  understory_stats <- add_stats(understory3, Unit_Code, Sampling_Frame,
                                Cycle, Year, Stratum, !!!rlang::syms(new_vars))

  # apply species Code filter if passed as argument
  if (missing(filter_Code)) {
    understory_stats_filter <- understory_stats
  } else
  {understory_stats_filter <- understory_stats %>%
    dplyr::filter(Code %in% filter_Code)}


  # sample size calculation for text
  sample_size <- understory_stats_filter %>%
    dplyr::select(Year, NPLOTS) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Text = paste0(Year, " [n = ", NPLOTS, "]")) %>%
    dplyr::pull(Text) %>%
    paste(collapse = ", ")
  sample_size

  #........BAR YEARLY MEANS
  plot <- understory_stats_filter %>%
    dplyr::filter(Parameter == "Cover") %>%
    ggplot2::ggplot(ggplot2::aes_string(x = x_var, y = "MEAN", fill = fill_var, alpha = "Year")) +
    ggplot2::geom_col(position = position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=L, ymax=R), width=.2,
                           position=position_dodge(.9)) +
    labs(y = "Mean % Cover") +
    ggplot2::facet_grid(Stratum ~ ., space = "free_y") +
    ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
    ggplot2::xlab("Year") +
    #ggplot2::theme(legend.position="none") +
    ggplot2::labs(caption = sample_size) +
    theme(axis.text.x = element_text(angle = 90))


  return(plot)

}
