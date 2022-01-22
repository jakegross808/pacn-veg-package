new_vars <- c()
plant_grouping <- "Nativity"
filter_codes <- c()

  # Get raw data
cover <- summarize_understory(sample_frame = "Olaa", plant_grouping = plant_grouping, paired_change = FALSE)

cover2 <- cover %>%
    dplyr::group_by(Sampling_Frame, Cycle) %>%
    dplyr::mutate(Year = min(Year)) %>%
    dplyr::mutate(Year = as.factor(Year)) %>%
    dplyr::ungroup()

unkcov <- cover2 %>%
    dplyr::filter(!!rlang::sym(plant_grouping) == "Unknown" & Cover > 0)

unkcov_tot <- unkcov %>%
    dplyr::pull(Cover) %>%
    sum()

cover3 <- cover2 %>%
    dplyr::filter(Stratum != "No_Veg",
                  !!rlang::sym(plant_grouping) != "Unknown")
print(paste0(round(unkcov_tot,2), "% cover of species with unknown Nativity removed"))

  # Nativity discrete scale Colors:
nativity_colors <- c("Native" = "#1b9e77",
                       "No_Veg" = "grey",
                       "Non-Native" = "#d95f02",
                       "Unknown" = "#7570b3")

  # add stats
cover_stats <- add_stats(cover3, Unit_Code, Sampling_Frame,
                                Cycle, Year, Stratum, !!!rlang::syms(new_vars))


    # sample size calculation for text
sample_size <- cover_stats %>%
      dplyr::select(Year, NPLOTS) %>%
      dplyr::distinct() %>%
      dplyr::mutate(Text = paste0(Year, " [n = ", NPLOTS, "]")) %>%
      dplyr::pull(Text) %>%
      paste(collapse = ", ")
    sample_size

    #........BAR YEARLY MEANS
plot <- cover_stats %>%
  dplyr::filter(Code %in% filter_codes) %>%
  dplyr::filter(Parameter == "Cover") %>%
  ggplot2::ggplot(ggplot2::aes_string(x = plant_grouping, y = "MEAN", fill = "Nativity", alpha = "Year")) +
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
plot
