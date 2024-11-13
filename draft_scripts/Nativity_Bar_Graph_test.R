new_vars <- c()
plant_grouping <- "Nativity"
filter_codes <- c()

  # Get raw data
cover <- summarize_understory(sample_frame = "Haleakala", plant_grouping = plant_grouping, paired_change = FALSE)

unkcov <- cover %>%
    dplyr::filter(!!rlang::sym(plant_grouping) == "Unknown" & Cover > 0)

unkcov_tot <- unkcov %>%
    dplyr::pull(Cover) %>%
    sum()

cover3 <- cover %>%
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
                                Cycle, Year, Stratum, Nativity)


# format sample size for text print out
sample_size <- haleakala_nativity_paired_stats %>%
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
plot <- cover_stats %>%
  #mutate(muli_SF = NA) %>%
  dplyr::filter(Parameter == "Cover") %>%
  ggplot2::ggplot(ggplot2::aes(x = Year, y = MEAN, fill = Nativity)) +
  ggplot2::geom_col(position = position_dodge()) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=L, ymax=R), width=.2,
                             position=position_dodge(.9)) +
  ggplot2::labs(y = "Mean % Cover") +
  #ggh4x::facet_nested(Stratum ~ Sampling_Frame + Nativity, scales = "free_x") +
  ggplot2::facet_grid(Stratum ~ Sampling_Frame + Nativity, labeller = label_parsed) +
  ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
  ggplot2::xlab("Year") +
  ggplot2::theme(legend.position="none") +
  ggplot2::labs(caption = sample_size) +
  ggplot2::theme(axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5))
plot
