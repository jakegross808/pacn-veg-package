# put two iterations in Rmd so plots can be compared?
# set default? if so use: https://stackoverflow.com/questions/67058016/how-to-set-default-values-in-filter-select-in-crosstalk-in-r-plotly

sample_frame = "Kahuku"
crosstalk_filters = TRUE

sum_und1 <- summarize_understory(combine_strata = TRUE,
                                 plant_grouping = "Species",
                                 paired_change = FALSE,
                                 sample_frame = "Kahuku")

sum_und2 <-sum_und1 %>%
  dplyr::mutate(Cycle = as.factor(Cycle),
                all = "Select all",
                hovertext = paste(Scientific_Name, Nativity, paste0(round(Cover, digits = 1), "%"), sep = "\n")) %>%
  # Filter zero cover
  dplyr::filter(Cover > 0) %>%
  # Filter No Veg Cover
  dplyr::filter(!is.na(Scientific_Name))

sp_w_cover <- sum_und2 %>%
  crosstalk::SharedData$new()

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
    crosstalk::filter_select("select-plot", "Filter on plot", sp_w_cover, ~Plot_Number, allLevels = FALSE, multiple = FALSE),
    widths = c(6,6))

  plot <- crosstalk::bscols(filters,
                            cover_bar,
                            widths = c(12, 12))
} else {
  plot <- cover_bar
}

plot


#' Bar plot of species % cover for each sampling plot
#'
#' Can be placed side-by-side to compare two plots
#'
#' @inheritParams LoadPACNVeg
#' @param sample_frame (required)
#' @param crosstalk_filters Include dropdowns to filter on species, nativity, and mgmt unit?
#' @return An HTML object (if `crosstalk_filters == TRUE`) or a plotly object.
#' @export
#'
v_cover_bar_spp_plot <- function(sample_frame, crosstalk_filters = TRUE) {

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
    dplyr::filter_all(any_vars(is.na(.)))

  if (nrow(message_nas) > 0) {
    warning(paste(nrow(message_nas), "rows with 'NA' in dataset", sep = " "))
    message(paste0(capture.output(message_nas), collapse = "\n"))
    }

  sp_w_cover <- sum_und2 %>%
    crosstalk::SharedData$new()

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
      crosstalk::filter_select("select-plot", "Filter on plot", sp_w_cover, ~Plot_Number, allLevels = FALSE, multiple = FALSE),
      widths = c(6,6))

    plot <- suppressWarnings(crosstalk::bscols(filters,
                              cover_bar,
                              widths = c(12, 12)))
  } else {
    plot <- cover_bar
  }

  return(plot)

}

v_cover_bar_spp_plot(sample_frame = "Kahuku",
                     crosstalk_filters = TRUE)


