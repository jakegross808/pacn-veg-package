sample_frame = "Kahuku"
crosstalk_filters = TRUE

sum_und1 <- summarize_understory(combine_strata = TRUE,
                                 plant_grouping = "Species",
                                 paired_change = FALSE,
                                 sample_frame = "Kahuku")

sum_und2 <-sum_und1 %>%
  dplyr::mutate(Cycle = as.factor(Cycle),
                all = "Select all",
                hovertext = paste(Scientific_Name, Nativity, paste0(Cover, "%"), sep = "\n")) %>%
  dplyr::filter(Cover > 0) #%>%
  #dplyr::group_by(GROUP_COL)

sp_w_cover <- sum_und2 %>%
  crosstalk::SharedData$new()

cover_bar <- sp_w_cover %>%
  plotly::plot_ly(type = "bar", x = ~Life_Form, y = ~Cover, color = ~Nativity, strokes = "white", stroke = ~Scientific_Name,
                  #colors = colorRamp(colors),
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
  year_filter <- crosstalk::filter_select("select-year", "Filter on year", sp_w_cover, ~Year, allLevels = FALSE, multiple = FALSE)
  plot_filter <- crosstalk::filter_select("select-plot", "Filter on plot", sp_w_cover, ~Plot_Number, allLevels = FALSE, multiple = FALSE)
  #sp_filter <- crosstalk::filter_select("select-sp", "Filter on species", sp_w_cover, ~Scientific_Name, allLevels = FALSE, multiple = FALSE)
  #nat_filter <- crosstalk::filter_select("select-nat", "Filter on nativity", sp_w_cover, ~Nativity, allLevels = FALSE, multiple = FALSE)
  #mgmt_filter <- crosstalk::filter_select("select-sp", "Filter on management unit", sp_w_cover, ~GROUP_COL, allLevels = FALSE)
  #nat_filter <- crosstalk::filter_select("select-nat", "Filter on nativity", sp_w_cover, ~Nativity, allLevels = FALSE)

  #show_all <- crosstalk::filter_select("show_all", "", sp_w_cover, ~all, allLevels = FALSE, multiple = FALSE)

  plot <- crosstalk::bscols(list(year_filter, plot_filter), cover_bar, widths = c(3, NA))
} else {
  plot <- cover_bar
}

plot


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
v_cover_bar_spp_plot <- function(sample_frame, crosstalk_filters = TRUE) {

  if (missing(sample_frame)) {
    stop("`sample_frame` is required")
  }

  und <- understorySpeciesCover2(sample_frame = sample_frame) %>%
    dplyr::mutate(Cycle = as.factor(Cycle),
                  all = "Select all",
                  hovertext = paste(Scientific_Name, Nativity, paste0(Avg_Cover, "%"), sep = "\n")) %>%
    dplyr::filter(Avg_Cover > 0) #%>%
    #dplyr::group_by(GROUP_COL)

  sp_w_cover <- und %>%
    crosstalk::SharedData$new()

  cover_bar <- sp_w_cover %>%
    plotly::plot_ly(type = "bar", x = ~Cycle, y = ~Avg_Cover, color = ~GROUP_COL, strokes = "white", stroke = ~Scientific_Name,
                    colors = colorRamp(colors),
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
    mgmt_filter <- crosstalk::filter_select("select-sp", "Filter on management unit", sp_w_cover, ~GROUP_COL, allLevels = FALSE)
    nat_filter <- crosstalk::filter_select("select-nat", "Filter on nativity", sp_w_cover, ~Nativity, allLevels = FALSE)

    show_all <- crosstalk::filter_select("show_all", "", sp_w_cover, ~all, allLevels = FALSE, multiple = FALSE)

    plot <- crosstalk::bscols(list(sp_filter, nat_filter, mgmt_filter), cover_bar, show_all, widths = c(3, 9, 0))
  } else {
    plot <- cover_bar
  }

  return(plot)
}
