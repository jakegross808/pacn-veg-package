

#' Get monitoring years for a sample frame
#'
#' If there are multiple monitoring years per cycle, assumes that the year with the most visits is the one that represents that cycle. You should verify that this is in fact the case for your data.
#'
#' @inheritParams FilterPACNVeg
#' @return A vector of years
#' @export
#'
get_years <- function(sample_frame) {
  years <- FilterPACNVeg("Events_extra_xy", sample_frame = sample_frame) %>%
    dplyr::select(Year, Cycle) %>%
    dplyr::arrange(Cycle, Year) %>%
    dplyr::group_by(Cycle, Year) %>%
    dplyr::mutate(Count = n()) %>%
    unique() %>%
    dplyr::group_by(Cycle) %>%
    dplyr::mutate(MaxCount = max(Count)) %>%
    dplyr::filter(Count == MaxCount)
  years <- years[["Year"]]  # get years as vector instead of tibble

  return(years)
}

#' Get cycles for a sample frame
#'
#' @inheritParams FilterPACNVeg
#'
#' @return A vector of cycles
#' @export
#'
get_cycles <- function(sample_frame) {
  cycles <- FilterPACNVeg("Events_extra_xy", sample_frame = sample_frame) %>%
    dplyr::select(Cycle) %>%
    unique() %>%
    dplyr::arrange(Cycle)
  cycles <- cycles[["Cycle"]]

  return(cycles)
}

#' Create DT tables with consistent formatting
#'
#' @param data A dataframe/tibble
#' @param record_count Display record count?
#' @param search Display search box?
#' @param ... Additional arguments to pass to [DT::datatable]
#'
#' @return A data table
#' @export
#'
pacn_dt <- function(data, record_count = TRUE, search = TRUE, ...) {
  dom <- "t"
  if (record_count) {
    dom <- paste0(dom, "i")
  }
  if (search) {
    dom <- paste0(dom, "f")
  }

  dt <- DT::datatable(data,
                rownames = FALSE,
                selection = "multiple",
                options = list(dom = dom,
                               paging = FALSE,
                               scrollY = "200px",
                               scrollCollapse = TRUE), ...)
  return(dt)
}

#' Create a numbered caption for a DT datatable.
#' Call this right before or right after the call to DT (depending whether you want the caption on top or bottom)
#'
#' @param caption The caption text
#'
#' @export
#'
add_table_caption <- function(caption) {
  knitr::kable(tibble::tibble(), caption = caption, table.attr = "style='width:100%;'")
}

#' Prep data for a sunburst plot
#' Shamelessly copied from https://stackoverflow.com/a/58481176
#'
#' @param data A dataframe/tibble
#' @param value_column The name of the column containing the value to be plotted
#' @param add_root Include a "Total" node in the center of the plot?
#'
#' @return A tibble properly formatted for use in a Plotly sunburst plot
#' @export
#'
as.sunburstDF <- function(data, value_column = NULL, add_root = FALSE){
  colNamesDF <- names(data)

  if(data.table::is.data.table(data)){
    DT <- data.table::copy(data)
  } else {
    DT <- data.table::data.table(data, stringsAsFactors = FALSE)
  }

  if(add_root){
    DT[, root := "Total"]
  }

  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]

  if(is.null(value_column) && add_root){
    data.table::setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    data.table::setnames(DT, value_column, "values", skip_absent=TRUE)
    data.table::setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    data.table::setnames(DT, value_column, "values", skip_absent=TRUE)
    data.table::setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }

  hierarchyList <- list()

  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    data.table::setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }

  hierarchyDT <- data.table::rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)

  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){data.table::fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  return(hierarchyDT)
}
