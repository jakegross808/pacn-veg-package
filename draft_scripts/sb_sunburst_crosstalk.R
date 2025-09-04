library(pacnvegetation)
library(tidyverse)

as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE){
  colNamesDF <- names(DF)

  if(data.table::is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table::data.table(DF, stringsAsFactors = FALSE)
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

LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_1_2021_20220120.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
            cache = TRUE, force_refresh = FALSE)

und <- FilterPACNVeg("Understory", sample_frame = "Haleakala", cycle = 2)

und <- und %>%
  mutate(Life_Form=replace(Life_Form, Code=="SOPCHR", "Shrub"))

# prep data for sunburst plot
nativity_colors <- c("Native" = "#1b9e77", "Non-Native" = "#d95f02", "No Veg" = "grey", "Unknown" = "#7570b3")

und2 <- UnderCombineStrata(und) %>%
  mutate(across(everything(), replace_na, "No Veg")) %>%
  dplyr::group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number,
           Nativity, Code, Scientific_Name, Life_Form) %>%
  dplyr::summarize(Hits_Sp = n(), .groups = "drop") %>%
  complete(tidyr::nesting(Cycle, Unit_Code, Sampling_Frame, Plot_Number),
           tidyr::nesting(Nativity, Code, Scientific_Name, Life_Form),
           fill = list(Hits_Sp = 0)) %>%
  mutate(Plot_Percent = Hits_Sp/300) %>%
  dplyr::group_by(Cycle, Unit_Code, Sampling_Frame,
           Nativity, Code, Scientific_Name, Life_Form) %>%
  dplyr::summarize(n = n(),
            plots_present = sum(Hits_Sp > 0),
            Avg_Cover = round(mean(Plot_Percent), 3),
            #Median = median(Plot_Percent),
            Std_Dev = round(sd(Plot_Percent), 3),
            .groups = "drop")

# Create sunburst plot
sb <- select(und2, Nativity, Life_Form, Code, Avg_Cover)
sb <- as.sunburstDF(sb, value_column = "Avg_Cover")

plotly::plot_ly(sb, ids = ~ids, labels = ~labels, parents = ~parents, values = ~values, type = 'sunburst', branchvalues = 'total') %>%
  plotly::layout(colorway = nativity_colors)

und_nest <- UnderCombineStrata(und) %>%
  mutate(across(everything(), replace_na, "No Veg")) %>%
  dplyr::group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number,
           Nativity, Code, Scientific_Name, Life_Form) %>%
  dplyr::summarize(Hits_Sp = n(), .groups = "drop") %>%
  complete(tidyr::nesting(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number),
           tidyr::nesting(Nativity, Code, Scientific_Name, Life_Form),
           fill = list(Hits_Sp = 0)) %>%
  mutate(Plot_Percent = Hits_Sp/300) #%>%

und_nest2 <- und_nest %>%
  nest(data = c(Nativity, Code, Scientific_Name, Life_Form, Hits_Sp, Plot_Percent)) %>%
  mutate(data = map2(data, Plot_Number, ~mutate(.x, PlotID = .y)))

#names(und_nest2$data) <- unique(und_nest$Plot_Number)
#cols_no_nest <- und_nest2 %>%
#  dplyr::select_if(purrr::negate(purrr::is_list)) %>%
##  select(-Plot_Number) %>%
#  select(-Plot_Type) %>%
#  dplyr::distinct(across()) #select columns that are not a list type

l_All <- purrr::map_dfr(und_nest2$data, bind_rows) %>%
  nest(data = everything()) %>%
  mutate(Plot_Number = "All")


l_Fixed <- und_nest2 %>%
  dplyr::filter(Plot_Type=="Fixed")
l_Fixed <- purrr::map_dfr(l_Fixed$data, bind_rows) %>%
  nest(data = everything()) %>%
  mutate(Plot_Number = "Fixed")

l_Rotational <- und_nest2 %>%
  dplyr::filter(Plot_Type=="Rotational")
l_Rotational <- purrr::map_dfr(l_Rotational$data, bind_rows) %>%
  nest(data = everything()) %>%
  mutate(Plot_Number = "Rotational")

und_nest3 <- dplyr::bind_rows(und_nest2, l_All, l_Fixed, l_Rotational)

str(und_nest3)

#und_nest4 <- und_nest3 %>%
#  mutate(max_cover = map_dbl(data, ~.x %>%
#                                       pull(Plot_Percent) %>%
#                                       max()))

und_nest4 <- und_nest3 %>%
  mutate(data = map(data,
                    ~.x %>%
                      dplyr::group_by(Nativity, Code, Scientific_Name, Life_Form) %>%
                      summarise(Total_Hits_Sp = sum(Hits_Sp),
                                Mean_Cover = mean(Plot_Percent),
                                Plots_Detected = sum(Hits_Sp > 0, na.rm = TRUE))))

und_nest_sb1 <- und_nest4 %>%
  mutate(data = map(data,
                    ~.x %>%
                      dplyr::ungroup() %>%
                      dplyr::select(Nativity, Life_Form, Code, Mean_Cover)))

und_nest_sb2 <- und_nest_sb1 %>%
  mutate(sunburst_data = map(data, ~.x %>%
                               as.sunburstDF(value_column = "Mean_Cover")))


plotly::plot_ly(und_nest_sb2[[7]][[2]],
                ids = ~ids,
                labels = ~labels,
                parents = ~parents,
                values = ~values,
                type = 'sunburst',
                branchvalues = 'total') %>%
  plotly::layout(colorway = nativity_colors)



create_buttons <- function(df, y_axis_var_names) {
  lapply(
    y_axis_var_names,
    FUN = function(var_name, df) {
      button <- list(
        method = 'restyle',
        args = list('y', list(df[, var_name])),
        label = sprintf('Show %s', var_name)
      )
    },
    df
  )

}

y_axis_var_names <- c('bible', 'simpsons')

plotly::plot_ly(und_nest_sb2[[7]][[2]],
                ids = ~ids,
                labels = ~labels,
                parents = ~parents,
                values = ~values,
                type = 'sunburst',
                branchvalues = 'total',
                updatemenus = list(
                  list(buttons = create_buttons(df, y_axis_var_names)))) %>%
  plotly::layout(colorway = nativity_colors)

p <- plot_ly(df, x = ~x, y = ~y, mode = "markers", name = "A", visible = T) %>%
  layout(
    title = "Drop down menus - Styling",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        y = 0.7,
        buttons = create_buttons(df, y_axis_var_names)
      )
    ))
p

lapply(fam_tree, )



# ----- Idea to filter data in plotly from Sarah:
und2 <- UnderCombineStrata(und) %>%
  mutate(across(everything(), replace_na, "No Veg")) %>%
  dplyr::group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number,
           Nativity, Code, Scientific_Name, Life_Form) %>%
  dplyr::summarize(Hits_Sp = n(), .groups = "drop") %>%
  complete(tidyr::nesting(Cycle, Unit_Code, Sampling_Frame, Plot_Number),
           tidyr::nesting(Nativity, Code, Scientific_Name, Life_Form),
           fill = list(Hits_Sp = 0)) %>%
  mutate(Plot_Percent = Hits_Sp/300) %>%
  dplyr::group_by(Cycle, Unit_Code, Sampling_Frame,
           Nativity, Code, Scientific_Name, Life_Form, Plot_Number) %>%
  dplyr::summarize(n = n(),
            plots_present = sum(Hits_Sp > 0),
            Avg_Cover = round(mean(Plot_Percent), 3),
            #Median = median(Plot_Percent),
            Std_Dev = round(sd(Plot_Percent), 3),
            .groups = "drop")
und2
und2 %>% nest(Plot_Number)
und2 %>% nest(Nativity, Code, Scientific_Name, Life_Form, Avg_Cover)
test <- und2 %>% nest(Nativity, Code, Scientific_Name, Life_Form, Avg_Cover)
head(test$data)
names(test$data) <- test$Plot_Number
head(test$data)
test2 <- lapply(test$data, as.sunburstDF, value_column = Avg_Cover)
test2 <- lapply(test$data, as.sunburstDF, value_column = "Avg_Cover")
head(test2)

#https://stackoverflow.com/questions/40024029/plotly-updating-data-with-dropdown-selection

require(plotly)
df <- data.frame(x = runif(200), y = runif(200), z = runif(200))
p <- plot_ly(df, x = ~x, y = ~y, mode = "markers", name = "A", visible = T) %>%
  layout(
    title = "Drop down menus - Styling",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        y = 0.7,
        buttons = list(
          list(method = "restyle",
               args = list("y", list(df$y)),  # put it in a list
               label = "Show A"),
          list(method = "restyle",
               args = list("y", list(df$z)),  # put it in a list
               label = "Show B")))
    ))
p

#--------------


library(crosstalk)
library(plotly)

data(txhousing, package = "ggplot2")

tx <- highlight_key(txhousing, ~city)

# initiate a plotly object
base <- plot_ly(tx, color = I("black")) %>%
  dplyr::group_by(city)

time_series <- base %>%
  #group_by(city) %>%
  add_lines(x = ~date, y = ~median)

highlight(
  time_series,
  on = "plotly_click",
  selectize = TRUE,
  dynamic = TRUE,
  persistent = FALSE
)

dot_plot <- base %>%
  summarise(miss = sum(is.na(median))) %>%
  filter(miss > 0) %>%
  add_markers(
    x = ~miss,
    y = ~forcats::fct_reorder(city, miss),
    hoverinfo = "x+y"
  ) %>%
  layout(
    xaxis = list(title = "Number of months missing"),
    yaxis = list(title = "")
  )

subplot(dot_plot, time_series, widths = c(.2, .8), titleX = TRUE) %>%
  layout(showlegend = FALSE) %>%
  highlight(on = "plotly_selected", dynamic = TRUE, selectize = TRUE)

hist <- add_histogram(
  base,
  x = ~median,
  histnorm = "probability density"
)
subplot(time_series, hist, nrows = 2) %>%
  layout(barmode = "overlay", showlegend = FALSE) %>%
  highlight(
    dynamic = TRUE,
    selectize = TRUE,
    selected = attrs_selected(opacity = 0.3)
  )



tx <- highlight_key(txhousing)
gg <- ggplot2::ggplot(tx) + geom_line(aes(date, median, group = city))
filter <- bscols(
  filter_select("id", "Select a city", tx, ~city),
  ggplotly(gg, dynamicTicks = TRUE),
  widths = c(12, 12)
)
tx2 <- highlight_key(txhousing, ~city, "Select a city")
gg <- ggplot2::ggplot(tx2) + geom_line(aes(date, median, group = city))
select <- highlight(
  ggplotly(gg, tooltip = "city"),
  selectize = TRUE, persistent = TRUE
)
bscols(filter, select)


#library(crosstalk)
tx <- highlight_key(txhousing)
widgets <- bscols(
  widths = c(12, 12, 12),
  filter_select("city", "Cities", tx, ~city),
  filter_slider("sales", "Sales", tx, ~sales),
  filter_checkbox("year", "Years", tx, ~year, inline = TRUE)
)
bscols(
  widths = c(4, 8), widgets,
  plot_ly(tx, x = ~date, y = ~median, showlegend = FALSE) %>%
    add_lines(color = ~city, colors = "black")
)


library(leaflet)
eqs <- highlight_key(quakes)
stations <- filter_slider(
  "station", "Number of Stations",
  eqs, ~stations
)

p <- plot_ly(eqs, x = ~depth, y = ~mag) %>%
  add_markers(alpha = 0.5) %>%
  highlight("plotly_selected")

map <- leaflet(eqs) %>%
  addTiles() %>%
  addCircles()

bscols(
  widths = c(6, 6, 3),
  p, map, stations
)

gg <- ggplot2::ggplot(tx) + geom_line(aes(date, median, group = city))
filter <- bscols(
  filter_select("id", "Select a city", tx, ~city),
  ggplotly(gg, dynamicTicks = TRUE),
  widths = c(12, 12)
)


momdad <- highlight()
fig <- plot_ly(
  labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
  parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve"),
  values = c(10, 14, 12, 10, 2, 6, 6, 4, 4),
  type = 'sunburst'
)

fig
