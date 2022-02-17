und <- FilterPACNVeg("Understory", sample_frame = params$sample_frame, cycle = max(cycles))  # Only get data from most recent cycle

und <- und %>%
  mutate(Life_Form=replace(Life_Form, Code=="SOPCHR", "Shrub"))

# prep data for sunburst plot
nativity_colors <- c("Native" = "#1b9e77", "No Veg" = "grey", "Non-Native" = "#d95f02", "Unknown" = "#7570b3")

und2 <- UnderCombineStrata(und) %>%
  mutate(across(everything(), replace_na, "No Veg")) %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number,
           Nativity, Code, Scientific_Name, Life_Form) %>%
  summarize(Hits_Sp = n(), .groups = "drop") %>%
  complete(nesting(Cycle, Unit_Code, Sampling_Frame, Plot_Number),
           nesting(Nativity, Code, Scientific_Name, Life_Form),
           fill = list(Hits_Sp = 0)) %>%
  mutate(Plot_Percent = Hits_Sp/300) %>%
  group_by(Cycle, Unit_Code, Sampling_Frame,
           Nativity, Code, Scientific_Name, Life_Form) %>%
  summarize(n = n(),
            plots_present = sum(Hits_Sp > 0),
            Avg_Cover = round(mean(Plot_Percent), 3),
            #Median = median(Plot_Percent),
            Std_Dev = round(sd(Plot_Percent), 3),
            .groups = "drop")

# Create sunburst plot
sb <- select(und2, Nativity, Life_Form, Code, Avg_Cover)
sb <- as.sunburstDF(sb, value_column = "Avg_Cover")

plot_ly(sb, ids = ~ids, labels = ~labels, parents = ~parents, values = ~values, type = 'sunburst', branchvalues = 'total') %>%
  plotly::layout(colorway = nativity_colors)

und_nest <- UnderCombineStrata(und) %>%
  mutate(across(everything(), replace_na, "No Veg")) %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number,
           Nativity, Code, Scientific_Name, Life_Form) %>%
  summarize(Hits_Sp = n(), .groups = "drop") %>%
  complete(nesting(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number),
           nesting(Nativity, Code, Scientific_Name, Life_Form),
           fill = list(Hits_Sp = 0)) %>%
  mutate(Plot_Percent = Hits_Sp/300) #%>%

und_nest2 <- und_nest %>%
  nest(data = c(Nativity, Code, Scientific_Name, Life_Form, Hits_Sp, Plot_Percent)) %>%
  mutate(data = map2(data, Plot_Number, ~mutate(.x, PlotGroup = .y)))

#names(und_nest2$data) <- unique(und_nest$Plot_Number)

l.1 <- purrr::map_dfr(und_nest2$data, bind_rows)

l.2 <- und_nest2 %>%
  for unique(und_nest2$Plot_Type) in



for row in

und_nest2 <- und_nest %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Type, Nativity, Code, Scientific_Name, Life_Form) %>%
  summarize(n = n(),
            plots_present = sum(Hits_Sp > 0),
            Avg_Cover = round(mean(Plot_Percent), 3),
            #Median = median(Plot_Percent),
            Std_Dev = round(sd(Plot_Percent), 3),
            .groups = "drop")

und2 %>% nest(Plot_Number)
und2 %>% nest(Nativity, Code, Scientific_Name, Life_Form, Avg_Cover)
test <- und2 %>% nest(Nativity, Code, Scientific_Name, Life_Form, Avg_Cover)
head(test$data)
names(test$data) <- test$Plot_Number
head(test$data)
test2 <- lapply(test$data, as.sunburstDF, value_column = Avg_Cover)
test2 <- lapply(test$data, as.sunburstDF, value_column = "Avg_Cover")
head(test2)









# ----- Idea to filter data in plotly from Sarah:
und2 <- UnderCombineStrata(und) %>%
  mutate(across(everything(), replace_na, "No Veg")) %>%
  group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number,
           Nativity, Code, Scientific_Name, Life_Form) %>%
  summarize(Hits_Sp = n(), .groups = "drop") %>%
  complete(nesting(Cycle, Unit_Code, Sampling_Frame, Plot_Number),
           nesting(Nativity, Code, Scientific_Name, Life_Form),
           fill = list(Hits_Sp = 0)) %>%
  mutate(Plot_Percent = Hits_Sp/300) %>%
  group_by(Cycle, Unit_Code, Sampling_Frame,
           Nativity, Code, Scientific_Name, Life_Form, Plot_Number) %>%
  summarize(n = n(),
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
  group_by(city)

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
gg <- ggplot(tx) + geom_line(aes(date, median, group = city))
filter <- bscols(
  filter_select("id", "Select a city", tx, ~city),
  ggplotly(gg, dynamicTicks = TRUE),
  widths = c(12, 12)
)
tx2 <- highlight_key(txhousing, ~city, "Select a city")
gg <- ggplot(tx2) + geom_line(aes(date, median, group = city))
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

gg <- ggplot(tx) + geom_line(aes(date, median, group = city))
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
