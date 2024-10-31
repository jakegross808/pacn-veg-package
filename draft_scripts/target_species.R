# target species list & graph---------------------------------------------------
library(pacnvegetation)
library(tidyverse)
library(leaflet)


#--- 1. Read latest cache ------------------------------------------------------

#** Download latest FTPC and EIPS data first!*

# Write/Read csv from pacnvegetation package:
pacnveg_cache_path <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/R_WritePACNVeg"

# Read
path_file_info <- file.info(list.files(pacnveg_cache_path, full.names = T))
latest_folder <- rownames(path_file_info)[which.max(path_file_info$mtime)]

LoadPACNVeg(data_path = latest_folder,
            data_source = "file")

names(FilterPACNVeg())

#--- 2. variable specification -------------------------------------------------

#var_sframe <- "Nahuku/East Rift"
#var_sframe <- "Olaa"
var_sframe <- NA
var_plots <- NA

var_park <- "HAVO"
target_spp_list <- c("Clidemia hirta")
target_spp_list <- c("Sphaeropteris cooperi")
target_spp_list <- c("Psidium cattleianum")
target_spp_list <- c("Hypolepis dicksonioides")
target_spp_list <- c("Asparagus spp.")
target_spp_list <- c("Juncus effusus")

## nahuku plots ----------------------------------------------------------------
# auto removal of east rift plots if Nahuku/East Rift in dataset

nahuku_plots <- c(1, 4, 10, 12, 13, 14, 15, #fixed
                  46, 49, 51, 52, 54, 55, 56, 58, #2021 rotational
                  24, 26, #2010 rotational
                  31, 32, 33, 34, 35, 38, 41, 45) #2015 rotational


#--- 3. detection % graph-------------------------------------------------------

all_pres <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                                          sample_frame = var_sframe,
                                          plot_number = var_plots,
                                          park = var_park)
## remove plots if needed ......................................................
all_pres_subsection <- all_pres |>
  filter(Sampling_Frame != "Nahuku/East Rift" | Plot_Number %in% nahuku_plots)

look <- all_pres_subsection |>
  dplyr::distinct(Sampling_Frame, Plot_Number)

#...............................................................................

plt_xy <- FilterPACNVeg(data_name = "Events_extra_xy",
                        sample_frame = var_sframe,
                        plot_number = var_plots,
                        park = var_park) |>
  mutate(QA_Plot = as.logical(QA_Plot))

target_sp_locations <- all_pres_subsection |>
  filter(Scientific_Name %in% target_spp_list) |>
  dplyr::left_join(plt_xy)

all_plots <- all_pres_subsection |>
  select(Sampling_Frame, Cycle, Plot_Number, QA_Plot) |>
  filter(QA_Plot == FALSE) |>
  dplyr::distinct() |>
  dplyr::group_by(Sampling_Frame, Cycle) |>
  summarise(n_plots = n())

target_sp_detect_graph <- target_sp_locations |>
  dplyr::group_by(Sampling_Frame, Cycle, Scientific_Name, Nativity, Outside_Plot) |>
  dplyr::summarize(plots_detected = n()) |>
  dplyr::left_join(all_plots) |>
  mutate(proportion = plots_detected/n_plots)

sample_size <- all_plots %>%
  dplyr::select(Sampling_Frame, Cycle, n_plots) %>%
  dplyr::mutate(Sampling_Frame = dplyr::case_when(Sampling_Frame == "Nahuku/East Rift" ~ "Nahuku",
                                                  TRUE ~ Sampling_Frame)) |>
  dplyr::distinct() %>%
  dplyr::group_by(Sampling_Frame) %>%
  dplyr::mutate(count_cycles = match(Cycle, unique(Cycle))) %>%
  dplyr::mutate(SF = paste0("; ", Sampling_Frame, ": ")) %>%
  dplyr::mutate(SF = dplyr::case_when(count_cycles == 1 ~ SF,
                                      TRUE ~ "")) %>%
  dplyr::mutate(Text = paste0(SF, Cycle, " [n = ", n_plots, "]")) %>%
  dplyr::pull(Text) %>%
  paste(collapse = ", ") %>%
  stringr::str_sub(3) %>%
  stringr::str_replace_all(", ;", ";")
sample_size

# Nativity discrete scale Colors:
nativity_colors <- c("Native" = "#1b9e77",
                     "No_Veg" = "grey",
                     "Non-Native" = "#d95f02",
                     "Unknown" = "#7570b3")

target_sp_detect_graph |>
  dplyr::mutate(Sampling_Frame = dplyr::case_when(Sampling_Frame == "Nahuku/East Rift" ~ "Nahuku",
                                                  TRUE ~ Sampling_Frame)) |>
  mutate(Cycle = as.numeric(Cycle)) |>
  ggplot2::ggplot(aes(x=Cycle, y=proportion, fill = Nativity, alpha = Outside_Plot)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(Sampling_Frame ~ Scientific_Name) +
  ggplot2::ylab("plot presence : total plots") +
  ggplot2::labs(caption = sample_size) +
  ggplot2::scale_fill_manual(values = nativity_colors) +
  scale_alpha_manual(name = "",
                     values = c(0.6),
                     labels = c("outside plot", "inside plot"))
# crosstalk example-------------------------------------------------------------

library(crosstalk)
data <- data.frame(id = c(1,2,3,4,5),
                   lat= c(50.9, 50.8, 50.5, 50.5, 51),
                   lon = c(-0.7, -0.92, -1, -0.8, -0.9),
                   date = c("2020-06-01", "2020-05-07", "2020-03-24", "2020-04-01", "2020-05-26"))

data <- data %>% dplyr::mutate(date2 = as.numeric(as.Date(date)),
                               date3 = as.Date(date)
)

shared_data <- SharedData$new(data)

test_slider <- filter_slider("date", "Date", shared_data, ~date3, width = "100%")
test_map <- leaflet(shared_data, width = "100%", height = 500) %>%
  leaflet::addTiles() %>%
  leaflet::addMarkers()

bscols(widths = 12, test_slider, test_map)

#--- 3. detection map-----------------------------------------------------------

all_pres <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                                          sample_frame = var_sframe,
                                          plot_number = var_plots,
                                          park = var_park)

plt_xy <- FilterPACNVeg(data_name = "Events_extra_xy",
                        sample_frame = var_sframe,
                        plot_number = var_plots,
                        park = var_park) |>
  mutate(QA_Plot = as.logical(QA_Plot))

target_sp_locations <- all_pres |>
  filter(Scientific_Name %in% target_spp_list) |>
  dplyr::left_join(plt_xy) |>
  mutate(event_year = lubridate::year(lubridate::ymd(Year, truncated = 2L)))

target_sp_locations$event_year
str(target_sp_locations$event_year)

# Crosstalk slider
shared_data <- SharedData$new(target_sp_locations)
year_slider <- filter_slider(id = "event_year", label = "Year", shared_data, ~event_year, width = "100%", sep = "")

# Color scheme
library(viridis) # My favorite palette for maps
target_sp_locations$Scientific_Name
col_length <- length(unique(target_sp_locations$Scientific_Name))
spp_colors <- colorFactor(rainbow(col_length), target_sp_locations$Scientific_Name)

# Map
target_sp_detect_map <- leaflet(shared_data) %>%
  addProviderTiles(providers$USGS.USTopo) %>%
  #addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addCircleMarkers(data = shared_data,
                   lng = ~Center_Long,
                   lat = ~Center_Lat,
                   color = ~"black", weight = 1,
                   stroke = TRUE,fill = TRUE,
                   radius = 6,
                   #label = ~Cycle,
                   fillColor = ~spp_colors(Scientific_Name),
                   fillOpacity = 0.7,
                   labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE),
                   popup = ~paste(Scientific_Name,"<br/>",
                                  "<strong> Plot: </strong>", Plot_Number, "<br/>",
                                  "<strong> Year: </strong>", Year),
                   group = ~Scientific_Name) %>%
  #addLegend(pal = spp_colors,
  #          values = ~Scientific_Name,
  #          title = "Target Species") %>%
  addLayersControl(overlayGroups = ~Scientific_Name,
                   options = layersControlOptions(collapsed = FALSE),
                   position = 'bottomleft')

bscols(widths = 12, year_slider, target_sp_detect_map)




#--- 4. add sp_plot_map_graph_function.R stuff----------------------------------

#sp_plot_map_graph_function.R is last code used to create the graph+map of clidemia cover
# per plot for NPSA. Need to polish that code, create function, and add here.


# also clidemia.R and clidemia_chg.R appear to be the functions needed?
