MapCoverTotal3chg <- function(map_dataset, crosstalk = FALSE, crosstalk_group = "cover", combine_strata = TRUE, cycle, park, sample_frame, community, year, plot_type, is_qa_plot = FALSE, certified, verified, silent = TRUE) {

  #if (missing(fixed_plot_cycle)) {
  #  stop("cycle is required")
  #}
  #if (length(fixed_plot_cycle) > 1) {
  #  stop("cycle must be length 1")
  #}

  # Get Max Cycle/Year from text
  getmax <- function(col) str_extract_all(col,"[0-9\\.-]+") %>%
    lapply(.,function(x) max(as.numeric(x), na.rm = T) ) %>%
    unlist()

  pts <- PlotAndTransectLocations(protocol = "FTPC",
                                  crosstalk = FALSE,
                                  crosstalk_group = crosstalk_group,
                                  park = park,
                                  sample_frame = sample_frame,
                                  cycle = cycle,
                                  plot_type,
                                  is_qa_plot = is_qa_plot,
                                  certified = certified,
                                  verified = verified) %>%
    dplyr::mutate(Cycle = as.character(Cycle),
                  Year = as.character(Year))
  #   #              Year_Text = Year)
  #   if (cycle) {
  #     pts <- pts %>%
  #
  #       dplyr::mutate(Cycle = getmax(Cycle),
  #                     Year = getmax(Year),
  #                     Sample_Unit_Number = as.integer(Sample_Unit_Number))
  #
  #   } else {
  #     pts <- pts
  #   }


  pts <- pts %>%
    dplyr::mutate(Cycle = Cycle, #getmax(Cycle) #removed getmax to try to have each plot filter
                  Year = Year, #getmax(Year)
                  Sample_Unit_Number = as.integer(Sample_Unit_Number)) %>%
    dplyr::rename(Plot_Type = Sample_Unit_Type,
                  Plot_Number = Sample_Unit_Number) %>%
    dplyr::select(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle, Lat, Long) #Year_Text, Cycle_Text,
  cover_data <- map_dataset

  #remove older fixed plot data? May want to remove this if user is filtering by Year
  #cover_data <- cover_data %>%
  #  dplyr::group_by(Sampling_Frame, Plot_Number) %>%
  #  dplyr::slice_max(Cycle)

  #Make 'Year' character for join:
  cover_data <- cover_data %>%
    dplyr::mutate(Cycle = as.character(Cycle),
                  Year = as.character(Year))

  # Combine cover and location data
  cover_data <- dplyr::left_join(cover_data, pts, by = c("Unit_Code", "Sampling_Frame", "Plot_Type", "Plot_Number", "Year", "Cycle")) %>%
    dplyr::mutate(stat = !!sym(param)) %>%
    dplyr::arrange(stat)


  # pal <- grDevices::colorRampPalette(c("red", "orange", "yellow", "green"))(length(cover_data$nat_ratio))
  #ramp <- grDevices::colorRamp(c("#d11141", "#f37735", "#C8E52A", "#00b159")) #better colors for red, orange, yellow, green
  #ramp <- grDevices::colorRamp(c("red", "orange", "yellow", "green"))
  #pal <- leaflet::colorNumeric(ramp, domain = c(0,100))

  # dplyr::mutate(color = dplyr::case_when(NonNative_Cover_Total_pct <= 0 & Native_Cover_Total_pct <= 0 ~ "#cccccc",#gray
  #                                        NonNative_Cover_Total_pct > 0 & Native_Cover_Total_pct <= 0 ~ "#d11141",#red
  #                                        NonNative_Cover_Total_pct > 0 & Native_Cover_Total_pct > 0 & NonNative_Cover_Total_pct > Native_Cover_Total_pct ~ "#f37735",#orange
  #                                        NonNative_Cover_Total_pct > 0 & Native_Cover_Total_pct > 0 & NonNative_Cover_Total_pct < Native_Cover_Total_pct ~ "#C8E52A",#yellow
  #                                        NonNative_Cover_Total_pct <= 0 & Native_Cover_Total_pct > 0 ~ "#00b159"))#green

  #cover_data <- cover_data %>%
  #  dplyr::mutate(color = pal(stat))


  # Enable crosstalk if specified
  if (crosstalk) {
    cover_data <- dplyr::mutate(cover_data, key = paste0(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle))
    cover <- crosstalk::SharedData$new(cover_data, group = crosstalk_group, key = ~key)
  }

  # Set up icons
  custom_icons <- pacnvegetation:::pchIcons(pch = rep(22, nrow(cover_data)),
                                            width = 30,
                                            height = 30,
                                            bg = colorspace::darken(cover_data$new_cols),
                                            col = cover_data$new_cols, 0.3)
  iconwidth <- 25
  iconheight <- 25

  # Make NPS map Attribution
  NPSAttrib <-
    htmltools::HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )

  # NPS park tiles URLs
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

  map <- leaflet::leaflet(cover) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
    leaflet::addMarkers(lng = ~cover_data$Long,
                        lat = ~cover_data$Lat,
                        icon = ~leaflet::icons(iconUrl = custom_icons,
                                               iconWidth = iconwidth,
                                               iconHeight = iconheight),
                        label = ~cover_data$Plot_Number,
                        #layerId = ~cover_data$key,
                        labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold")),
                        popup = ~paste0("<br><strong>Change in Clidemia cover 2013-18:</strong> ", round(cover_data$Chg_Prior, digits = 1), "%",
                                        "<br><strong>Clidemia cover in 2018:</strong> ", round(cover_data$Cover, digits = 1), "%",
                                        "<br><strong>Sampling Frame:</strong> ", cover_data$Sampling_Frame,
                                        "<br><strong>Cycle:</strong> ", cover_data$Cycle,
                                        "<br><strong>Year:</strong> ", cover_data$Year))

  map %<>% leaflet::addScaleBar(position = "bottomleft")

  return(map)
}
