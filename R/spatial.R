#' Get locations of FTPC plots and EIPS transects for mapping.
#'
#' @param protocol Character vector indicating whether to include FTPC plots, EIPS transects, or both
#' @param crosstalk If `TRUE`, returns a SharedData object for use with the `crosstalk` package. Otherwise, returns a normal tibble.
#' @param crosstalk_group If `crosstalk == TRUE`, the crosstalk group name to use. Ignored if `crosstalk == FALSE`.
#' @inheritParams FilterPACNVeg
#'
#' @return A tibble with one row per location and columns Protocol, Unit_Code, Sampling_Frame, Sample_Unit_Number, Lat, Long, Cycles, Years
#' @export
#'
#' @examples
#' \dontrun{
#' all_locs <- PlotAndTransectLocations()
#' eips_transects <- PlotAndTransectLocations("EIPS")
#' }
PlotAndTransectLocations <- function(protocol = c("FTPC", "EIPS"), crosstalk = FALSE, crosstalk_group = "map", park, sample_frame, cycle, plot_type, is_qa_plot, transect_type, certified, verified) {
  if (!all(toupper(protocol) %in% c("FTPC", "EIPS"))) {
    stop("Invalid protocol selection. Protocol must be 'FTPC', 'EIPS', or c('FTPC', 'EIPS')")
  }

  ftpc_pts <- FilterPACNVeg(data_name = "Events_extra_xy", park = park, sample_frame = sample_frame, cycle = cycle, plot_type = plot_type, is_qa_plot = is_qa_plot, certified = certified, verified = verified) %>%
    dplyr::mutate(Protocol = "FTPC", Sample_Unit = "Plot") %>%
    dplyr::rename(Sample_Unit_Number = Plot_Number, Sample_Unit_Type = Plot_Type, Lat = Start_Lat, Long = Start_Long) %>%
    dplyr::select(Protocol, Unit_Code, Sampling_Frame, Sample_Unit, Sample_Unit_Type, Sample_Unit_Number, Lat, Long, Year, Cycle)

  eips_pts <- FilterPACNVeg(data_name = "Events_extra_xy_EIPS", park = park, sample_frame = sample_frame, cycle = cycle, transect_type = transect_type, certified = certified, verified = verified) %>%
    dplyr::mutate(Protocol = "EIPS", Sample_Unit = "Transect") %>%
    dplyr::rename(Sample_Unit_Number = Transect_Number, Sample_Unit_Type = Transect_Type) %>%
    dplyr::select(Protocol, Unit_Code, Sampling_Frame, Sample_Unit, Sample_Unit_Type, Sample_Unit_Number, Lat, Long, Year, Cycle)

  eips_tsects <- FilterPACNVeg(data_name = "EIPS_image_pts", park = park, sample_frame = sample_frame, cycle = cycle, transect_type = transect_type, certified = certified, verified = verified) %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Transect_Type, Transect_Number) %>%
    #only take coordinates for last cycle
    dplyr::filter(Cycle == max(Cycle), Year == max(Year),
                  !is.na(Latitude), !is.na(Longitude)) %>%
    dplyr::ungroup()
  # execute the following line separately so we can suppress the warning that as.numeric is generating NA values
  suppressWarnings(eips_tsects <- dplyr::mutate(eips_tsects, Image_Point = as.numeric(Image_Point), Protocol = "EIPS", Sample_Unit = "Transect"))
  eips_tsects <- eips_tsects %>%
    dplyr::rename(Sample_Unit_Number = Transect_Number, Sample_Unit_Type = Transect_Type) %>%
    dplyr::arrange(Unit_Code, Community, Sampling_Frame, Year, Cycle, Sample_Unit_Type, Sample_Unit_Number, Image_Point) %>%
    tidyr::nest(Transect_Line = c(Image_Point, Latitude, Latitude_Dir, Longitude, Longitude_Dir, GCS, GPS_Error)) %>%
    dplyr::mutate(Transect_Line = purrr::map(Transect_Line, function(df){
      df <- dplyr::filter(df, !is.na(Longitude) & !is.na(Latitude))
      # Convert to SpatialPointsDataFrame
      sp::coordinates(df) <- c("Longitude", "Latitude")
      sp::Line(df)}
    ))

  eips_pts <- eips_pts %>%
    dplyr::group_by(Protocol, Unit_Code, Sampling_Frame, Sample_Unit, Sample_Unit_Type, Sample_Unit_Number, Lat, Long) %>%
    dplyr::arrange(Protocol, Unit_Code, Sampling_Frame, Sample_Unit_Type, Sample_Unit_Number, Year, Cycle) %>%
    dplyr::mutate(Tsect_Line_Cycle = max(Cycle),
                     Tsect_Line_Year = max(Year)) %>%
                     #Cycle_Text = paste(Cycle, collapse = ", "),
                     #Year_Text = paste(Year, collapse = ", ")#,
                     #) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(eips_tsects, by = c("Protocol", "Unit_Code", "Sampling_Frame", "Sample_Unit", "Sample_Unit_Type", "Sample_Unit_Number", "Tsect_Line_Cycle" = "Cycle", "Tsect_Line_Year" = "Year")) %>%
    dplyr::select(-Community)

  ftpc_pts <- ftpc_pts %>%
    dplyr::group_by(Protocol, Unit_Code, Sampling_Frame, Sample_Unit, Sample_Unit_Type, Sample_Unit_Number, Lat, Long) %>%
    dplyr::arrange(Protocol, Unit_Code, Sampling_Frame, Sample_Unit_Type, Sample_Unit_Number, Year, Cycle) %>%
    #dplyr::summarise(Cycle = paste(Cycle, collapse = ", "),
    #                 Year = paste(Year, collapse = ", ")) %>%
    dplyr::mutate(Tsect_Line_Cycle = NA, Tsect_Line_Year = NA, Transect_Line = NA) %>%
    dplyr::ungroup()

  all_pts <- rbind(ftpc_pts, eips_pts) %>%
    dplyr::filter(Protocol %in% toupper(protocol))

  if (crosstalk) {
    all_pts <- crosstalk::SharedData$new(all_pts, group = crosstalk_group)
  }

  return(all_pts)
}


#' Map PACN vegetation plots and transects
#'
#' @inheritParams PlotAndTransectLocations
#'
#' @return A leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' MapPACNVeg(protocol = "FTPC")
#' MapPACNVeg(park = "AMME")
#' }
MapPACNVeg <- function(protocol = c("FTPC", "EIPS"), crosstalk = FALSE, crosstalk_group = "map", park, sample_frame, cycle, plot_type, is_qa_plot, transect_type, certified, verified) {
  pts <- PlotAndTransectLocations(protocol = protocol, crosstalk = crosstalk, crosstalk_group = crosstalk_group, park = park, sample_frame = sample_frame, cycle = cycle, plot_type = plot_type, is_qa_plot = is_qa_plot, transect_type = transect_type, certified = certified, verified = verified)

  # If pts is a crosstalk object, extract just the data for functions that need a regular tibble/dataframe
  if (crosstalk) {
    pts_data <- pts$data()
  } else {
    pts_data <- pts
  }

  if ("EIPS" %in% protocol) {
    tsect_lines_df <- pts_data %>%
      dplyr::filter(!is.na(Transect_Line), !purrr::map_lgl(Transect_Line, is.null)) %>%
      tibble::rowid_to_column(var = "id")
    tsect_lines <- apply(tsect_lines_df, 1, function(df) {return(sp::Lines(df$Transect_Line, df$id))})
    tsect_lines <- sp::SpatialLines(tsect_lines)
    tsect_lines <- sp::SpatialLinesDataFrame(tsect_lines, tsect_lines_df, match.ID = TRUE)
  }

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

  # Set up custom icons
  icon_filename <- paste0(pts_data$Protocol, "_", pts_data$Sample_Unit_Type, ".png")
  customIcons <- leaflet::icons(iconUrl = here::here("inst", "rmarkdown", icon_filename),
    iconWidth = 20, iconHeight = 20,
    iconAnchorX = 10, iconAnchorY = 10
  )

  # Set up group labels for layers control
  grps <- paste(protocol, "points")
  if ("EIPS" %in% protocol) {
    grps <- c(grps, "EIPS transects")
  }

  map <- leaflet::leaflet(pts) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = grps,
                              options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
    leaflet.esri::addEsriFeatureLayer(url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0",
                                      useServiceSymbology = TRUE,
                                      labelProperty = "Sampling_Frame") %>%
    leaflet::addMarkers(lng = ~Long,
                               lat = ~Lat,
                               icon = customIcons,
                        group = ~paste(Protocol, "points"),
                        label = ~Sample_Unit_Number,
                        labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold")),
                        popup = ~paste0("<strong>", Protocol, " ", Sample_Unit, ":</strong> ", Sample_Unit_Number,
                                        "<br><strong>", Sample_Unit, " Type:</strong> ", Sample_Unit_Type,
                                        "<br><strong>Sampling Frame:</strong> ", Sampling_Frame,
                                        "<br><strong>Cycle:</strong> ", Cycle,
                                        "<br><strong>Year:</strong> ", Year))

  # Add EIPS transect lines
  if ("EIPS" %in% protocol) {
    map <- leaflet::addPolylines(map, data = tsect_lines,
                          group = "EIPS transects", color = "#c56c39", opacity = 0.8)
  }

  map %<>% leaflet::addScaleBar(position = "bottomleft")

  return(map)
}

#' Map change in vegetation cover
#'
#' @inheritParams PlotAndTransectLocations
#'
#' @return A leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' MapPACNVeg(protocol = "FTPC")
#' MapPACNVeg(park = "AMME")
#' }
MapCoverChange <- function(crosstalk = FALSE, crosstalk_group = "cover", combine_strata = TRUE, park, sample_frame, community, year, cycle = 2, plot_type, paired_cycle = 1, is_qa_plot, certified, verified, silent = TRUE) {

  if (missing(cycle)) {
    stop("cycle is required")
  }
  if (length(cycle) > 1) {
    stop("cycle must be length 1")
  }

  pts <- PlotAndTransectLocations(protocol = "FTPC", crosstalk = FALSE, crosstalk_group = crosstalk_group, park = park, sample_frame = sample_frame, cycle = cycle, plot_type = "fixed", is_qa_plot = is_qa_plot, certified = certified, verified = verified) %>%
    dplyr::mutate(Cycle = as.integer(Cycle),
                  Year = as.integer(Year),
                  Sample_Unit_Number = as.integer(Sample_Unit_Number)) %>%
    dplyr::rename(Plot_Type = Sample_Unit_Type,
                  Plot_Number = Sample_Unit_Number) %>%
    dplyr::select(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle, Lat, Long)
  cover_data <- UnderNativityCover(combine_strata = combine_strata, paired_change = TRUE, crosstalk = FALSE, park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle,
                              plot_type = plot_type, paired_cycle = paired_cycle, silent = silent)

  # Combine cover and location data
  cover_data <- dplyr::left_join(cover_data, pts, by = c("Unit_Code", "Sampling_Frame", "Plot_Type", "Plot_Number", "Year", "Cycle")) %>%
    dplyr::mutate(color = dplyr::case_when(NonNative_Cover_Change_pct <= 0 & Native_Cover_Change_pct <= 0 ~ "#cccccc",#gray
                                           NonNative_Cover_Change_pct > 0 & Native_Cover_Change_pct <= 0 ~ "#d11141",#red
                                           NonNative_Cover_Change_pct > 0 & Native_Cover_Change_pct > 0 & NonNative_Cover_Change_pct > Native_Cover_Change_pct ~ "#f37735",#orange
                                           NonNative_Cover_Change_pct > 0 & Native_Cover_Change_pct > 0 & NonNative_Cover_Change_pct < Native_Cover_Change_pct ~ "#C8E52A",#yellow
                                           NonNative_Cover_Change_pct <= 0 & Native_Cover_Change_pct > 0 ~ "#00b159"))#green

  # Enable crosstalk if specified
  if (crosstalk) {
    cover_data <- dplyr::mutate(cover_data, key = paste0(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle))
    cover <- crosstalk::SharedData$new(cover_data, group = crosstalk_group, key = ~key)
  }

  # Set up icons
  custom_icons <- pchIcons(pch = rep(22, nrow(cover_data)),
                           width = 30,
                           height = 30,
                           bg = colorspace::darken(cover_data$color),
                           col = cover_data$color, 0.3)
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
                        # layerId = ~cover_data$key,
                        labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold")),
                        popup = ~paste0("<br><strong>Non-native cover change:</strong> ", cover_data$NonNative_Cover_Change_pct,
                                        "<br><strong>Native cover change:</strong> ", cover_data$Native_Cover_Change_pct,
                                        "<br><strong>Sampling Frame:</strong> ", cover_data$Sampling_Frame,
                                        "<br><strong>Cycle:</strong> ", cover_data$Cycle,
                                        "<br><strong>Year:</strong> ", cover_data$Year))

  map %<>% leaflet::addScaleBar(position = "bottomleft")

  return(map)
}

#' Map Total Native vs. Non-native vegetation cover
#'
#' @inheritParams PlotAndTransectLocations
#'
#' @return A leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' MapCoverTotal(protocol = "FTPC") #currently only FTPC mapping supported
#' MapCoverTotal(park = "AMME")
#' MapCoverTotal(sample_frame = "Puu Alii")
#' }
MapCoverTotal <- function(crosstalk = FALSE, crosstalk_group = "cover", combine_strata = TRUE, park, sample_frame, community, year, cycle = max(cycles), plot_type, is_qa_plot, certified, verified, silent = TRUE) {

  if (missing(cycle)) {
    stop("cycle is required")
  }
  if (length(cycle) > 1) {
    stop("cycle must be length 1")
  }

  pts <- PlotAndTransectLocations(protocol = "FTPC", crosstalk = FALSE, crosstalk_group = crosstalk_group, park = park, sample_frame = sample_frame, cycle = cycle, plot_type, is_qa_plot = is_qa_plot, certified = certified, verified = verified) %>%
    dplyr::mutate(Cycle = as.integer(Cycle),
                  Year = as.integer(Year),
                  Sample_Unit_Number = as.integer(Sample_Unit_Number)) %>%
    dplyr::rename(Plot_Type = Sample_Unit_Type,
                  Plot_Number = Sample_Unit_Number) %>%
    dplyr::select(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle, Lat, Long)
  cover_data <- UnderNativityCover(combine_strata = combine_strata, paired_change = FALSE, crosstalk = FALSE, park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle,
                                   plot_type = plot_type, silent = silent)

  # Combine cover and location data
  cover_data <- dplyr::left_join(cover_data, pts, by = c("Unit_Code", "Sampling_Frame", "Plot_Type", "Plot_Number", "Year", "Cycle")) %>%
    dplyr::mutate(nat_ratio = Native_Cover_Total_pct / (NonNative_Cover_Total_pct + Native_Cover_Total_pct) * 100) %>%
    dplyr::arrange(nat_ratio)


  # pal <- grDevices::colorRampPalette(c("red", "orange", "yellow", "green"))(length(cover_data$nat_ratio))
  ramp <- grDevices::colorRamp(c("#d11141", "#f37735", "#C8E52A", "#00b159")) #better colors for red, orange, yellow, green
  #ramp <- grDevices::colorRamp(c("red", "orange", "yellow", "green"))
  pal <- leaflet::colorNumeric(ramp, domain = c(0,100))

  # dplyr::mutate(color = dplyr::case_when(NonNative_Cover_Total_pct <= 0 & Native_Cover_Total_pct <= 0 ~ "#cccccc",#gray
  #                                        NonNative_Cover_Total_pct > 0 & Native_Cover_Total_pct <= 0 ~ "#d11141",#red
  #                                        NonNative_Cover_Total_pct > 0 & Native_Cover_Total_pct > 0 & NonNative_Cover_Total_pct > Native_Cover_Total_pct ~ "#f37735",#orange
  #                                        NonNative_Cover_Total_pct > 0 & Native_Cover_Total_pct > 0 & NonNative_Cover_Total_pct < Native_Cover_Total_pct ~ "#C8E52A",#yellow
  #                                        NonNative_Cover_Total_pct <= 0 & Native_Cover_Total_pct > 0 ~ "#00b159"))#green

  cover_data <- cover_data %>%
    dplyr::mutate(color = pal(nat_ratio))


  # Enable crosstalk if specified
  if (crosstalk) {
    cover_data <- dplyr::mutate(cover_data, key = paste0(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle))
    cover <- crosstalk::SharedData$new(cover_data, group = crosstalk_group, key = ~key)
  }

  # Set up icons
  custom_icons <- pchIcons(pch = rep(22, nrow(cover_data)),
                                            width = 30,
                                            height = 30,
                                            bg = colorspace::darken(cover_data$color),
                                            col = cover_data$color, 0.3)
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
                        # layerId = ~cover_data$key,
                        labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold")),
                        popup = ~paste0("<br><strong>Non-native cover:</strong> ", cover_data$NonNative_Cover_Total_pct,
                                        "<br><strong>Native cover:</strong> ", cover_data$Native_Cover_Total_pct,
                                        "<br><strong>Sampling Frame:</strong> ", cover_data$Sampling_Frame,
                                        "<br><strong>Cycle:</strong> ", cover_data$Cycle,
                                        "<br><strong>Year:</strong> ", cover_data$Year))

  map %<>% leaflet::addScaleBar(position = "bottomleft")

  return(map)
}

#' Map Total Native vs. Non-native vegetation cover
#'
#' @inheritParams PlotAndTransectLocations
#'
#' @return A leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' MapCoverTotal(protocol = "FTPC") #currently only FTPC mapping supported
#' MapCoverTotal(park = "AMME")
#' MapCoverTotal(sample_frame = "Puu Alii")
#' }
MapCoverTotal2 <- function(crosstalk = FALSE, crosstalk_group = "cover", combine_strata = TRUE, cycle, park, sample_frame, community, year, plot_type, is_qa_plot, certified, verified, silent = TRUE) {

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
  cover_data <- UnderNativityCover(combine_strata = combine_strata, paired_change = FALSE, crosstalk = FALSE, park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle,
                                   plot_type = plot_type, silent = silent)

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
    dplyr::mutate(nat_ratio = Native_Cover_Total_pct / (NonNative_Cover_Total_pct + Native_Cover_Total_pct) * 100) %>%
    dplyr::arrange(nat_ratio)


  # pal <- grDevices::colorRampPalette(c("red", "orange", "yellow", "green"))(length(cover_data$nat_ratio))
  ramp <- grDevices::colorRamp(c("#d11141", "#f37735", "#C8E52A", "#00b159")) #better colors for red, orange, yellow, green
  #ramp <- grDevices::colorRamp(c("red", "orange", "yellow", "green"))
  pal <- leaflet::colorNumeric(ramp, domain = c(0,100))

  # dplyr::mutate(color = dplyr::case_when(NonNative_Cover_Total_pct <= 0 & Native_Cover_Total_pct <= 0 ~ "#cccccc",#gray
  #                                        NonNative_Cover_Total_pct > 0 & Native_Cover_Total_pct <= 0 ~ "#d11141",#red
  #                                        NonNative_Cover_Total_pct > 0 & Native_Cover_Total_pct > 0 & NonNative_Cover_Total_pct > Native_Cover_Total_pct ~ "#f37735",#orange
  #                                        NonNative_Cover_Total_pct > 0 & Native_Cover_Total_pct > 0 & NonNative_Cover_Total_pct < Native_Cover_Total_pct ~ "#C8E52A",#yellow
  #                                        NonNative_Cover_Total_pct <= 0 & Native_Cover_Total_pct > 0 ~ "#00b159"))#green

  cover_data <- cover_data %>%
    dplyr::mutate(color = pal(nat_ratio))


  # Enable crosstalk if specified
  if (crosstalk) {
    cover_data <- dplyr::mutate(cover_data, key = paste0(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle))
    cover <- crosstalk::SharedData$new(cover_data, group = crosstalk_group, key = ~key)
  }

  # Set up icons
  custom_icons <- pchIcons(pch = rep(22, nrow(cover_data)),
                           width = 30,
                           height = 30,
                           bg = colorspace::darken(cover_data$color),
                           col = cover_data$color, 0.3)
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
                        # layerId = ~cover_data$key,
                        labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold")),
                        popup = ~paste0("<br><strong>Non-native cover:</strong> ", cover_data$NonNative_Cover_Total_pct,
                                        "<br><strong>Native cover:</strong> ", cover_data$Native_Cover_Total_pct,
                                        "<br><strong>Sampling Frame:</strong> ", cover_data$Sampling_Frame,
                                        "<br><strong>Cycle:</strong> ", cover_data$Cycle,
                                        "<br><strong>Year:</strong> ", cover_data$Year))

  map %<>% leaflet::addScaleBar(position = "bottomleft")

  return(map)
}

# this is modified from
# https://github.com/rstudio/leaflet/blob/master/inst/examples/icons.R#L24
#' Generate png icons for use in Leaflet
#'
#' @param pch Plot character code ([full list here](http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r))
#' @param width Width of character
#' @param height Height of character
#' @param bg Background color
#' @param col Color
#' @param ... Additional arguments to [graphics::points()]
#'
#' @return Filename(s) of icon(s)
pchIcons <- function(pch = 1, width = 30, height = 30, bg = "transparent", col = "black", lwd = 3, ...) {
  n = length(pch)
  files = character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f = filename(pch[i], col[i], bg[i])
    if (!file.exists(f)) {
      grDevices::png(f, width = width, height = height, bg = "transparent")
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()
      graphics::points(.5, .5, pch = pch[i], col = col[i], bg = bg[i], cex = min(width, height) / 8, lwd = lwd, ...)
      grDevices::dev.off()
    }
    files[i] = f
  }

  return(files)
}


#' Generate filename for icon generated by [pchIcons()]
#' @description Helper function for [pchIcons()] and [build_legend()]
#' @inheritParams pchIcons
#' @return Filename and path
#'
filename <- function(pch, col, bg) {
  icon_dir <- normalizePath(rappdirs::user_cache_dir(appname = "pacnvegetation"), winslash = "/", mustWork = FALSE)
  path <- paste0(icon_dir, '/', pch, '_', bg, "_", col, '.png')
  return(path)
}


#' Map PACN vegetation plots and transects
#'
#' @inheritParams PlotAndTransectLocations
#'
#' @return A leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' MapPACNVeg(protocol = "FTPC")
#' MapPACNVeg(park = "AMME")
#' }
MapPACNVeg2 <- function(protocol = c("FTPC", "EIPS"), crosstalk = FALSE, crosstalk_group = "map", park, sample_frame, cycle, plot_type, is_qa_plot, transect_type, certified, verified) {
  pts <- PlotAndTransectLocations(protocol = protocol, crosstalk = crosstalk, crosstalk_group = crosstalk_group, park = park, sample_frame = sample_frame, cycle = cycle, plot_type = plot_type, is_qa_plot = is_qa_plot, transect_type = transect_type, certified = certified, verified = verified)

  pts <- pts %>%
    dplyr::mutate(symb_color = dplyr::case_when(Sample_Unit_Type == "Fixed" ~ "#0000FF",#blue
                                           Sample_Unit_Type == "Rotational" ~ "#FF0000")) %>% #red
    dplyr::mutate(map_symb = dplyr::case_when(Sample_Unit == "Plot" ~ 22,#square
                                              Sample_Unit == "Transect" ~ 21)) %>% #circle
    dplyr::mutate(symb_w = dplyr::case_when(Sample_Unit == "Plot" ~ 30,#larger
                                              Sample_Unit == "Transect" ~ 20)) %>% #smaller
    dplyr::mutate(symb_h = dplyr::case_when(Sample_Unit == "Plot" ~ 30,#larger
                                              Sample_Unit == "Transect" ~ 20))#smaller


  # If pts is a crosstalk object, extract just the data for functions that need a regular tibble/dataframe
  if (crosstalk) {
    pts_data <- pts$data()
  } else {
    pts_data <- pts
  }

  if ("EIPS" %in% protocol) {
    tsect_lines_df <- pts_data %>%
      dplyr::filter(!is.na(Transect_Line), !purrr::map_lgl(Transect_Line, is.null)) %>%
      tibble::rowid_to_column(var = "id")
    tsect_lines <- apply(tsect_lines_df, 1, function(df) {return(sp::Lines(df$Transect_Line, df$id))})
    tsect_lines <- sp::SpatialLines(tsect_lines)
    tsect_lines <- sp::SpatialLinesDataFrame(tsect_lines, tsect_lines_df, match.ID = TRUE)
  }

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

  # Set up icons
  custom_icons <- pchIcons(pch = pts$map_symb,
                           width = pts$symb_w,
                           height = pts$symb_h,
                           bg = colorspace::darken(pts$symb_color),
                           col = pts$symb_color, 0.3)

  # Set up group labels for layers control
  grps <- paste(protocol, "points")
  if ("EIPS" %in% protocol) {
    grps <- c(grps, "EIPS transects")
  }

  map <- leaflet::leaflet(pts) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet.esri::addEsriFeatureLayer(options = leaflet.esri::featureLayerOptions(where = paste0("Sampling_Frame == '", sample_frame, "'")),
                                      group = "Sampling Frame",
                                      url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0",
                                      useServiceSymbology = TRUE,
                                      labelProperty = "Sampling_Frame") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = c("Sampling Frame", grps),
                              options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
    leaflet::addMarkers(lng = ~Long,
                        lat = ~Lat,
                        icon = ~leaflet::icons(iconUrl = custom_icons,
                                               iconWidth = symb_w,
                                               iconHeight = symb_h),
                        group = ~paste(Protocol, "points"),
                        label = ~Sample_Unit_Number,
                        labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold")),
                        popup = ~paste0("<strong>", Protocol, " ", Sample_Unit, ":</strong> ", Sample_Unit_Number,
                                        "<br><strong>", Sample_Unit, " Type:</strong> ", Sample_Unit_Type,
                                        "<br><strong>Sampling Frame:</strong> ", Sampling_Frame,
                                        "<br><strong>Cycle:</strong> ", Cycle,
                                        "<br><strong>Year:</strong> ", Year))

  # Add EIPS transect lines
  if ("EIPS" %in% protocol) {
    map <- leaflet::addPolylines(map, data = tsect_lines,
                                 group = "EIPS transects", color = tsect_lines@data$symb_color, opacity = 0.8)
  }

  map %<>% leaflet::addScaleBar(position = "bottomleft")

  return(map)
}
