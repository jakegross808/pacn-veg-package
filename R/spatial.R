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
PlotAndTransectLocations <- function(protocol = c("FTPC", "EIPS"), crosstalk = FALSE, crosstalk_group = "map", park, sample_frame, cycle, plot_type, is_qa_plot = FALSE, transect_type, certified, verified) {
  if (!all(toupper(protocol) %in% c("FTPC", "EIPS"))) {
    stop("Invalid protocol selection. Protocol must be 'FTPC', 'EIPS', or c('FTPC', 'EIPS')")
  }

  ftpc_pts <- FilterPACNVeg(data_name = "Events_extra_xy", park = park, sample_frame = sample_frame, cycle = cycle, plot_type = plot_type, is_qa_plot = is_qa_plot, certified = certified, verified = verified) %>%
    dplyr::mutate(Protocol = "FTPC", Sample_Unit = "Plot") %>%
    dplyr::rename(Sample_Unit_Number = Plot_Number, Sample_Unit_Type = Plot_Type, Lat = Start_Lat, Long = Start_Long) %>%
    dplyr::select(Protocol, Unit_Code, Sampling_Frame, Sample_Unit, Sample_Unit_Type, Sample_Unit_Number, Lat, Long, Year, Cycle) %>%
    # Change so all sampling cyles have same year (ie first year of new cycle)
    dplyr::group_by(Sampling_Frame, Cycle) %>%
    dplyr::mutate(Year = min(Year)) %>%
    dplyr::ungroup()

  eips_pts <- FilterPACNVeg(data_name = "Events_extra_xy_EIPS", park = park, sample_frame = sample_frame, cycle = cycle, transect_type = transect_type, certified = certified, verified = verified) %>%
    dplyr::mutate(Protocol = "EIPS", Sample_Unit = "Transect") %>%
    dplyr::rename(Sample_Unit_Number = Transect_Number, Sample_Unit_Type = Transect_Type) %>%
    dplyr::select(Protocol, Unit_Code, Sampling_Frame, Sample_Unit, Sample_Unit_Type, Sample_Unit_Number, Lat, Long, Year, Cycle) %>%
    # Change so all sampling cyles have same year (ie first year of new cycle)
    dplyr::group_by(Sampling_Frame, Cycle) %>%
    dplyr::mutate(Year = min(Year)) %>%
    dplyr::ungroup()

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
MapCoverTotal2 <- function(crosstalk = FALSE, crosstalk_group = "cover", combine_strata = TRUE, cycle, park, sample_frame, community, year, plot_type, is_qa_plot = FALSE, certified, verified, silent = TRUE) {

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
                        #layerId = ~cover_data$key,
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
MapPACNVeg2 <- function(protocol = c("FTPC", "EIPS"), crosstalk = FALSE, crosstalk_group = "map", park, sample_frame, cycle, plot_type, is_qa_plot = FALSE, transect_type, certified, verified) {
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

  # AGOL Layers

  #------------.
  # Sampling Frames in AGOL currently do not have same names as in database...
  # GIS layer should match sampling frame names from database in the future,
  # Temporary Fixes for this go here:
  if (sample_frame == "Mauna Loa") {
    agol_sample_frame <- "Subalpine Shrubland"

  } else if (sample_frame == "Guam") {
    agol_sample_frame <- "Limestone Forest"

  } else if (sample_frame == "Kipahulu District") {
    agol_sample_frame <- "Kīpahulu District"

  } else if (sample_frame == "Nahuku/East Rift") {
    agol_sample_frame <- "Thurston/East Rift"

  } else if (sample_frame == "Hoolehua" | sample_frame == "Kalawao") {
    agol_sample_frame <- "KALA Coast"

  } else {
    agol_sample_frame <- sample_frame
  }
  #------------.

  url <- httr::parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/")
  url$path <- paste(url$path, "PACN_Vegetation_Sampling_Frames_vlyr/FeatureServer/0/query", sep = "/")
  url$query <- list(where = paste0("Sampling_Frame = '", agol_sample_frame, "'"),
                    outFields = "*",
                    returnGeometry = "true",
                    f = "geojson")
  agol_request <- httr::build_url(url)
  agol_sf <- sf::st_read(agol_request, quiet = TRUE)

  if (is.na(agol_sf$Zone)) {
    agol_sf$Zone <- agol_sf$Sampling_Frame
  }

  factpal <- leaflet::colorFactor(c("#F8573A", "#F4C47B", "#28468B", "#AED5CB"),
                                  agol_sf$Zone) # Colors for polygons factors
  #factpal <- leaflet::colorFactor(topo.colors(5), agol_sf$Zone) # Colors for polygons factors
  #leaflet::addPolygons(color =  ~factpal(Zone), label = ~Zone)

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
    leaflet::addPolygons(data = agol_sf, group = "Zone", color =  ~factpal(Zone), label = ~Zone) %>%
    #leaflet.esri::addEsriFeatureLayer(options = leaflet.esri::featureLayerOptions(where = paste0("Sampling_Frame = '", sample_frame, "'")),
    #                                  group = "Sampling Frame",
    #                                  url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_vlyr/FeatureServer/0",
    #                                  useServiceSymbology = TRUE,
    #                                  labelProperty = "Sampling_Frame") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = c("Zone", grps),
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

#' Add Management Unit
#'
#' Adds Management Unit and Zone Categorical Variables to Events Table and
#'
#' Saves a new Table (tibble) with management unit for use in other functions.
#' Management unit is added to the table by performing a spatial join
#' using both a specific AGOL management layer (hard coded)
#' and the X,Y plot center coordinates found in the Events table.
#'
#' @param sample_frame Sampling_Frame name. For all sampling frames, use: Sampling_Frame = "All"
#'
#' @return A tibble copy of "Events_extra_xy" with columns 'Zone' and 'Mgmt' added for each event.
#' Saved to R folder for now.
#' @export
#'
add_mgmt_unit <- function(sample_frame){
  # Get events table with plot locations:
  if (sample_frame == "All") {
    Events_filtered <- FilterPACNVeg("Events_extra_xy" ,
                                     is_qa_plot = FALSE)
  } else {
    Events_filtered <- FilterPACNVeg("Events_extra_xy" ,
                                     sample_frame = sample_frame,
                                     is_qa_plot = FALSE)
  }

  # Add message if missing center X, Y coordinates
  # populate center coordinates with start coordinates, if available
  missing_center_xy <- Events_filtered %>%
    dplyr::filter(is.na(Center_Lat) | is.na(Center_Long)) %>%
    dplyr::select(-Azimuth_Plot, -QA_Plot, -GCS, -Lat_Dir, -Long_Dir, -Certified, -Verified)

  count_missing_center_xy <- nrow(missing_center_xy)

  if (nrow(missing_center_xy) > 0) {
    message(paste0(count_missing_center_xy, " records are missing plot center X,Y coordinates:"))
    print(missing_center_xy)
    message("Using Start_Lat, Start_Long if avaiable")

    Events_filtered <- Events_filtered %>%
      dplyr::mutate(Center_Lat = dplyr::case_when(
        is.na(Center_Lat) ~ Start_Lat,
        TRUE ~ Center_Lat)) %>%
      dplyr::mutate(Center_Long = dplyr::case_when(
        is.na(Center_Long) ~ Start_Long,
        TRUE ~ Center_Long))
  }

  # Add message if missing center X, Y coordinates
  # populate center coordinates with start coordinates, if available
  still_missing_center_xy <- Events_filtered %>%
    dplyr::filter(is.na(Center_Lat) | is.na(Center_Long)) %>%
    dplyr::select(-Azimuth_Plot, -QA_Plot, -GCS, -Lat_Dir, -Long_Dir, -Certified, -Verified)

  count_still_missing_center_xy <- nrow(still_missing_center_xy)

  if (nrow(still_missing_center_xy) > 0) {
    message(paste0(count_still_missing_center_xy, " records are missing BOTH",
                   " center & start X,Y coordinates. These records were dropped:"))
    print(still_missing_center_xy)

    Events_filtered <- Events_filtered %>%
      dplyr::filter(!is.na(Center_Lat))
  }

  # Make the events table a simple features object for spatial join
  # Use the plot center coordinates as the spatial location
  # 'crs = 4326' is saying the coordinate reference system used is EPSG 4326
  # 4326 is the EPSG registry identifier for WGS84

  plots_sf <- sf::st_as_sf(Events_filtered, coords = c("Center_Long", "Center_Lat"), crs = 4326)

  # Sampling Frames in AGOL currently do not have same names as in database...
  # GIS layer should match sampling frame names from database in the future,
  # Temporary Fixes for this go here:
  if (sample_frame == "Mauna Loa" | sample_frame == "Haleakala") {
    agol_sample_frame <- "Subalpine Shrubland"

  } else if (sample_frame == "Guam") {
    agol_sample_frame <- "Limestone Forest"

  } else if (sample_frame == "Nahuku/East Rift") {
    agol_sample_frame <- "Thurston/East Rift"

  } else if (sample_frame == "Hoolehua" | sample_frame == "Kalawao") {
    agol_sample_frame <- "KALA Coast"

  } else {
    agol_sample_frame <- sample_frame
  }

  if (sample_frame == "All") {
    # Read/Load AGOL layer:
    url <- httr::parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/")
    url$path <- paste(url$path, "PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0/query", sep = "/")
    url$query <- list(where = paste0("Sampling_Frame = Sampling_Frame"),
                      outFields = "*",
                      returnGeometry = "true",
                      f = "geojson")
    request <- httr::build_url(url)
    request #print url request
  } else {

    # Read/Load AGOL layer:
    url <- httr::parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/")
    url$path <- paste(url$path, "PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0/query", sep = "/")
    url$query <- list(where = paste0("Sampling_Frame = '", agol_sample_frame, "'"),
                      outFields = "*",
                      returnGeometry = "true",
                      f = "geojson")
    request <- httr::build_url(url)
    request #print url request
  }

  # Convert AGOL layer into a simple features object
  mgmt_unit <- sf::st_read(request)

  # Add line to make geometry valid - otherwise was receiving following error:
  # Error: "Edge 270 has duplicate vertex with edge 273"
  # see following for more information: https://r-spatial.org/r/2017/03/19/invalid.html
  mgmt_unit_valid <- sf::st_make_valid(mgmt_unit)

  #st_is_valid(mgmt_unit, reason = TRUE)
  #st_is_valid(mgmt_unit_valid, reason = TRUE)
  # An alternative to "st_make_valid" is to turn spherical geometry off:
  #sf_use_s2(TRUE) # turn off spherical geometry

  # Spatially join plot locations with mgmt unit layer
  # copies mgmt unit attribute (categorical variable) to the events (plots) table:
  plots_mgmt_sf <- sf::st_join(plots_sf, mgmt_unit_valid)

  # Drop sf geometry from table
  plots_mgmt <- plots_mgmt_sf %>%
    sf::st_drop_geometry()

  # Pull center coordinates from geometry and
  # put coordinates back into X,Y (Center_Long, Center_Lat) columns for the events table
  xy <- sf::st_coordinates(plots_mgmt_sf$geometry) %>%
    tibble::as_tibble() %>%
    dplyr::select(Center_Long = X, Center_Lat = Y) %>%
    dplyr::bind_cols(plots_mgmt)

  # Select columns to match original Events table
  plots_mgmt <- xy %>%
    dplyr::select(Unit_Code = Unit_Code.x, Sampling_Frame = Sampling_Frame.x,
                  Year, Cycle, Plot_Type, Plot_Number, Azimuth_Plot, QA_Plot,
                  Start_Lat, Start_Long, Center_Lat, Center_Long, End_Lat, End_Long,
                  GCS, Lat_Dir, Long_Dir, Certified, Verified,
                  Zone) # Add Mgmt Unit here when available

  # Write to R folder in project. Is this best location?
  new_folder <- paste0(getwd(),"/R")
  new_folder
  dir.create(new_folder)
  readr::write_csv(plots_mgmt, file = paste0(getwd(),"/R/Events_extra_xy_mgmt.csv"))

}

#' Download AGOL attachments (JPG or PNG)
#'
#' From MOJN/Sarah Wright. Currently only works with jpeg or png images.
#'
#' @param feature_layer_url URL of the feature layer containing attachments (including layer ID). *Do not* include a slash at the end.
#' @param agol_username AGOL username for a headless account with permissions to view the feature layer.
#' @param agol_password Do not hardcode passwords in your saved code! By default, assumes that the password was stored using [keyring::key_set()] with `service = AGOL`.
#' @param test_run If `TRUE`, returns attachment data and proposed file locations without actually downloading and saving attachments.
#' @param dest_folder Folder in which to save downloaded attachments
#' @param prefix Prefix for photo file names. Can either be a character string (to set the same prefix for all files) or an unquoted column name from the attachment table.
#' @param sep Separator character to use between prefix and photo ID in file names. Ignored if `custom_name` == FALSE
#' @param custom_name Use prefix and sep to create filenames from attachment ID's? If `FALSE`, will use the filename as stored in AGOL. If you are not customizing your filenames when saving attachments, this may cause problems due to duplicate filenames.
#' @param append_id Append attachment id to custom prefix? Ignored if `custom_name == FALSE`
#' @param join_cols Named vector in the format `c("foreign key column to data table in attachment table" = "primary key column of data table")` where the attachment table is the table that stores attachments only (this is mostly hidden from view in AGOL) and the data table is the table for which attachments have been enabled. If you aren't sure what the foreign key column is called, leave this argument empty and set `test_run = TRUE`. The function will try to guess the join columns, but if it gets it wrong, it will return the attachment table without the accompanying data table. At that point, you can check the name(s) of the foreign key column(s) and re-run your code with the join columns specified. They should be something along the lines of "parentGlobalId" or "parentObjectId".
#'
#' @return Tibble of attachment data, including paths to saved files.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic use
#' DownloadAGOLAttachments("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_SV_OverviewPhotos/FeatureServer/0", agol_username = "mojn_data")
#'
#' # Include data from table for which attachments are enabled
#' DownloadAGOLAttachments("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_SV_OverviewPhotos/FeatureServer/0",
#'                         agol_username = "mojn_data",
#'                         join_cols = c("parentGlobalId" = "globalID"))
#' }

DownloadAGOLAttachments <- function(feature_layer_url,
                                    agol_username,
                                    agol_password = keyring::key_get("AGOL", agol_username),
                                    dest_folder = "photo_downloads",
                                    custom_name = TRUE,
                                    prefix = "photo",
                                    append_id = TRUE,
                                    sep = "_",
                                    join_cols = c(),
                                    test_run = FALSE) {

  # Format destination folder path properly and create it if it doesn't exist
  dest_folder <- normalizePath(dest_folder, winslash = .Platform$file.sep, mustWork = FALSE)
  if (!dir.exists(dest_folder)) {
    dir.create(dest_folder, recursive = TRUE)
  }

  # AGOL authentication - get token
  token_resp <- httr::POST("https://nps.maps.arcgis.com/sharing/rest/generateToken",
                           body = list(username = agol_username,
                                       password = agol_password,
                                       referer = 'https://irma.nps.gov',
                                       f = 'json'),
                           encode = "form")
  agol_token <- jsonlite::fromJSON(httr::content(token_resp, type="text", encoding = "UTF-8"))

  # Get attachment table data
  data <- httr::GET(paste0(feature_layer_url, "/query"),
                    query = list(where="1=1",
                                 outFields="*",
                                 f="JSON",
                                 token=agol_token$token)) %>%
    httr::content(type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()

  global_id <- data$globalIdFieldName  # Get global ID field name
  object_id <- data$objectIdFieldName  # Get object ID field name

  data <- data$features$attributes %>%
    tibble::as_tibble()



  # Get attachment info
  attrs <- httr::GET(paste0(feature_layer_url, "/queryAttachments"),
                     query = list(f="JSON",
                                  definitionExpression = "1=1",
                                  returnUrl = "true",
                                  returnMetadata="true",
                                  token=agol_token$token)) %>%
    httr::content(type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()

  # Get attachment info as a dataframe and generate file paths for saving attachments
  attachments <- attrs$attachmentGroups %>%
    tibble::as_tibble() %>%
    tidyr::unnest(cols = attachmentInfos)

  # Pull photo date out of the nested Exif and add as formatted column
  # attachments <- attachments %>%
  #   select(exifInfo) %>%
  #   unnest(cols = c(exifInfo)) %>%
  #   filter(name == "Exif IFD0") %>%
  #   select(tags) %>%
  #   unnest(cols = c(tags)) %>%
  #   filter(name == "Date/Time") %>%
  #   select(value) %>%
  #   mutate(value2 = as.character(str_replace_all(value, ":", ""))) %>%
  #   mutate(photo_day_time = as.character(str_replace_all(value2, " ", "_"))) %>%
  #   select(photo_day_time) %>%
  #   bind_cols(attachments)

  # Join attachment table data to attachment info
  if (is.null(join_cols)) {
    if ("parentGlobalId" %in% names(attachments) && length(global_id) == 1) {
      join_cols <- c("parentGlobalId" = global_id)
    } else if ("parentObjectId" %in% names(attachments) && length(object_id) == 1) {
      join_cols <- c("parentObjectId" = object_id)
    }
  }

  if (length(join_cols) > 0) {
    attachments <- dplyr::left_join(attachments, data, by = join_cols)
  } else {
    warning("Could not join attachment info to parent data table")
  }

  if (!append_id) {
    sep <- ""  # Make separator blank if not appending ID to photo name
  }

  # Make Subject column compatible for all layers

  # Change Subject_FTPC and Subject_EIPS to just "Subject"
  lookup <- c(Subject = "Subject_FTPC", Subject = "Subject_EIPS")
  attachments <- dplyr::rename(attachments, tidyselect::any_of(lookup))

  # If "Plant" layer use Code from Photo_Taxon as "Subject"
  if (any(names(attachments) %in% c("Photo_Taxon"))) {
    attachments$Subject <- stringr::str_extract(attachments$Photo_Taxon, "(?<=\\().*?(?=\\))")
  }

  # remove all special characters from Subject_other & append to any Subject == other
  if (any(names(attachments) %in% c("Subject_other"))) {
    attachments <- attachments %>%
      dplyr::mutate(Sub_other = stringr::str_remove(Subject_other, "[[:punct:]]")) %>%
      dplyr::mutate(Sub_other = stringr::str_replace_all(Sub_other," ", "_")) %>%
      dplyr::mutate(Subject = dplyr::case_when(Subject == "Other" ~ paste(Subject, Sub_other, sep = "_"),
                                               .default = as.character(Subject)))
  }

  # utilize numeric count of photos from Field Maps
  attachments$photo_cnt <- stringr::str_extract(string = attachments$name, pattern = "(\\d)+")

  # utilize date column from point created in field maps
  attachments <- attachments %>%
    dplyr::mutate(pt_date = as.character(as.POSIXct(created_date/1000,
                                                    origin="1970-01-01"))) %>%
    #tidyr::separate_wider_delim(pt_date, " ",
    #                            names = c("pt_date_file", NA),
    #                            cols_remove = FALSE) %>%
    #dplyr::mutate(pt_date_file = stringr::str_remove_all(pt_date_file,
    #                                                     pattern = "-")) %>%
    dplyr::mutate(pt_date_file = stringr::str_remove_all(string = pt_date,
                                                        pattern = "[-:]")) %>%
    dplyr::mutate(pt_date_file = stringr::str_replace(string = pt_date_file,
                                                     pattern = " ",
                                                     replacement = "_")) %>%
    # Drop seconds from pt_date_file
    dplyr::mutate(pt_date_file = stringr::str_sub(pt_date_file, end = -3)) %>%
    # Separate Site_numb column into protocol and site columns
    tidyr::separate_wider_delim(Site_numb, " ",
                                names = c("protocol", "site_typenum"),
                                cols_remove = FALSE,
                                too_many = "merge")

  # Create new name utilizing columns above
  attachments <- attachments %>%
    dplyr::mutate(new_name = paste(pt_date_file, id, site_typenum, Subject, sep = "_")) #photo_cnt removed because was not unique sometimes!

  attachments <- attachments %>%
    dplyr::mutate(customFileName = custom_name,
                  appendID = append_id,
                  fileExt = paste0(".", tools::file_ext(name)),  # Get file extension
                  fileName = ifelse(customFileName, paste(new_name, ifelse(appendID, id, ""), sep = sep), tools::file_path_sans_ext(name)),
                  #fileName = ifelse(customFileName, paste({{prefix}}, ifelse(appendID, id, ""), sep = sep), tools::file_path_sans_ext(name)),  # If custom_name is FALSE, just use the original filename
                  fileDest = file.path(dest_folder, paste0(fileName, fileExt))) %>%  # Full file path
    dplyr::select(-fileExt, -fileName, -customFileName)

  if (length(unique(attachments$fileDest)) != length(attachments$fileDest)) {
    stop("Cannot save photos due to duplicate filenames. Try setting `custom_name = TRUE`.")
  }

  if (!test_run) {
    apply(attachments, 1, function(att) {
      dat <- httr::GET(att$url,
                       query = list(token=agol_token$token)) %>%
        httr::content()
      if (grepl("jpe{0,1}g", att$contentType, ignore.case = TRUE)) {
        jpeg::writeJPEG(dat, att$fileDest)
      } else if (grepl("png", att$contentType, ignore.case = TRUE)){
        png::writePNG(dat, target = att$fileDest)
      } else {
        warning(paste("Could not write", att$fileDest, "- unsupported file type"))
      }
    })
  }

  return(attachments)
}
