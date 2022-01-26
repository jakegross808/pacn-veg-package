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
    dplyr::filter(Cycle == max(Cycle), Year == max(Year),
                  !is.na(Latitude), !is.na(Longitude)) %>%
    dplyr::ungroup()
  # execute the following line separately so we can suppress the warning that as.numeric is generating NA values
  suppressWarnings(eips_tsects <- dplyr::mutate(eips_tsects, Image_Point = as.numeric(Image_Point), Protocol = "EIPS", Sample_Unit = "Transect"))
  eips_tsects <- eips_tsects %>%
    dplyr::rename(Sample_Unit_Number = Transect_Number, Sample_Unit_Type = Transect_Type) %>%
    dplyr::arrange(Unit_Code, Community, Sampling_Frame, Year, Cycle, Sample_Unit_Type, Sample_Unit_Number, Image_Point) %>%
    tidyr::nest(Transect_Line = c(Image_Point, Latitude, Latitude_Dir, Longitude, Longitude_Dir, GCS, GPS_Error)) %>%
    mutate(Transect_Line = map(Transect_Line, function(df){
      df <- filter(df, !is.na(Longitude) && !is.na(Latitude))
      # Convert to SpatialPointsDataFrame
      sp::coordinates(df) <- c("Longitude", "Latitude")
      sp::Line(df)}
    ))

  eips_pts <- eips_pts %>%
    dplyr::group_by(Protocol, Unit_Code, Sampling_Frame, Sample_Unit, Sample_Unit_Type, Sample_Unit_Number, Lat, Long) %>%
    dplyr::arrange(Protocol, Unit_Code, Sampling_Frame, Sample_Unit_Type, Sample_Unit_Number, Year, Cycle) %>%
    dplyr::summarise(Tsect_Line_Cycle = max(Cycle),
                     Tsect_Line_Year = max(Year),
                     Cycle = paste(Cycle, collapse = ", "),
                     Year = paste(Year, collapse = ", "),) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(eips_tsects, by = c("Protocol", "Unit_Code", "Sampling_Frame", "Sample_Unit", "Sample_Unit_Type", "Sample_Unit_Number", "Tsect_Line_Cycle" = "Cycle", "Tsect_Line_Year" = "Year")) %>%
    dplyr::select(-Community)

  ftpc_pts <- ftpc_pts %>%
    dplyr::group_by(Protocol, Unit_Code, Sampling_Frame, Sample_Unit, Sample_Unit_Type, Sample_Unit_Number, Lat, Long) %>%
    dplyr::arrange(Protocol, Unit_Code, Sampling_Frame, Sample_Unit_Type, Sample_Unit_Number, Year, Cycle) %>%
    dplyr::summarise(Cycle = paste(Cycle, collapse = ", "),
                     Year = paste(Year, collapse = ", ")) %>%
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
      dplyr::filter(!is.na(Transect_Line), !map_lgl(Transect_Line, is.null)) %>%
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
