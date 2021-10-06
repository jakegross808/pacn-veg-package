#' Map PACN vegetation plots and transects
#'
#' @param protocol Character vector indicating whether to include FTPC plots, EIPS transects, or both
#' @inheritParams FilterPACNVeg
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' MapPACNVeg(protocol = "FTPC")
#' MapPACNVeg(park = "AMME")
#' }
MapPACNVeg <- function(protocol = c("FTPC", "EIPS"), crosstalk = FALSE, crosstalk_group, park, sample_frame, cycle, plot_type, is_qa_plot, transect_type, certified, verified) {

  if (!all(toupper(protocol) %in% c("FTPC", "EIPS"))) {
    stop("Invalid protocol selection. Protocol must be 'FTPC', 'EIPS', or c('FTPC', 'EIPS')")
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

  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

  map <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = protocol,
                              options=leaflet::layersControlOptions(collapsed = TRUE))
  ftpc_icons <- leaflet::awesomeIcons(icon = "leaf", library = "fa", markerColor = "white", iconColor ="#56903A", squareMarker = TRUE)
  eips_icons <- leaflet::awesomeIcons(icon = "leaf", library = "fa", markerColor = "white", iconColor ="#C56C39", squareMarker = TRUE)

  if ("FTPC" %in% toupper(protocol)) {
    ftpc_pts <- FilterPACNVeg("Events_extra_xy", park, sample_frame, cycle, plot_type, is_qa_plot, certified, verified) %>%
      dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Number, Start_Lat, Start_Long) %>%
      dplyr::arrange(Unit_Code, Sampling_Frame, Plot_Number, Year, Cycle) %>%
      dplyr::summarise(Cycles = paste(Cycle, collapse = ", "),
                       Years = paste(Year, collapse = ", ")) %>%
      dplyr::ungroup()
    map <- map %>%
      leaflet::addAwesomeMarkers(data = ftpc_pts,
                          lng = ~Start_Long,
                          lat = ~Start_Lat,
                          icon = ftpc_icons,
                          label = ~Plot_Number,
                          group = "FTPC",
                          popup = ~paste0("<strong>FTPC Plot:</strong> ", Plot_Number,
                                          "<br><strong>Sampling Frame:</strong> ", Sampling_Frame,
                                          "<br><strong>Cycle:</strong> ", Cycles,
                                          "<br><strong>Year:</strong> ", Years))
  }
  if ("EIPS" %in% toupper(protocol)) {
    eips_pts <- FilterPACNVeg("Events_extra_xy_EIPS", park, sample_frame, cycle, transect_type, certified, verified) %>%
      dplyr::group_by(Unit_Code, Sampling_Frame, Transect_Number, Lat, Long) %>%
      dplyr::arrange(Unit_Code, Sampling_Frame, Transect_Number, Year, Cycle) %>%
      dplyr::summarise(Cycles = paste(Cycle, collapse = ", "),
                       Years = paste(Year, collapse = ", ")) %>%
      dplyr::ungroup()
    map <- map %>%
      leaflet::addAwesomeMarkers(data = eips_pts,
                          lng = ~Long,
                          lat = ~Lat,
                          icon = eips_icons,
                          label = ~Transect_Number,
                          group = "EIPS",
                          popup = ~paste0("<strong>EIPS Transect:</strong> ", Transect_Number,
                                          "<br><strong>Sampling Frame:</strong> ", Sampling_Frame,
                                          "<br><strong>Cycle:</strong> ", Cycles,
                                          "<br><strong>Year:</strong> ", Years))
  }

  map %<>% leaflet::addScaleBar(position = "bottomleft")

  return(map)
}
