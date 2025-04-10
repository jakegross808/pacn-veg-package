# Similar function but maps EIPS data to inter-stations instead of segments:
samp <- "Nahuku/East Rift"
samp <- "Olaa"

test_data <- v_EIPS_prep(sample_frame = samp)

v_EIPS_map_interstation3(.data = test_data,
                         parameter = "Mean_Species_Cover",
                         change = FALSE,
                         agol_sample_frame = samp)

nonnative_orchids <- c("Arundina graminifolia", "Phaius tankervilleae", "Spathoglottis plicata")



EIPS_orchids_Olaa <- station_summary_centroids |>
  sf::st_drop_geometry() |>
  dplyr::select(-geometry)

typeof(EIPS_orchids_Olaa)

EIPS_orchids_Olaa_filter <- EIPS_orchids_Olaa |>
  dplyr::filter(Scientific_Name %in% nonnative_orchids) |>
  dplyr::filter(!is.na(Sampling_Frame))



#Add this to utils.R in EIPS section


# Image Points
all_segs <- dplyr::tbl(conn, "tbl_Segments") %>%
  dplyr::collect()

tbl_Events <- dplyr::tbl(conn, "tbl_Events") %>%
  dplyr::collect()

tbl_Transects <- dplyr::tbl(conn, "tbl_Transects") %>%
  dplyr::collect()

tbl_Locations <- dplyr::tbl(conn, "tbl_Locations") %>%
  dplyr::collect()

tlu_Segment_Points <- dplyr::tbl(conn, "tlu_Segment_Points") %>%
  dplyr::collect()

tbl_Sites <- dplyr::tbl(conn, "tbl_Sites") %>%
  dplyr::collect()

tlu_Points_Images <- dplyr::tbl(conn, "tlu_Points_Images") %>%
  dplyr::collect()

tbl_Image_Points <- dplyr::tbl(conn, "tbl_Image_Points") %>%
  dplyr::collect()

# Create a segments spatial file from Image Points

# Get Station Coordinates and turn into segment coordinates
EIPS_pts <- FilterPACNVeg(data_name = "EIPS_image_pts",
                          is_qa_plot = FALSE)

EIPS_pts <- EIPS_pts |>
  dplyr::filter(Sampling_Frame == "Olaa")

leaflet::leaflet(EIPS_pts) |>
  leaflet::addCircles()


#Change Year to first year of the Cycle and prep data
EIPS_pts <- EIPS_pts %>%
  dplyr::group_by(Sampling_Frame, Cycle) %>%
  dplyr::mutate(Year = min(Year)) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Transect_Number = as.factor(Transect_Number)) %>%
  dplyr::select(-Latitude_Dir, -Longitude_Dir, -GCS, -GPS_Error)

#join to GPS points:
station_summary2 <- station_summary %>%
  dplyr::mutate(Start_Image_Point = as.character(Start_Station_m)) %>%
  dplyr::left_join(EIPS_pts, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "Start_Image_Point" = "Image_Point")) %>%
  dplyr::rename(Lat_Start = Latitude,
                Long_Start = Longitude) %>%
  dplyr::mutate(End_Image_Point = as.character(End_Station_m)) %>%
  dplyr::left_join(EIPS_pts, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "End_Image_Point" = "Image_Point")) %>%
  dplyr::rename(Lat_End = Latitude,
                Long_End = Longitude)


# Create segments data table to join with spatial table (from above)

all_segs_events <- all_segs |>
  dplyr::left_join(tbl_Events, by = join_by(Event_ID)) |>
  dplyr::left_join(tbl_Transects, by = join_by(Transect_ID)) |>
  dplyr::left_join(tbl_Locations, by = join_by(Location_ID)) |>
  dplyr::left_join(tlu_Segment_Points, by = join_by(Segment_ID)) |>
  #dplyr::left_join(tbl_Sites, by = join_by(Site_ID)) |>
  # add "Year" (year sampled) and "Cycle" (sample cycle)
  dplyr::mutate(Year = lubridate::year(Start_Date)) |>
  dplyr::mutate(Cycle = ifelse(Year <= 2014, 1,
                               ifelse(Year >= 2015 & Year <= 2020, 2,
                                      ifelse(Year >= 2021, 3, NA)))) |>
  mutate(Segment = case_when(
      Sampling_Frame == "Kahuku" |
        Sampling_Frame == "Muchot" |
        Plant_Community == "Coastal Strand"  ~ Segment_10,
      .default = Segment_20))  |>
  dplyr::select(Year, Unit_Code, Sampling_Frame, Plant_Community,
                Transect_Number, Transect_Type, Segment,
                Sort_Order, No_Data, Sampling_Frame_English, Event_ID, Segment_ID) |>
  dplyr::arrange(desc(Year), Sampling_Frame, Transect_Number) |>
  dplyr::mutate(Segment = str_replace_all(Segment, " ", "")) |>
  tidyr::separate_wider_delim(Segment, "-", names = c("Segment_Start", "Segment_End"), cols_remove = FALSE)

stations_no_end <- all_segs_events |>
  dplyr::inner_join(tbl_Image_Points, by = join_by(Event_ID, Segment_Start == Image_Point))
stations_end <- all_segs_events |>
  group_by(Event_ID) |>
  filter(points == max(points, na.rm=TRUE))
  dplyr::inner_join(tbl_Image_Points, by = join_by(Event_ID, Segment_Start == Image_Point))



dplyr::left_join(tlu_Points_Images, by = join_by(Event_ID, Image_Point))


