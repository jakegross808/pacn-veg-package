#' Convert EIPS Cover Class column to Range of Cover or vice-versa
#' @description Input a column with values 0-7 for Braun-Blanquet cover classes and
#' returns two percent cover columns (min and max percent cover). Note that
#' Cover Class "0" returns 0 for min and max, while cover class 7 will return 0.75
#' for min and 1.0 for max.
#' @return Filename and path
#' @export
#' @examples
#' \dontrun{
#' MapPACNVeg(protocol = "FTPC")
#' MapPACNVeg(park = "AMME")
#'}
v_EIPS_cover_class2percent <- function(df, cover_column) {
  df <- df %>%
    dplyr::mutate(Cov_Range_Min = dplyr::case_when({{ cover_column }} == 1 ~ 0.005,
                                                   {{ cover_column }} == 2 ~ 0.01,
                                                   {{ cover_column }} == 3 ~ 0.05,
                                                   {{ cover_column }} == 4 ~ 0.10,
                                                   {{ cover_column }} == 5 ~ 0.25,
                                                   {{ cover_column }} == 6 ~ 0.50,
                                                   {{ cover_column }} == 7 ~ 0.75,
                                                   is.na({{ cover_column }}) ~ 0,
                                                   {{ cover_column }} == "OUT" ~ 0)) %>%
    dplyr::mutate(Cov_Range_Max = dplyr::case_when({{ cover_column }} == 1 ~ 0.009,
                                                   {{ cover_column }} == 2 ~ 0.04,
                                                   {{ cover_column }} == 3 ~ 0.09,
                                                   {{ cover_column }} == 4 ~ 0.24,
                                                   {{ cover_column }} == 5 ~ 0.49,
                                                   {{ cover_column }} == 6 ~ 0.74,
                                                   {{ cover_column }} == 7 ~ 1.0,
                                                    is.na({{ cover_column }}) ~ 0,
                                                   {{ cover_column }} == "OUT" ~ 0))
  return(df)
}

#' Convert EIPS Cover Class column to Range of Cover or vice-versa
#' @description Input a column with values 0-7 for Braun-Blanquet cover classes and
#' returns two percent cover columns (min and max percent cover). Note that
#' Cover Class "0" returns 0 for min and max, while cover class 7 will return 0.75
#' for min and 1.0 for max.
#' @return Filename and path
#' @export
#' @examples
#' \dontrun{
#' MapPACNVeg(protocol = "FTPC")
#' MapPACNVeg(park = "AMME")
#'}
v_EIPS_cover_percent2class <- function(df, range_column, cover_column_name) {
  paste_col <- paste("col", rlang::as_string(cover_column_name), sep ="_")
  df <- df %>%
    dplyr::mutate( {{cover_column_name}} := dplyr::case_when(
      {{ range_column }} == 0 ~ 0,
      {{ range_column }} > 0 & {{ range_column }} < 0.01 ~ 1,
      {{ range_column }} >= 0.01 & {{ range_column }} < 0.05 ~ 2,
      {{ range_column }} >= 0.05 & {{ range_column }} < 0.10 ~ 3,
      {{ range_column }} >= 0.10 & {{ range_column }} < 0.25 ~ 4,
      {{ range_column }} >= 0.25 & {{ range_column }} < 0.50 ~ 5,
      {{ range_column }} >= 0.50 & {{ range_column }} < 0.75 ~ 6,
      {{ range_column }} >= 0.75 ~ 7))

  df2 <- df %>%
    dplyr::mutate({{ paste_col }} := dplyr::case_when(
      .data[[cover_column_name]] == 0  ~ "#FFFFFF",
      .data[[cover_column_name]] == 1  ~ "#F6F4C6",
      .data[[cover_column_name]] == 2  ~ "#EEE98D",
      .data[[cover_column_name]] == 3  ~ "#EAC07D",
      .data[[cover_column_name]] == 4  ~ "#E7976E",
      .data[[cover_column_name]] == 5  ~ "#E36E5F",
      .data[[cover_column_name]] == 6  ~ "#E04550",
      .data[[cover_column_name]] == 7  ~ "#DD1C41"))
  return(df2)
}

#' Prep and check EIPS data and add helpful columns for mapping ect.
#' @description Pulls EIPS dataset from database cache and performs some
#' basic data preparation.
#' @return dataframe
#' @export
#' @examples
#' \dontrun{
#' v_EIPS_prep(sample_frame = "Olaa")
#' v_EIPS_prep(park = "AMME")
#'}
v_EIPS_prep <- function(park, sample_frame, community, year, cycle, transect_type, verified, certified) {
  EIPS <- FilterPACNVeg(data_name = "EIPS_data",
                        park = park,
                        sample_frame = sample_frame,
                        community = community,
                        year = year,
                        cycle = cycle,
                        transect_type = transect_type,
                        verified = verified,
                        certified = certified)

  #EIPS_pts <- FilterPACNVeg(data_name = "EIPS_image_pts",
  #                          #sample_frame = "Olaa",
  #                          is_qa_plot = FALSE)


  # Change Year to first year of the Cycle and prep data
  #EIPS_pts <- EIPS_pts %>%
  #  dplyr::group_by(Sampling_Frame, Cycle) %>%
  #  dplyr::mutate(Year = min(Year)) %>%
  #  dplyr::mutate(Year = as.factor(Year)) %>%
  #  dplyr::ungroup() %>%
  #  dplyr::mutate(Transect_Number = as.factor(Transect_Number)) %>%
  #  dplyr::select(-Latitude_Dir, -Longitude_Dir, -GCS, -GPS_Error)

  # Change Year to first year of the Cycle and prep data
  EIPS2 <- EIPS %>%
    dplyr::group_by(Sampling_Frame, Cycle) %>%
    dplyr::mutate(Year = min(Year)) %>%
    dplyr::mutate(Year = as.factor(Year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Transect_Number = as.factor(Transect_Number))

  # Add segment meter lengths and transect meter lengths specific to Community
  EIPS3 <- EIPS2 %>%
    dplyr::mutate(Seg_Length_m = dplyr::case_when(Community == "Subalpine Shrubland" ~ 20,
                                                  Community == "Wet Forest" & Sampling_Frame != "Kahuku" ~ 20,
                                                  Community == "Wet Forest" & Sampling_Frame == "Kahuku" ~ 10,
                                                  Community == "Coastal Strand" | Community == "Mangrove Wetland" ~ 10,
                                                  TRUE ~ NA_real_)) %>%
    dplyr::mutate(Tran_Length_m = dplyr::case_when(Community == "Subalpine Shrubland" ~ 500,
                                                   Community == "Wet Forest" & Sampling_Frame != "Kahuku" ~ 1000,
                                                   Community == "Wet Forest" & Sampling_Frame == "Kahuku" ~ 250,
                                                   Community == "Coastal Strand" | Community == "Mangrove Wetland"~ 499,
                                                   TRUE ~ NA_real_)) %>%
    dplyr::mutate(Start_m = (Segment * Seg_Length_m) - Seg_Length_m,
                  End_m = (Segment * Seg_Length_m)) %>%
    dplyr::mutate(Start_Station_m = dplyr::case_when(Tran_Length_m == 500 & Start_m < 100 ~ 0,
                                                     Tran_Length_m == 500 & Start_m < 200 ~ 100,
                                                     Tran_Length_m == 500 & Start_m < 300 ~ 200,
                                                     Tran_Length_m == 500 & Start_m < 400 ~ 300,
                                                     Tran_Length_m == 500 & Start_m < 500 ~ 400)) %>%
    dplyr::mutate(Start_Station_m = dplyr::case_when(Tran_Length_m == 1000 & Start_m < 200 ~ 0,
                                                     Tran_Length_m == 1000 & Start_m < 400 ~ 200,
                                                     Tran_Length_m == 1000 & Start_m < 600 ~ 400,
                                                     Tran_Length_m == 1000 & Start_m < 800 ~ 600,
                                                     Tran_Length_m == 1000 & Start_m < 1000 ~ 800,
                                                     TRUE ~ Start_Station_m)) %>%
    dplyr::mutate(Start_Station_m = dplyr::case_when(Tran_Length_m < 500 & Start_m < 50 ~ 0,
                                                     Tran_Length_m < 500 & Start_m < 100 ~ 50,
                                                     Tran_Length_m < 500 & Start_m < 150 ~ 100,
                                                     Tran_Length_m < 500 & Start_m < 200 ~ 150,
                                                     Tran_Length_m < 500 & Start_m < 250 ~ 200,
                                                     Tran_Length_m < 500 & Start_m < 300 ~ 250,
                                                     Tran_Length_m < 500 & Start_m < 350 ~ 300,
                                                     Tran_Length_m < 500 & Start_m < 400 ~ 350,
                                                     Tran_Length_m < 500 & Start_m < 450 ~ 400,
                                                     Tran_Length_m < 500 & Start_m < 500 ~ 450,
                                                     TRUE ~ Start_Station_m)) %>%
    dplyr::mutate(End_Station_m = dplyr::case_when(Tran_Length_m == 500 ~ Start_Station_m + 100,
                                                   Tran_Length_m == 1000 ~ Start_Station_m + 200,
                                                   Tran_Length_m < 500 ~ Start_Station_m + 50)) %>%
    dplyr::mutate(Segs_Per_Station = (End_Station_m-Start_Station_m)/Seg_Length_m) %>%
    dplyr::mutate(Meters_Per_Station = Segs_Per_Station*Seg_Length_m)

  chk_stations <- EIPS3 %>%
    dplyr::select(Community, Sampling_Frame, Seg_Length_m, Tran_Length_m, Start_Station_m, End_Station_m) %>%
    dplyr::distinct()

  # Change Cover Class to low and high percentage (Cov_Range_Min, Cov_Range_Max)
  EIPS4 <- EIPS3 %>%
    v_EIPS_cover_class2percent(cover_column = Cover_Class)

  chk_1sp_per_seg <- EIPS4 %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year,
             Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
             Seg_Length_m, Segs_Per_Station, Meters_Per_Station, Segment,
             Code, Scientific_Name, Life_Form, Nativity) %>%
    dplyr::summarize(Tot_Seg_Cover_Min = sum(Cov_Range_Min),
              Tot_Seg_Cover_Max = sum(Cov_Range_Max),
              Tot_Seg_Richness = sum(!is.na(Code)), .groups = "drop")

  dups <- chk_1sp_per_seg %>%
    dplyr::filter(Tot_Seg_Richness > 1) %>%
    dplyr::select(Unit_Code, Sampling_Frame, Cycle, Year, Transect_Number, Segment,
                  Scientific_Name, Code, Dups = Tot_Seg_Richness)

  if(nrow(dups) > 0) {
    warning(paste(nrow(dups), "duplicate species present within data, see list above", sep = " "))
    message(paste0(capture.output(dups), collapse = "\n"))
  }

  return(EIPS4)
}


#' Map EIPS data
#' @description Input a dataframe, metric, and a column to be mapped, returns a map using
#' the column specified to color the metric.
#' @return leaflet map with crosstalk selection
#' @export
#' @examples
#' \dontrun{
#' v_EIPS_map()
#' v_EIPS_map()
#'}
v_EIPS_map_interstation <- function(.data, parameter, change = FALSE, agol_sample_frame = "*") {

  if (parameter == "Mean_Species_Cover") {
    station_summary <- .data %>%
      # Mean Species Cover by inter-station
      dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year,
               Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
               Seg_Length_m, Segs_Per_Station, Meters_Per_Station,
               Code, Scientific_Name, Life_Form, Nativity) %>%
      dplyr::summarize(Actual_Segs = dplyr::n_distinct(Segment),
                Tot_Station_Cov_Min = sum(Cov_Range_Min),
                Tot_Station_Cov_Max = sum(Cov_Range_Max)) %>%
      dplyr::mutate(Actual_Meters = Actual_Segs * Seg_Length_m,
             Mean_Seg_Cov_Min = Tot_Station_Cov_Min/Actual_Segs,
             Mean_Seg_Cov_Max = Tot_Station_Cov_Max/Actual_Segs)
  }

  if (parameter == "Max_Richness" | parameter == "Mean_Total_Cover") {
    station_summary <- .data %>%
      # Richness & Total Cover by Segment
        dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station, Segment) %>%
        dplyr::summarize(Tot_Seg_Cover_Min = sum(Cov_Range_Min),
                  Tot_Seg_Cover_Max = sum(Cov_Range_Max),
                  Tot_Seg_Richness = sum(!is.na(Code))) %>%
        dplyr::ungroup()

      # Mean Total Cover (Max Richness) by inter-station
    station_summary <- station_summary %>%
        dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station) %>%
        dplyr::summarize(Actual_Segs = dplyr::n_distinct(Segment),
                  Max_Seg_Richness = max(Tot_Seg_Richness),
                  Tot_Station_Cov_Min = sum(Tot_Seg_Cover_Min),
                  Tot_Station_Cov_Max = sum(Tot_Seg_Cover_Max)) %>%
        dplyr::mutate(Actual_Meters = Actual_Segs * Seg_Length_m,
               Mean_Seg_Cov_Min = Tot_Station_Cov_Min/Actual_Segs,
               Mean_Seg_Cov_Max = Tot_Station_Cov_Max/Actual_Segs)

  }

  # Remove stations without at least half of the segments present
  # Example: if station normally every 10 segments and inter-station only has
  # 4 segments, it gets dropped.
  discard <- station_summary %>%
    dplyr::filter(Actual_Meters < Meters_Per_Station/2)

  station_summary <- dplyr::anti_join(station_summary, discard)

  # Get Station Coordinates
  EIPS_pts <- FilterPACNVeg(data_name = "EIPS_image_pts",
                            is_qa_plot = FALSE)


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

  missing_lat_long <- station_summary2 %>%
    dplyr::filter(if_any(c(Lat_Start, Lat_End, Long_Start, Long_End), ~is.na(.))) %>%
    dplyr::ungroup() %>%
    dplyr::select(Sampling_Frame, Year, Transect_Number, Start_Station_m,
                  End_Station_m, Lat_Start, Long_Start, Lat_End, Long_End) %>%
    dplyr::distinct()

  if(nrow(missing_lat_long) > 0) {
    message(paste(nrow(missing_lat_long), "stations missing lat/long coordinates", sep = " "))
    message(paste0(capture.output(missing_lat_long), collapse = "\n"))
  }

  station_summary2 <- station_summary2 %>%
    dplyr::ungroup() %>%
    # remove records with no spatial data
    dplyr::anti_join(missing_lat_long) %>%
    # convert mean cover to cover class
    v_EIPS_cover_percent2class(range_column = Mean_Seg_Cov_Max, cover_column_name = "Mean_MaxCover") %>%
    # add text to describe cover range
    dplyr::mutate(Txt_Range = paste0(round(Mean_Seg_Cov_Min*100), "-", round(Mean_Seg_Cov_Max*100), "% Cover")) %>%
    # add tran_seg_id for spatial mapping
    dplyr::mutate(tran_seg_id = paste(Sampling_Frame, Cycle, Transect_Number, Start_Station_m, sep = "_"))

  if (parameter == "Max_Richness" | parameter == "Mean_Total_Cover") {
    station_summary2 <- station_summary2 %>%
      # conver richness to richness class
      dplyr::relocate(Max_Seg_Richness, .after = last_col()) %>%
      dplyr::mutate(Richness_Class = dplyr::case_when(Max_Seg_Richness == 0 ~ "0",
                                                      Max_Seg_Richness == 1 ~ "1",
                                                      Max_Seg_Richness >= 2 & Max_Seg_Richness <= 3 ~ "2-3",
                                                      Max_Seg_Richness >= 4 & Max_Seg_Richness <= 5 ~ "4-5",
                                                      Max_Seg_Richness >= 6 & Max_Seg_Richness <= 8 ~ "6-8",
                                                      Max_Seg_Richness >= 9 & Max_Seg_Richness <= 14 ~ "9-14",
                                                      Max_Seg_Richness >= 15 ~ "15+")) %>%
      dplyr::mutate(col_Richness = dplyr::case_when(Richness_Class == "0" ~ "#FFFFFF",
                                                    Richness_Class == "1" ~ "#FFFFCC",
                                                    Richness_Class == "2-3" ~ "#C7E9B4",
                                                    Richness_Class == "4-5" ~ "#7FCDBB",
                                                    Richness_Class == "6-8" ~ "#41B6C4",
                                                    Richness_Class == "9-14" ~ "#2C7FB8",
                                                    Richness_Class == "15+" ~ "#253494"))
  }

  # If Change selected:
  if (change == TRUE) {

    if(parameter == "Max_Richness" | parameter == "Mean_Total_Cover") {
      grp_vars <- c("Unit_Code", "Community", "Sampling_Frame", "Transect_Number",
                    "Start_Station_m", "End_Station_m", "Seg_Length_m", "Segs_Per_Station",
                    "Meters_Per_Station")
    }

    if(parameter == "Mean_Species_Cover") {
      grp_vars <- c("Unit_Code", "Community", "Sampling_Frame", "Transect_Number",
                    "Start_Station_m", "End_Station_m", "Seg_Length_m", "Segs_Per_Station",
                    "Meters_Per_Station",
                    "Code", "Scientific_Name", "Life_Form", "Nativity")
    }

    # Get list of cycle/year by sampling frame:
    sf_cycle_year <- station_summary2 %>%
      dplyr::select(Sampling_Frame, Cycle, Year) %>%
      dplyr::distinct()

    sf_cycle <- station_summary2 %>%
      dplyr::select(Sampling_Frame, Cycle) %>%
      dplyr::distinct()

    station_summary2 <- station_summary2 %>%
      dplyr::filter(Transect_Type == "Fixed") %>%
      dplyr::ungroup() %>%
      # if species recorded one year than any year it was not recorded,
      # it will show up as zero cover:
      tidyr::complete(tidyr::nesting(!!!syms(grp_vars)),
                      tidyr::nesting(Cycle),
                      fill = list(Mean_Seg_Cov_Max = 0, Mean_Seg_Cov_Min = 0)) %>%
      dplyr::semi_join(sf_cycle) %>%
      dplyr::left_join(sf_cycle_year, by = c("Sampling_Frame", "Cycle")) %>%
      dplyr::mutate(Year = dplyr::coalesce(Year.x, Year.y)) %>%
      dplyr::select(-Year.x, -Year.y) %>%
      # group by grouping variable specified by parameter above:
      dplyr::group_by(across(grp_vars)) %>%
      dplyr::arrange(Cycle, Year, .by_group = TRUE) %>%
      # Calculate the change in cover per cycle
      dplyr::mutate(Chg_Prior = Mean_Seg_Cov_Max - dplyr::lag(Mean_Seg_Cov_Max, order_by = Cycle)) %>%
      tidyr::fill(c(Long_Start, Long_End, Lat_Start, Lat_End), .direction = "updown") %>%
      # Add Chg_Class & col_Chg (could add this to v_EIPS_cover_percent2class?)
      dplyr::mutate(Chg_Class = dplyr::case_when(Chg_Prior <= -0.75 ~ "-[75+]",
                                                 Chg_Prior <= -0.50 & Chg_Prior > -0.75 ~ "-[50-75]",
                                                 Chg_Prior <= -0.25 & Chg_Prior > -0.50 ~ "-[25-50]",
                                                 Chg_Prior <= -0.10 & Chg_Prior > -0.25 ~ "-[10-25]",
                                                 Chg_Prior <= -0.05 & Chg_Prior > -0.10 ~ "-[5-10]",
                                                 Chg_Prior <= -0.01 & Chg_Prior > -0.05 ~ "-[1-5]",
                                                 Chg_Prior < 0 & Chg_Prior > -0.01 ~ "-[0-1]",
                                                 Chg_Prior == 0 ~ "0",
                                                 Chg_Prior > 0 & Chg_Prior < 0.01 ~ "0-1",
                                                 Chg_Prior >= 0.01 & Chg_Prior < 0.05 ~ "1-5",
                                                 Chg_Prior >= 0.05 & Chg_Prior < 0.10 ~ "5-10",
                                                 Chg_Prior >= 0.10 & Chg_Prior < 0.25 ~ "10-25",
                                                 Chg_Prior >= 0.25 & Chg_Prior < 0.50 ~ "25-50",
                                                 Chg_Prior >= 0.50 & Chg_Prior < 0.75 ~ "50-75",
                                                 Chg_Prior >= 0.75 ~ "75+")) %>%
      dplyr::mutate(col_Chg = dplyr::case_when(Chg_Class == "-[75+]" ~ "#005A32",
                                               Chg_Class == "-[50-75]" ~ "#238B45",
                                               Chg_Class == "-[25-50]" ~ "#41AB5D",
                                               Chg_Class == "-[10-25]" ~ "#74C476",
                                               Chg_Class == "-[5-10]" ~ "#A1D99B",
                                               Chg_Class == "-[1-5]" ~ "#C7E9C0",
                                               Chg_Class == "-[0-1]" ~ "#EDF8E9",
                                               Chg_Class == "0" ~ "#FFFFFF",
                                               Chg_Class == "0-1" ~ "#FFFFB2",
                                               Chg_Class == "1-5" ~ "#FED976",
                                               Chg_Class == "5-10" ~ "#FEB24C",
                                               Chg_Class == "10-25" ~ "#FD8D3C",
                                               Chg_Class == "25-50" ~ "#FC4E2A",
                                               Chg_Class == "50-75" ~ "#E31A1C",
                                               Chg_Class == "75-100+" ~ "#B10026")) %>%
      dplyr::mutate(Txt_Range = paste0(round(Mean_Seg_Cov_Min*100), "-", round(Mean_Seg_Cov_Max*100), "% Cover")) %>%
      dplyr::mutate(tran_seg_id = paste(Sampling_Frame, Cycle, Transect_Number, Start_Station_m, sep = "_")) %>%
      #dplyr::mutate(Year = as.numeric(as.character(Year))) %>%
      #dplyr::mutate(Years_Prior = Year - dplyr::lag(Year, order_by = Cycle)) %>%
      #dplyr::mutate(Year = as.factor(Year)) %>%
      #dplyr::mutate(Chg_Per_Year = Chg_Prior / Years_Prior) %>%
      #dplyr::mutate(!!max_cycle_lable := Cover - dplyr::lag(Cover, order_by = Cycle,
      #n = max_cycle-1)) %>%
      dplyr::ungroup()
  }

  # Pivot data longer so each segment has a row for start lat/long and a separate
  # row for end lat/long -
  # this is so table can be read by simple features function sf_multilinestring()
  station_summary3 <- station_summary2 %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number,
             Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station) %>%
    tidyr::pivot_longer(cols = c("Lat_Start", "Lat_End"), names_to = "S_E", names_prefix = "Lat_", values_to = "Lat") %>%
    tidyr::pivot_longer(cols = c("Long_Start", "Long_End"), names_to = "long_S_E", names_prefix = "Long_",values_to = "Long") %>%
    dplyr::filter(S_E == long_S_E) %>%
    dplyr::mutate(tran_id = paste(Sampling_Frame, Cycle, Transect_Number, sep = "_")) %>%
    dplyr::mutate(tran_seg_id = paste(Sampling_Frame, Cycle, Transect_Number, Start_Station_m, sep = "_")) %>%
    dplyr::arrange(tran_seg_id)

  # Use sfheaders package to prep dataset and use with simple features (sf)
  EIPS_sf <- sfheaders::sf_multilinestring(
    obj = station_summary3,
    x = "Long",
    y = "Lat",
    multilinestring_id = "tran_seg_id",
    keep = T
  )

  # Change from line segments to centroids (because cannot color individual
  # lines in leaflet currently):
  EIPS_sf_centroids <- EIPS_sf %>%
    dplyr::group_by(tran_seg_id) %>%
    dplyr::summarize(geometry = sf::st_union(geometry)) %>%
    sf::st_centroid()

  # Take centroids and replace as only spatial feature
  EIPS_inter_station <- station_summary2 %>%
    dplyr::select(-Lat_Start, -Lat_End, -Long_Start, -Long_End) %>%
    dplyr::left_join(EIPS_sf_centroids) %>%
    dplyr::mutate(long = unlist(purrr::map(geometry,1)),
           lat = unlist(purrr::map(geometry,2)))

  # Create Null data that can be mapped to show all sites sampled
  all_sites <- EIPS_inter_station %>%
    dplyr::ungroup() %>%
    dplyr::distinct(Cycle, Year, Unit_Code, Community, Sampling_Frame,
                    Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
                    Seg_Length_m, Segs_Per_Station, Meters_Per_Station,
                    tran_seg_id, lat, long) %>%
    dplyr::mutate(Scientific_Name = "All Sites",
                  Code = "All Sites",
                  Mean_MaxCover = 0,
                  col_Mean_MaxCover = "#FFFFFF")

  #EIPS_inter_station <- dplyr::bind_rows(all_sites, EIPS_inter_station) %>%
  #  dplyr::mutate(Monitoring_Sites = dplyr::case_when(Code == "All Sites" ~ TRUE,
  #                                                    TRUE ~ FALSE))

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

  # NPS tiles
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

  # Set up cross-talk shared object
  sd_cover <- crosstalk::SharedData$new(EIPS_inter_station)
  sd_all_sites <- crosstalk::SharedData$new(all_sites)

  # Species Cover (no change)----
  if (parameter == "Mean_Species_Cover" & change == FALSE) {

    Palette <- c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41")
    Levels <- as.factor(c(0,1,2,3,4,5,6,7))
    Labels <- c("0","<1", "1 - 5","5 - 10","10 - 25", "25 - 50", "50 - 75", "75+")
    custom_leg <- data.frame(Palette, Levels, Labels)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
           list(
             crosstalk::filter_checkbox("cover data", "Cover Data", sd_cover, ~Year),
             crosstalk::filter_checkbox("monitoring sites", "Monitoring Sites", sd_all_sites, ~Year),
             crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = FALSE)
             ),
           leaflet::leaflet(width = "100%", height = 700) %>%
             leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
             leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
             leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
             leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
             leaflet.esri::addEsriFeatureLayer(group = "Sampling Frame",
                                               options = leaflet.esri::featureLayerOptions(where = paste0("Sampling_Frame = '", agol_sample_frame, "'")),
                                               url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0",
                                               useServiceSymbology = TRUE,
                                               labelProperty = "Sampling_Frame") %>%
             leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                       overlayGroups = c("Sampling Frame", "Absence", "Non-native Cover"),
                                       options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
             leaflet::addCircleMarkers(data = sd_all_sites,
                                       group = "Absence",
                                       color = "#FFFFFF",
                                       stroke = FALSE,
                                       radius = 6,
                                       fillOpacity = 1,
                                       popup = ~paste(sep = "<br/>",
                                                      paste("Transect", Transect_Number)
                                                      )) %>%
             leaflet::addCircleMarkers(data = sd_cover,
                                       group = "Non-native Cover",
                                       color = ~col_Mean_MaxCover,
                              stroke = FALSE,
                              radius = 6,
                              fillOpacity = 1,
                              popup = ~paste(sep = "<br/>",
                                             paste("Transect", Transect_Number),
                                             Scientific_Name,
                                             Txt_Range)) %>%
             leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                       title = "Cover (%)",
                       opacity = 1))

  }

  # Total Cover (no change) ----
  if (parameter == "Mean_Total_Cover" & change == FALSE) {

    Palette <- c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41")
    Levels <- as.factor(c(0,1,2,3,4,5,6,7))
    Labels <- c("0","<1", "1 - 5","5 - 10","10 - 25", "25 - 50", "50 - 75", "75+")
    custom_leg <- data.frame(Palette, Levels, Labels)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_checkbox("year", "Year", sd_cover, ~Year)#,
                               #crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = TRUE)
                             ),
                             leaflet::leaflet(sd_cover, width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet.esri::addEsriFeatureLayer(group = "Sampling Frame",
                                                                 options = leaflet.esri::featureLayerOptions(where = paste0("Sampling_Frame = '", agol_sample_frame, "'")),
                                                                 url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0",
                                                                 useServiceSymbology = TRUE,
                                                                 labelProperty = "Sampling_Frame") %>%
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Sampling Frame", "Invasive Plant Transects (l)"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(group = "Invasive Plant Transects (l)",
                                                         color = ~col_Mean_MaxCover,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        paste("Transect", Transect_Number),
                                                                        #Scientific_Name,
                                                                        Txt_Range)) %>%
                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Cover (%)",
                                                  opacity = 1))

  }
  #  Richness (no change) ----
  if (parameter == "Max_Richness" & change == FALSE) {
    # Richness

    Palette <- c("#FFFFFF", "#FFFFCC", "#C7E9B4", "#7FCDBB", "#41B6C4", "#2C7FB8", "#253494")
    Labels <- c("0", "1", "2-3", "4-5", "6-8", "9-14", "15+")
    custom_leg <- data.frame(Labels, Palette)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_select("year", "Year", sd_cover, ~Year, multiple = FALSE)#,
                               #crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = FALSE)
                               ),
                             leaflet::leaflet(sd_cover, width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet.esri::addEsriFeatureLayer(group = "Sampling Frame",
                                                                 options = leaflet.esri::featureLayerOptions(where = paste0("Sampling_Frame = '", agol_sample_frame, "'")),
                                                                 url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0",
                                                                 useServiceSymbology = TRUE,
                                                                 labelProperty = "Sampling_Frame") %>%
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Sampling Frame"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(color = ~col_Richness,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        paste("Transect", Transect_Number),
                                                                        #Scientific_Name,
                                                                        paste("Max non-native species: ", Max_Seg_Richness),
                                                                        paste0("Across ",Segs_Per_Station, " Segments"),
                                                                        paste0("Each segment = 5x", Seg_Length_m, " meters")
                                                                        )) %>%
                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Non-Native Species",
                                                  opacity = 1))

  }

  # Change in Cover ----
  if (parameter == "Mean_Species_Cover" & change == TRUE |
      parameter == "Mean_Total_Cover" & change == TRUE) {

    Palette <- c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#EDF8E9",
                 "#FFFFFF", "#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026")
    Labels <- c("- [75+]","-[50-75]", "-[25-50]", "-[10-25]", "-[5-10]", "-[1-5]", "-[0-1]",
                "0", "0-1", "1-5", "5-10", "10-25", "25-50", "50-75", "75+")
    custom_leg <- data.frame(Labels, Palette)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_select("year", "Year", sd_cover, ~Year, multiple = FALSE),
                               crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = FALSE)
                             ),
                             leaflet::leaflet(sd_cover, width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet.esri::addEsriFeatureLayer(group = "Sampling Frame",
                                                                 options = leaflet.esri::featureLayerOptions(where = paste0("Sampling_Frame = '", agol_sample_frame, "'")),
                                                                 url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0",
                                                                 useServiceSymbology = TRUE,
                                                                 labelProperty = "Sampling_Frame") %>%
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Invasive Plant Transects (f)", "Sampling Frame"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(group = "Invasive Plant Transects (f)",
                                                         color = ~col_Chg,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        paste("Transect", Transect_Number),
                                                                        Scientific_Name,
                                                                        Txt_Range)) %>%
                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Cover Change",
                                                  opacity = 1))

  }


  return(map)


}


#' Plot species cover x frequency for all species (facet by species)
#'
#' @details X axis is Cover Classes, y axis is frequency along transect
#'
#' @param sample_frame EIPS sampling frame name
#' @param transect_number transect number (must be only one transect)
#' @param save_folder folder where .png plot will be saved
#'
#' @return graph of species found in transect across all cycles.
#' Optionally save individual .png file (if save_folder specified).
#' @export
#'
#' @examples
#' \dontrun{
#' qc_EIPS_cover_x_freq(sample_frame =  "Mauna Loa", transect_number = 3, save_folder = "C:/Users/JJGross/OneDrive/Downloads")
#' }
EIPS_cover_x_freq <- function(sample_frame, transect_number, save_folder) {

  eips_data <- pacnvegetation::FilterPACNVeg(data_name = "EIPS_data", sample_frame = sample_frame) |>
    dplyr::filter(Transect_Number == transect_number)

  eips_data_check <- pacnvegetation::v_EIPS_prep(sample_frame = sample_frame)|>
    dplyr::filter(Transect_Number == transect_number) |>
    dplyr::mutate(Seg_Position = paste0(Start_m, "-", End_m))|>
    dplyr::group_by(Cycle) |>
    dplyr::mutate(transect_segs_calc = n_distinct(Segment))

  segs <- eips_data_check |>
    dplyr::select(Sampling_Frame, Cycle, Transect_Number, transect_segs_calc) |>
    dplyr::distinct()


  if (any(eips_data_check$No_Data)) {

    missing_segs <- eips_data_check[eips_data_check$No_Data,]

    warning(paste0("Segment(s) missing data: ",
                   eips_data_check$Seg_Position[eips_data_check$No_Data == "TRUE"]))

    segs$transect_segs_calc <- segs$transect_segs_calc - nrow(missing_segs)

  }

  print(segs)
  message(paste0("Cycle ", segs$Cycle, " total segments used in frequency calculation = ", segs$transect_segs_calc, "; "))

  # Will need to make segs_per_tran calculation more robust
  eips_cover_freq <- eips_data |>
    dplyr::filter(Transect_Number == transect_number) |>
    dplyr::select(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Dead, Code, Segment, Cover_Class) |>
    dplyr::distinct() |>
    dplyr::mutate(segs_pres = 1) |>
    dplyr::group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Dead, Code, Cover_Class) |>
    dplyr::summarize(segs_per_tran = sum(segs_pres), .groups = "drop") #|>
    #dplyr::mutate(Cover_Class = dplyr::case_when(Cover_Class == "OUT" ~ "0",
    #                                             .default = as.character(Cover_Class)))

  gCC <- eips_cover_freq |>
    dplyr::group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead) |>
    dplyr::filter(Cover_Class == "CC") |>
    dplyr::mutate(`CC` = sum(segs_per_tran))|>
    dplyr::select(-Cover_Class, -segs_per_tran)|>
    dplyr::distinct()

  OUT <- eips_cover_freq |>
    dplyr::group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead) |>
    dplyr::filter(Cover_Class == "OUT") |>
    dplyr::mutate(OUT = sum(segs_per_tran))|>
    dplyr::select(-Cover_Class, -segs_per_tran) |>
    dplyr::distinct()

  eips_cover_freq_numbers_only <- eips_cover_freq |>
    dplyr::filter(Cover_Class != "CC" & Cover_Class != "OUT")

  g0p <- eips_cover_freq_numbers_only |>
    dplyr::group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead) |>
    dplyr::filter(Cover_Class >= 1) |>
    dplyr::mutate(`0` = sum(segs_per_tran))|>
    dplyr::select(-Cover_Class, -segs_per_tran) |>
    dplyr::distinct()

  g1p <- eips_cover_freq_numbers_only |>
    dplyr::group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead) |>
    dplyr::filter(Cover_Class >= 2) |>
    dplyr::mutate(`1` = sum(segs_per_tran))|>
    dplyr::select(-Cover_Class, -segs_per_tran)|>
    dplyr::distinct()

  g5p <- eips_cover_freq_numbers_only |>
    dplyr::group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead) |>
    dplyr::filter(Cover_Class >= 3) |>
    dplyr::mutate(`5` = sum(segs_per_tran))|>
    dplyr::select(-Cover_Class, -segs_per_tran)|>
    dplyr::distinct()

  g10p <- eips_cover_freq_numbers_only |>
    dplyr::group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead) |>
    dplyr::filter(Cover_Class >= 4) |>
    dplyr::mutate(`10` = sum(segs_per_tran))|>
    dplyr::select(-Cover_Class, -segs_per_tran)|>
    dplyr::distinct()

  g25p <- eips_cover_freq_numbers_only |>
    dplyr::group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead) |>
    dplyr::filter(Cover_Class >= 5) |>
    dplyr::mutate(`25` = sum(segs_per_tran))|>
    dplyr::select(-Cover_Class, -segs_per_tran)|>
    dplyr::distinct()

  g50p <- eips_cover_freq_numbers_only |>
    dplyr::group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead) |>
    dplyr::filter(Cover_Class >= 6) |>
    dplyr::mutate(`50` = sum(segs_per_tran))|>
    dplyr::select(-Cover_Class, -segs_per_tran)|>
    dplyr::distinct()

  g75p <- eips_cover_freq_numbers_only |>
    dplyr::group_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead) |>
    dplyr::filter(Cover_Class >= 7) |>
    dplyr::mutate(`75` = sum(segs_per_tran))|>
    dplyr::select(-Cover_Class, -segs_per_tran)|>
    dplyr::distinct()

  eips_add_cover <- eips_cover_freq %>%
    dplyr::ungroup() |>
    select(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead) |>
    dplyr::distinct() |>
    dplyr::left_join(OUT, by = dplyr::join_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead)) |>
    dplyr::left_join(g0p, by = dplyr::join_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead)) |>
    dplyr::left_join(g1p, by = dplyr::join_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead)) |>
    dplyr::left_join(g5p, by = dplyr::join_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead)) |>
    dplyr::left_join(g10p, by = dplyr::join_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead)) |>
    dplyr::left_join(g25p, by = dplyr::join_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead)) |>
    dplyr::left_join(g50p, by = dplyr::join_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead)) |>
    dplyr::left_join(g75p, by = dplyr::join_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead)) |>
    dplyr::left_join(gCC, by = dplyr::join_by(Sampling_Frame, Cycle, Transect_Number, Nativity, Scientific_Name, Code, Dead))

  final_table <- eips_add_cover |>
    pivot_longer(cols = `0`:`75`, names_to = "cover_greater_than", values_to = "segs") |>
    #pivot_longer(cols = c("OUT"), names_to = "cover_greater_than", values_to = "segs") |>
    dplyr::left_join(segs, by = join_by(Sampling_Frame, Cycle, Transect_Number)) |>
    dplyr::mutate(freq = segs/transect_segs_calc) |>
    dplyr::mutate(freq = case_when(!is.na(CC) ~ CC/transect_segs_calc,
                                   .default = freq)) |>
    dplyr::mutate(freq = case_when(!is.na(OUT) ~ OUT/transect_segs_calc,
                                   .default = freq)) |>
    dplyr::mutate(Scientific_Name = case_when(
      Dead == TRUE ~ paste(as.character(Scientific_Name), "(Dead)"),
      .default = as.character(Scientific_Name)))

  library(viridis)

  graph_out <- final_table |>
    dplyr::mutate(Cycle = as.factor(Cycle)) |>
    dplyr::mutate(cover_greater_than = as_factor(cover_greater_than)) |>
    ggplot2::ggplot(aes(x=cover_greater_than,
                        y= freq,
                        group = Cycle,
                        color = Cycle)) +
    ggplot2::geom_point(size = 4, data = subset(final_table, !is.na(OUT)), color = "green") +
    ggplot2::geom_line(linewidth = 3, data = subset(final_table, !is.na(OUT)), color = "green") +
    ggplot2::geom_point(size = 4, data = subset(final_table, !is.na(CC)), color = "dark gray") +
    ggplot2::geom_line(linewidth = 3, data = subset(final_table, !is.na(CC)), color = "dark gray") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x="% Cover", y=expression(paste("Frequency")))+
    ggplot2::ggtitle(paste(final_table$Sampling_Frame, "Transect", final_table$Transect_Number))+
    scale_color_viridis(discrete = TRUE, direction = -1) +
    ggplot2::theme(plot.title = ggplot2::element_text(size=14, face="bold", vjust=1, lineheight=0.8))+
    ggplot2::theme(axis.title.x=element_text(size=12, vjust=-0.2))+
    ggplot2::theme(axis.text.x=element_text(angle=0, size=11, vjust=0.5))+
    ggplot2::theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5))+
    ggplot2::scale_x_discrete(labels = function(x) paste0(">", x)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                                limits = c(0, 1),
                                breaks=seq(0,1,.2)) +
    facet_wrap(~Scientific_Name)

  plot_message <- ""

  if (any(!is.na(final_table$CC))) {
    plot_message <- paste0("gray highlight: shows frequency only (cover data is grouped within 'Alien Grass')")
    graph_out <- graph_out + ggplot2::labs(caption = plot_message)
  }

  if (any(!is.na(final_table$OUT))) {
    plot_message <- paste(plot_message, "green highlight: a species occurrence was noted outside transect boundary", sep = "\n")
    graph_out <- graph_out + ggplot2::labs(caption = plot_message)
  }


  if (!missing(save_folder)) {

    var_folder_sframe <- sample_frame

    if (var_sframe == "Nahuku/East Rift") {
      var_folder_sframe <- "Nahuku"
    }

    path_var <- paste0(save_folder, "/", var_folder_sframe)
    filename_var <- paste0("trans_", stringr::str_pad(transect_number, 2, pad = "0"), "_spp_cover_x_freq.png")
    print(paste(path_var, filename_var, sep = "/"))
    suppressWarnings(suppressMessages(
      ggplot2::ggsave(filename = filename_var, path = path_var, height = 10, width = 20)))

  } else {

    suppressWarnings(suppressMessages(
      print(graph_out)))
  }
}

#' Map EIPS data (updated 20250211)
#' @description Input a dataframe, metric, and a column to be mapped, returns a map using
#' the column specified to color the metric.
#' @return leaflet map with crosstalk selection
#' @export
#' @examples
#' \dontrun{
#' v_EIPS_map()
#' v_EIPS_map()
#'}
v_EIPS_map_interstation2 <- function(.data, parameter, change = FALSE, agol_sample_frame = "*") {

  if (parameter == "Mean_Species_Cover") {
    station_summary <- .data %>%
      # Mean Species Cover by inter-station
      dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year,
                      Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
                      Seg_Length_m, Segs_Per_Station, Meters_Per_Station,
                      Code, Scientific_Name, Life_Form, Nativity) %>%
      dplyr::summarize(Segs_Present = dplyr::n_distinct(Segment),
                       Tot_Station_Cov_Min = sum(Cov_Range_Min),
                       Tot_Station_Cov_Max = sum(Cov_Range_Max)) %>%
      dplyr::mutate(Meters_Present = Segs_Present * Seg_Length_m,
                    Mean_Seg_Cov_Min = Tot_Station_Cov_Min/Segs_Present,
                    Mean_Seg_Cov_Max = Tot_Station_Cov_Max/Segs_Present)
  }

  if (parameter == "Max_Richness" | parameter == "Mean_Total_Cover") {
    station_summary <- .data %>%
      # Richness & Total Cover by Segment
      dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station, Segment) %>%
      dplyr::summarize(Tot_Seg_Cover_Min = sum(Cov_Range_Min),
                       Tot_Seg_Cover_Max = sum(Cov_Range_Max),
                       Tot_Seg_Richness = sum(!is.na(Code))) %>%
      dplyr::ungroup()

    # Mean Total Cover (Max Richness) by inter-station
    station_summary <- station_summary %>%
      dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station) %>%
      dplyr::summarize(Segs_Present = dplyr::n_distinct(Segment),
                       Max_Seg_Richness = max(Tot_Seg_Richness),
                       Tot_Station_Cov_Min = sum(Tot_Seg_Cover_Min),
                       Tot_Station_Cov_Max = sum(Tot_Seg_Cover_Max)) %>%
      dplyr::mutate(Meters_Present = Segs_Present * Seg_Length_m,
                    Mean_Seg_Cov_Min = Tot_Station_Cov_Min/Segs_Present,
                    Mean_Seg_Cov_Max = Tot_Station_Cov_Max/Segs_Present)

  }

  # Get Station Coordinates
  EIPS_pts <- FilterPACNVeg(data_name = "EIPS_image_pts",
                            is_qa_plot = FALSE)


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

  missing_lat_long <- station_summary2 %>%
    dplyr::filter(if_any(c(Lat_Start, Lat_End, Long_Start, Long_End), ~is.na(.))) %>%
    dplyr::ungroup() %>%
    dplyr::select(Sampling_Frame, Year, Transect_Number, Start_Station_m,
                  End_Station_m, Lat_Start, Long_Start, Lat_End, Long_End) %>%
    dplyr::distinct()

  if(nrow(missing_lat_long) > 0) {
    message(paste(nrow(missing_lat_long), "stations missing lat/long coordinates", sep = " "))
    message(paste0(capture.output(missing_lat_long), collapse = "\n"))
  }

  station_summary2 <- station_summary2 %>%
    dplyr::ungroup() %>%
    # remove records with no spatial data
    dplyr::anti_join(missing_lat_long) %>%
    # convert mean cover to cover class
    v_EIPS_cover_percent2class(range_column = Mean_Seg_Cov_Max, cover_column_name = "Mean_MaxCover") %>%
    # add text to describe cover range
    dplyr::mutate(Txt_Range = paste0(round(Mean_Seg_Cov_Min*100), "-", round(Mean_Seg_Cov_Max*100), "% Cover")) %>%
    # add tran_seg_id for spatial mapping
    dplyr::mutate(tran_seg_id = paste(Sampling_Frame, Cycle, Transect_Number, Start_Station_m, sep = "_"))

  if (parameter == "Max_Richness" | parameter == "Mean_Total_Cover") {
    station_summary2 <- station_summary2 %>%
      # conver richness to richness class
      dplyr::relocate(Max_Seg_Richness, .after = last_col()) %>%
      dplyr::mutate(Richness_Class = dplyr::case_when(Max_Seg_Richness == 0 ~ "0",
                                                      Max_Seg_Richness == 1 ~ "1",
                                                      Max_Seg_Richness >= 2 & Max_Seg_Richness <= 3 ~ "2-3",
                                                      Max_Seg_Richness >= 4 & Max_Seg_Richness <= 5 ~ "4-5",
                                                      Max_Seg_Richness >= 6 & Max_Seg_Richness <= 8 ~ "6-8",
                                                      Max_Seg_Richness >= 9 & Max_Seg_Richness <= 14 ~ "9-14",
                                                      Max_Seg_Richness >= 15 ~ "15+")) %>%
      dplyr::mutate(col_Richness = dplyr::case_when(Richness_Class == "0" ~ "#FFFFFF",
                                                    Richness_Class == "1" ~ "#FFFFCC",
                                                    Richness_Class == "2-3" ~ "#C7E9B4",
                                                    Richness_Class == "4-5" ~ "#7FCDBB",
                                                    Richness_Class == "6-8" ~ "#41B6C4",
                                                    Richness_Class == "9-14" ~ "#2C7FB8",
                                                    Richness_Class == "15+" ~ "#253494"))
  }

  # If Change selected:
  if (change == TRUE) {

    if(parameter == "Max_Richness" | parameter == "Mean_Total_Cover") {
      grp_vars <- c("Unit_Code", "Community", "Sampling_Frame", "Transect_Number",
                    "Start_Station_m", "End_Station_m", "Seg_Length_m", "Segs_Per_Station",
                    "Meters_Per_Station")
    }

    if(parameter == "Mean_Species_Cover") {
      grp_vars <- c("Unit_Code", "Community", "Sampling_Frame", "Transect_Number",
                    "Start_Station_m", "End_Station_m", "Seg_Length_m", "Segs_Per_Station",
                    "Meters_Per_Station",
                    "Code", "Scientific_Name", "Life_Form", "Nativity")
    }

    # Get list of cycle/year by sampling frame:
    sf_cycle_year <- station_summary2 %>%
      dplyr::select(Sampling_Frame, Cycle, Year) %>%
      dplyr::distinct()

    sf_cycle <- station_summary2 %>%
      dplyr::select(Sampling_Frame, Cycle) %>%
      dplyr::distinct()

    station_summary2 <- station_summary2 %>%
      dplyr::filter(Transect_Type == "Fixed") %>%
      dplyr::ungroup() %>%
      # if species recorded one year than any year it was not recorded,
      # it will show up as zero cover:
      tidyr::complete(tidyr::nesting(!!!syms(grp_vars)),
                      tidyr::nesting(Cycle),
                      fill = list(Mean_Seg_Cov_Max = 0, Mean_Seg_Cov_Min = 0)) %>%
      dplyr::semi_join(sf_cycle) %>%
      dplyr::left_join(sf_cycle_year, by = c("Sampling_Frame", "Cycle")) %>%
      dplyr::mutate(Year = dplyr::coalesce(Year.x, Year.y)) %>%
      dplyr::select(-Year.x, -Year.y) %>%
      # group by grouping variable specified by parameter above:
      dplyr::group_by(across(grp_vars)) %>%
      dplyr::arrange(Cycle, Year, .by_group = TRUE) %>%
      # Calculate the change in cover per cycle
      dplyr::mutate(Chg_Prior = Mean_Seg_Cov_Max - dplyr::lag(Mean_Seg_Cov_Max, order_by = Cycle)) %>%
      tidyr::fill(c(Long_Start, Long_End, Lat_Start, Lat_End), .direction = "updown") %>%
      # Add Chg_Class & col_Chg (could add this to v_EIPS_cover_percent2class?)
      dplyr::mutate(Chg_Class = dplyr::case_when(Chg_Prior <= -0.75 ~ "-[75+]",
                                                 Chg_Prior <= -0.50 & Chg_Prior > -0.75 ~ "-[50-75]",
                                                 Chg_Prior <= -0.25 & Chg_Prior > -0.50 ~ "-[25-50]",
                                                 Chg_Prior <= -0.10 & Chg_Prior > -0.25 ~ "-[10-25]",
                                                 Chg_Prior <= -0.05 & Chg_Prior > -0.10 ~ "-[5-10]",
                                                 Chg_Prior <= -0.01 & Chg_Prior > -0.05 ~ "-[1-5]",
                                                 Chg_Prior < 0 & Chg_Prior > -0.01 ~ "-[0-1]",
                                                 Chg_Prior == 0 ~ "0",
                                                 Chg_Prior > 0 & Chg_Prior < 0.01 ~ "0-1",
                                                 Chg_Prior >= 0.01 & Chg_Prior < 0.05 ~ "1-5",
                                                 Chg_Prior >= 0.05 & Chg_Prior < 0.10 ~ "5-10",
                                                 Chg_Prior >= 0.10 & Chg_Prior < 0.25 ~ "10-25",
                                                 Chg_Prior >= 0.25 & Chg_Prior < 0.50 ~ "25-50",
                                                 Chg_Prior >= 0.50 & Chg_Prior < 0.75 ~ "50-75",
                                                 Chg_Prior >= 0.75 ~ "75+")) %>%
      dplyr::mutate(col_Chg = dplyr::case_when(Chg_Class == "-[75+]" ~ "#005A32",
                                               Chg_Class == "-[50-75]" ~ "#238B45",
                                               Chg_Class == "-[25-50]" ~ "#41AB5D",
                                               Chg_Class == "-[10-25]" ~ "#74C476",
                                               Chg_Class == "-[5-10]" ~ "#A1D99B",
                                               Chg_Class == "-[1-5]" ~ "#C7E9C0",
                                               Chg_Class == "-[0-1]" ~ "#EDF8E9",
                                               Chg_Class == "0" ~ "#FFFFFF",
                                               Chg_Class == "0-1" ~ "#FFFFB2",
                                               Chg_Class == "1-5" ~ "#FED976",
                                               Chg_Class == "5-10" ~ "#FEB24C",
                                               Chg_Class == "10-25" ~ "#FD8D3C",
                                               Chg_Class == "25-50" ~ "#FC4E2A",
                                               Chg_Class == "50-75" ~ "#E31A1C",
                                               Chg_Class == "75-100+" ~ "#B10026")) %>%
      dplyr::mutate(Txt_Range = paste0(round(Mean_Seg_Cov_Min*100), "-", round(Mean_Seg_Cov_Max*100), "% Cover")) %>%
      dplyr::mutate(tran_seg_id = paste(Sampling_Frame, Cycle, Transect_Number, Start_Station_m, sep = "_")) %>%
      #dplyr::mutate(Year = as.numeric(as.character(Year))) %>%
      #dplyr::mutate(Years_Prior = Year - dplyr::lag(Year, order_by = Cycle)) %>%
      #dplyr::mutate(Year = as.factor(Year)) %>%
      #dplyr::mutate(Chg_Per_Year = Chg_Prior / Years_Prior) %>%
      #dplyr::mutate(!!max_cycle_lable := Cover - dplyr::lag(Cover, order_by = Cycle,
      #n = max_cycle-1)) %>%
      dplyr::ungroup()
  }

  # Pivot data longer so each segment has a row for start lat/long and a separate
  # row for end lat/long -
  # this is so table can be read by simple features function sf_multilinestring()
  station_summary3 <- station_summary2 %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number,
                    Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station) %>%
    tidyr::pivot_longer(cols = c("Lat_Start", "Lat_End"), names_to = "S_E", names_prefix = "Lat_", values_to = "Lat") %>%
    tidyr::pivot_longer(cols = c("Long_Start", "Long_End"), names_to = "long_S_E", names_prefix = "Long_",values_to = "Long") %>%
    dplyr::filter(S_E == long_S_E) %>%
    dplyr::mutate(tran_id = paste(Sampling_Frame, Cycle, Transect_Number, sep = "_")) %>%
    dplyr::mutate(tran_seg_id = paste(Sampling_Frame, Cycle, Transect_Number, Start_Station_m, sep = "_")) %>%
    dplyr::arrange(tran_seg_id)

  # Use sfheaders package to prep dataset and use with simple features (sf)
  EIPS_sf <- sfheaders::sf_multilinestring(
    obj = station_summary3,
    x = "Long",
    y = "Lat",
    multilinestring_id = "tran_seg_id",
    keep = T
  )

  # Change from line segments to centroids (because cannot color individual
  # lines in leaflet currently):
  EIPS_sf_centroids <- EIPS_sf %>%
    dplyr::group_by(tran_seg_id) %>%
    dplyr::summarize(geometry = sf::st_union(geometry)) %>%
    sf::st_centroid()

  # Take centroids and replace as only spatial feature
  EIPS_inter_station <- station_summary2 %>%
    dplyr::select(-Lat_Start, -Lat_End, -Long_Start, -Long_End) %>%
    dplyr::left_join(EIPS_sf_centroids) %>%
    dplyr::mutate(long = unlist(purrr::map(geometry,1)),
                  lat = unlist(purrr::map(geometry,2)))

  EIPS_inter_station_absence <- EIPS_inter_station |>
    dplyr::mutate(Year = as.numeric(as.character(Year))) |>
    #dplyr::mutate(Year = as.Date(ISOdate(as.character(Year), 1, 1))) |>
    dplyr::mutate(Code = replace_na(Code, "no_invasives")) |>
    dplyr::mutate(Scientific_Name = replace_na(Scientific_Name, "no_invasives")) |>
    dplyr::mutate(Life_Form = replace_na(Life_Form, "no_invasives")) |>
    dplyr::mutate(Nativity = replace_na(Nativity, "no_invasives")) |>
    complete(Scientific_Name, nesting(Transect_Number, Cycle, Year, long, lat)) |>
    dplyr::mutate(col_Mean_MaxCover = case_when(is.na(col_Mean_MaxCover) ~ "#FF000000",
                                                .default = as.character(col_Mean_MaxCover)))

  #Create Null data that can be mapped to show all sites sampled
  all_sites <- EIPS_inter_station %>%
    dplyr::ungroup() %>%
    dplyr::distinct(Cycle, Year, Unit_Code, Community, Sampling_Frame,
                    Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
                    Seg_Length_m, Segs_Per_Station, Meters_Per_Station,
                    tran_seg_id, lat, long) %>%
    dplyr::mutate(Scientific_Name = "All Sites",
                  Code = "All Sites",
                  Mean_MaxCover = 0,
                  col_Mean_MaxCover = "#FFFFFF")

  #EIPS_inter_station <- dplyr::bind_rows(all_sites, EIPS_inter_station) %>%
  #  dplyr::mutate(Monitoring_Sites = dplyr::case_when(Code == "All Sites" ~ TRUE,
  #                                                    TRUE ~ FALSE))

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

  # NPS tiles
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

  # PACN/GIS polygons:
  if (agol_sample_frame == "Mauna Loa") {
    agol_sample_frame <- "Subalpine Shrubland"
  }

  url <- httr::parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/")
  url$path <- paste(url$path, "PACN_Vegetation_Sampling_Frames_vlyr/FeatureServer/0/query", sep = "/")
  url$query <- list(where = paste0("Sampling_Frame = '", agol_sample_frame, "'"),
                    outFields = "*",
                    returnGeometry = "true",
                    f = "geojson")
  request <- httr::build_url(url)

  request #print url request


  # sf::st_read() will not silence message when AGOL layer not available
  # message is: "Re-reading with feature count reset from 1 to 0"
  # This is showing up in knitted html so I have to remove following function (highlighted with **)
  # until AGOL public layer works again.
  mgmt_unit <- dplyr::tibble()
  #**mgmt_unit <- sf::st_read(request, quiet = TRUE)*

  if (nrow(mgmt_unit) == 0) {
    # If agol request does not work (zero observation returned), then pull
    # sampling frame polygons from static file in Rmarkdown file (.Rmd) folder:
    gdb <- "spatial/PACN_Vegetation_Sampling_Frames_view/aae67c1a-113a-4c2a-beaa-16139b2d6f7a.gdb"
    gdb_layer <- "PACN_Vegetation_Sampling_Frames"
    mgmt_unit <- sf::read_sf(gdb, gdb_layer, quiet = TRUE)
    mgmt_unit <- sf::st_transform(mgmt_unit, crs = 4326)
    mgmt_unit <- mgmt_unit |>
      dplyr::filter(Sampling_Frame == agol_sample_frame)
  }

  # Colors for polygons:
  factpal <- leaflet::colorFactor(c("#F8573A", "#F4C47B", "#28468B", "#AED5CB"),
                                  mgmt_unit$Zone)


  # Set up cross-talk shared object
  #sd_cover <- crosstalk::SharedData$new(EIPS_inter_station)
  sd_cover <- crosstalk::SharedData$new(EIPS_inter_station_absence)
  sd_all_sites <- crosstalk::SharedData$new(EIPS_inter_station_absence)

  # Species Cover (no change)----
  if (parameter == "Mean_Species_Cover" & change == FALSE) {

    Palette <- c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41")
    Levels <- as.factor(c(0,1,2,3,4,5,6,7))
    Labels <- c("Out","<1", "1 - 5","5 - 10","10 - 25", "25 - 50", "50 - 75", "75+")
    custom_leg <- data.frame(Palette, Levels, Labels)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_slider("cover data", "Cover Data (Select Years)", sd_cover, ~Year, step = 5, ticks = FALSE, sep = ""),
                               crosstalk::filter_slider("sites", "Sites (Select Years)", sd_all_sites, ~Year, step = 5, sep = "", ticks = FALSE),
                               crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = FALSE)
                             ),
                             leaflet::leaflet(width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet::addPolygons(data = mgmt_unit, group = "Zone", color =  ~factpal(Zone), label = ~Zone) |>
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Zone", "Sites", "Non-native Cover"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(data = sd_all_sites,
                                                         group = "Sites",
                                                         color = "black",
                                                         weight = 3,
                                                         stroke = TRUE,
                                                         radius = 7,
                                                         fillOpacity = 0,
                                                         # popup = ~paste(sep = "<br/>",
                                                         #                paste("Transect", Transect_Number),
                                                         #                Scientific_Name,
                                                         #                Txt_Range)
                               ) %>%

                               leaflet::addCircleMarkers(data = sd_cover,
                                                         group = "Non-native Cover",
                                                         color = ~col_Mean_MaxCover,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        as.character(Year),
                                                                        paste("Transect",
                                                                              Transect_Number),
                                                                        Scientific_Name,
                                                                        Txt_Range)
                               ) %>%



                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Cover (%)",
                                                  opacity = 1))

  }

  # Total Cover (no change) ----
  if (parameter == "Mean_Total_Cover" & change == FALSE) {

    Palette <- c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41")
    Levels <- as.factor(c(0,1,2,3,4,5,6,7))
    Labels <- c("0","<1", "1 - 5","5 - 10","10 - 25", "25 - 50", "50 - 75", "75+")
    custom_leg <- data.frame(Palette, Levels, Labels)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_checkbox("year", "Year", sd_cover, ~Year)#,
                               #crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = TRUE)
                             ),
                             leaflet::leaflet(sd_cover, width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet::addPolygons(data = mgmt_unit, group = "Zone", color =  ~factpal(Zone), label = ~Zone) |>
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Sampling Frame", "Invasive Plant Transects (l)"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(group = "Invasive Plant Transects (l)",
                                                         color = ~col_Mean_MaxCover,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        paste("Transect", Transect_Number),
                                                                        #Scientific_Name,
                                                                        Txt_Range)) %>%
                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Cover (%)",
                                                  opacity = 1))

  }
  #  Richness (no change) ----
  if (parameter == "Max_Richness" & change == FALSE) {
    # Richness

    Palette <- c("#FFFFFF", "#FFFFCC", "#C7E9B4", "#7FCDBB", "#41B6C4", "#2C7FB8", "#253494")
    Labels <- c("0", "1", "2-3", "4-5", "6-8", "9-14", "15+")
    custom_leg <- data.frame(Labels, Palette)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_select("year", "Year", sd_cover, ~Year, multiple = FALSE)#,
                               #crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = FALSE)
                             ),
                             leaflet::leaflet(sd_cover, width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet::addPolygons(data = mgmt_unit, group = "Zone", color =  ~factpal(Zone), label = ~Zone) |>
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Zone"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(color = ~col_Richness,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        paste("Transect", Transect_Number),
                                                                        #Scientific_Name,
                                                                        paste("Max non-native species: ", Max_Seg_Richness),
                                                                        paste0("Across ",Segs_Per_Station, " Segments"),
                                                                        paste0("Each segment = 5x", Seg_Length_m, " meters")
                                                         )) %>%
                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Non-Native Species",
                                                  opacity = 1))

  }

  # Change in Cover ----
  if (parameter == "Mean_Species_Cover" & change == TRUE |
      parameter == "Mean_Total_Cover" & change == TRUE) {

    Palette <- c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#EDF8E9",
                 "#FFFFFF", "#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026")
    Labels <- c("- [75+]","-[50-75]", "-[25-50]", "-[10-25]", "-[5-10]", "-[1-5]", "-[0-1]",
                "0", "0-1", "1-5", "5-10", "10-25", "25-50", "50-75", "75+")
    custom_leg <- data.frame(Labels, Palette)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_select("year", "Year", sd_cover, ~Year, multiple = FALSE),
                               crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = FALSE)
                             ),
                             leaflet::leaflet(sd_cover, width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet::addPolygons(data = mgmt_unit, group = "Zone", color =  ~factpal(Zone), label = ~Zone) |>
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Invasive Plant Transects (f)", "Zone"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(group = "Invasive Plant Transects (f)",
                                                         color = ~col_Chg,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        paste("Transect", Transect_Number),
                                                                        Scientific_Name,
                                                                        Txt_Range)) %>%
                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Cover Change",
                                                  opacity = 1))

  }


  return(map)


}

#' Map EIPS data (updated 20250211)
#' @description Input a dataframe, metric, and a column to be mapped, returns a map using
#' the column specified to color the metric.
#' @return leaflet map with crosstalk selection
#' @export
#' @examples
#' \dontrun{
#' v_EIPS_map()
#' v_EIPS_map()
#'}
v_EIPS_map_interstation3 <- function(.data, parameter, change = FALSE, agol_sample_frame = "*") {

  if (parameter == "Mean_Species_Cover") {
    station_summary <- .data %>%
      # Mean Species Cover by inter-station
      dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year,
                      Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
                      Seg_Length_m, Segs_Per_Station, Meters_Per_Station,
                      Code, Scientific_Name, Life_Form, Nativity) %>%
      dplyr::summarize(Segs_Present = dplyr::n_distinct(Segment),
                       Tot_Station_Cov_Min = sum(Cov_Range_Min),
                       Tot_Station_Cov_Max = sum(Cov_Range_Max)) %>%
      dplyr::mutate(Meters_Present = Segs_Present * Seg_Length_m,
                    Mean_Seg_Cov_Min = Tot_Station_Cov_Min/Segs_Present,
                    Mean_Seg_Cov_Max = Tot_Station_Cov_Max/Segs_Present)
  }

  if (parameter == "Max_Richness" | parameter == "Mean_Total_Cover") {
    station_summary <- .data %>%
      # Richness & Total Cover by Segment
      dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station, Segment) %>%
      dplyr::summarize(Tot_Seg_Cover_Min = sum(Cov_Range_Min),
                       Tot_Seg_Cover_Max = sum(Cov_Range_Max),
                       Tot_Seg_Richness = sum(!is.na(Code))) %>%
      dplyr::ungroup()

    # Mean Total Cover (Max Richness) by inter-station
    station_summary <- station_summary %>%
      dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number, Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station) %>%
      dplyr::summarize(Segs_Present = dplyr::n_distinct(Segment),
                       Max_Seg_Richness = max(Tot_Seg_Richness),
                       Tot_Station_Cov_Min = sum(Tot_Seg_Cover_Min),
                       Tot_Station_Cov_Max = sum(Tot_Seg_Cover_Max)) %>%
      dplyr::mutate(Meters_Present = Segs_Present * Seg_Length_m,
                    Mean_Seg_Cov_Min = Tot_Station_Cov_Min/Segs_Present,
                    Mean_Seg_Cov_Max = Tot_Station_Cov_Max/Segs_Present)

  }

  # Get Station Coordinates
  EIPS_pts <- FilterPACNVeg(data_name = "EIPS_image_pts",
                            is_qa_plot = FALSE)


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

  missing_lat_long <- station_summary2 %>%
    dplyr::filter(if_any(c(Lat_Start, Lat_End, Long_Start, Long_End), ~is.na(.))) %>%
    dplyr::ungroup() %>%
    dplyr::select(Sampling_Frame, Year, Transect_Number, Start_Station_m,
                  End_Station_m, Lat_Start, Long_Start, Lat_End, Long_End) %>%
    dplyr::distinct()

  if(nrow(missing_lat_long) > 0) {
    message(paste(nrow(missing_lat_long), "stations missing lat/long coordinates", sep = " "))
    message(paste0(capture.output(missing_lat_long), collapse = "\n"))
  }

  station_summary2 <- station_summary2 %>%
    dplyr::ungroup() %>%
    # remove records with no spatial data
    dplyr::anti_join(missing_lat_long) %>%
    # convert mean cover to cover class
    v_EIPS_cover_percent2class(range_column = Mean_Seg_Cov_Max, cover_column_name = "Mean_MaxCover") %>%
    # add text to describe cover range
    dplyr::mutate(Txt_Range = paste0(round(Mean_Seg_Cov_Min*100), "-", round(Mean_Seg_Cov_Max*100), "% Cover")) %>%
    # add tran_seg_id for spatial mapping
    dplyr::mutate(tran_seg_id = paste(Sampling_Frame, Cycle, Transect_Number, Start_Station_m, sep = "_"))

  if (parameter == "Max_Richness" | parameter == "Mean_Total_Cover") {
    station_summary2 <- station_summary2 %>%
      # conver richness to richness class
      dplyr::relocate(Max_Seg_Richness, .after = last_col()) %>%
      dplyr::mutate(Richness_Class = dplyr::case_when(Max_Seg_Richness == 0 ~ "0",
                                                      Max_Seg_Richness == 1 ~ "1",
                                                      Max_Seg_Richness >= 2 & Max_Seg_Richness <= 3 ~ "2-3",
                                                      Max_Seg_Richness >= 4 & Max_Seg_Richness <= 5 ~ "4-5",
                                                      Max_Seg_Richness >= 6 & Max_Seg_Richness <= 8 ~ "6-8",
                                                      Max_Seg_Richness >= 9 & Max_Seg_Richness <= 14 ~ "9-14",
                                                      Max_Seg_Richness >= 15 ~ "15+")) %>%
      dplyr::mutate(col_Richness = dplyr::case_when(Richness_Class == "0" ~ "#FFFFFF",
                                                    Richness_Class == "1" ~ "#FFFFCC",
                                                    Richness_Class == "2-3" ~ "#C7E9B4",
                                                    Richness_Class == "4-5" ~ "#7FCDBB",
                                                    Richness_Class == "6-8" ~ "#41B6C4",
                                                    Richness_Class == "9-14" ~ "#2C7FB8",
                                                    Richness_Class == "15+" ~ "#253494"))
  }

  # If Change selected:
  if (change == TRUE) {

    if(parameter == "Max_Richness" | parameter == "Mean_Total_Cover") {
      grp_vars <- c("Unit_Code", "Community", "Sampling_Frame", "Transect_Number",
                    "Start_Station_m", "End_Station_m", "Seg_Length_m", "Segs_Per_Station",
                    "Meters_Per_Station")
    }

    if(parameter == "Mean_Species_Cover") {
      grp_vars <- c("Unit_Code", "Community", "Sampling_Frame", "Transect_Number",
                    "Start_Station_m", "End_Station_m", "Seg_Length_m", "Segs_Per_Station",
                    "Meters_Per_Station",
                    "Code", "Scientific_Name", "Life_Form", "Nativity")
    }

    # Get list of cycle/year by sampling frame:
    sf_cycle_year <- station_summary2 %>%
      dplyr::select(Sampling_Frame, Cycle, Year) %>%
      dplyr::distinct()

    sf_cycle <- station_summary2 %>%
      dplyr::select(Sampling_Frame, Cycle) %>%
      dplyr::distinct()

    station_summary2 <- station_summary2 %>%
      dplyr::filter(Transect_Type == "Fixed") %>%
      dplyr::ungroup() %>%
      # if species recorded one year than any year it was not recorded,
      # it will show up as zero cover:
      tidyr::complete(tidyr::nesting(!!!syms(grp_vars)),
                      tidyr::nesting(Cycle),
                      fill = list(Mean_Seg_Cov_Max = 0, Mean_Seg_Cov_Min = 0)) %>%
      dplyr::semi_join(sf_cycle) %>%
      dplyr::left_join(sf_cycle_year, by = c("Sampling_Frame", "Cycle")) %>%
      dplyr::mutate(Year = dplyr::coalesce(Year.x, Year.y)) %>%
      dplyr::select(-Year.x, -Year.y) %>%
      # group by grouping variable specified by parameter above:
      dplyr::group_by(across(grp_vars)) %>%
      dplyr::arrange(Cycle, Year, .by_group = TRUE) %>%
      # Calculate the change in cover per cycle
      dplyr::mutate(Chg_Prior = Mean_Seg_Cov_Max - dplyr::lag(Mean_Seg_Cov_Max, order_by = Cycle)) %>%
      tidyr::fill(c(Long_Start, Long_End, Lat_Start, Lat_End), .direction = "updown") %>%
      # Add Chg_Class & col_Chg (could add this to v_EIPS_cover_percent2class?)
      dplyr::mutate(Chg_Class = dplyr::case_when(Chg_Prior <= -0.75 ~ "-[75+]",
                                                 Chg_Prior <= -0.50 & Chg_Prior > -0.75 ~ "-[50-75]",
                                                 Chg_Prior <= -0.25 & Chg_Prior > -0.50 ~ "-[25-50]",
                                                 Chg_Prior <= -0.10 & Chg_Prior > -0.25 ~ "-[10-25]",
                                                 Chg_Prior <= -0.05 & Chg_Prior > -0.10 ~ "-[5-10]",
                                                 Chg_Prior <= -0.01 & Chg_Prior > -0.05 ~ "-[1-5]",
                                                 Chg_Prior < 0 & Chg_Prior > -0.01 ~ "-[0-1]",
                                                 Chg_Prior == 0 ~ "0",
                                                 Chg_Prior > 0 & Chg_Prior < 0.01 ~ "0-1",
                                                 Chg_Prior >= 0.01 & Chg_Prior < 0.05 ~ "1-5",
                                                 Chg_Prior >= 0.05 & Chg_Prior < 0.10 ~ "5-10",
                                                 Chg_Prior >= 0.10 & Chg_Prior < 0.25 ~ "10-25",
                                                 Chg_Prior >= 0.25 & Chg_Prior < 0.50 ~ "25-50",
                                                 Chg_Prior >= 0.50 & Chg_Prior < 0.75 ~ "50-75",
                                                 Chg_Prior >= 0.75 ~ "75+")) %>%
      dplyr::mutate(col_Chg = dplyr::case_when(Chg_Class == "-[75+]" ~ "#005A32",
                                               Chg_Class == "-[50-75]" ~ "#238B45",
                                               Chg_Class == "-[25-50]" ~ "#41AB5D",
                                               Chg_Class == "-[10-25]" ~ "#74C476",
                                               Chg_Class == "-[5-10]" ~ "#A1D99B",
                                               Chg_Class == "-[1-5]" ~ "#C7E9C0",
                                               Chg_Class == "-[0-1]" ~ "#EDF8E9",
                                               Chg_Class == "0" ~ "#FFFFFF",
                                               Chg_Class == "0-1" ~ "#FFFFB2",
                                               Chg_Class == "1-5" ~ "#FED976",
                                               Chg_Class == "5-10" ~ "#FEB24C",
                                               Chg_Class == "10-25" ~ "#FD8D3C",
                                               Chg_Class == "25-50" ~ "#FC4E2A",
                                               Chg_Class == "50-75" ~ "#E31A1C",
                                               Chg_Class == "75-100+" ~ "#B10026")) %>%
      dplyr::mutate(Txt_Range = paste0(round(Mean_Seg_Cov_Min*100), "-", round(Mean_Seg_Cov_Max*100), "% Cover")) %>%
      dplyr::mutate(tran_seg_id = paste(Sampling_Frame, Cycle, Transect_Number, Start_Station_m, sep = "_")) %>%
      #dplyr::mutate(Year = as.numeric(as.character(Year))) %>%
      #dplyr::mutate(Years_Prior = Year - dplyr::lag(Year, order_by = Cycle)) %>%
      #dplyr::mutate(Year = as.factor(Year)) %>%
      #dplyr::mutate(Chg_Per_Year = Chg_Prior / Years_Prior) %>%
      #dplyr::mutate(!!max_cycle_lable := Cover - dplyr::lag(Cover, order_by = Cycle,
      #n = max_cycle-1)) %>%
      dplyr::ungroup()
  }

  # Pivot data longer so each segment has a row for start lat/long and a separate
  # row for end lat/long -
  # this is so table can be read by simple features function sf_multilinestring()
  station_summary3 <- station_summary2 %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year, Transect_Type, Transect_Number,
                    Start_Station_m, End_Station_m, Seg_Length_m, Segs_Per_Station, Meters_Per_Station) %>%
    tidyr::pivot_longer(cols = c("Lat_Start", "Lat_End"), names_to = "S_E", names_prefix = "Lat_", values_to = "Lat") %>%
    tidyr::pivot_longer(cols = c("Long_Start", "Long_End"), names_to = "long_S_E", names_prefix = "Long_",values_to = "Long") %>%
    dplyr::filter(S_E == long_S_E) %>%
    dplyr::mutate(tran_id = paste(Sampling_Frame, Cycle, Transect_Number, sep = "_")) %>%
    dplyr::mutate(tran_seg_id = paste(Sampling_Frame, Cycle, Transect_Number, Start_Station_m, sep = "_")) %>%
    dplyr::arrange(tran_seg_id)

  # Use sfheaders package to prep dataset and use with simple features (sf)
  EIPS_sf <- sfheaders::sf_multilinestring(
    obj = station_summary3,
    x = "Long",
    y = "Lat",
    multilinestring_id = "tran_seg_id",
    keep = T
  )

  # Change from line segments to centroids (because cannot color individual
  # lines in leaflet currently):
  EIPS_sf_centroids <- EIPS_sf %>%
    dplyr::group_by(tran_seg_id) %>%
    dplyr::summarize(geometry = sf::st_union(geometry)) %>%
    sf::st_centroid()

  # Take centroids and replace as only spatial feature
  station_summary_centroids <- station_summary2 %>%
    dplyr::select(-Lat_Start, -Lat_End, -Long_Start, -Long_End) %>%
    dplyr::left_join(EIPS_sf_centroids) %>%
    dplyr::mutate(long = unlist(purrr::map(geometry,1)),
                  lat = unlist(purrr::map(geometry,2)))

  if (parameter == "Mean_Species_Cover" & change == FALSE) {
    station_summary_centroids <- station_summary_centroids |>
      dplyr::mutate(Year = as.numeric(as.character(Year))) |>
      dplyr::mutate(Code = replace_na(Code, "no_invasives")) |>
      dplyr::mutate(Scientific_Name = replace_na(Scientific_Name, "no_invasives")) |>
      dplyr::mutate(Life_Form = replace_na(Life_Form, "no_invasives")) |>
      dplyr::mutate(Nativity = replace_na(Nativity, "no_invasives")) |>
      complete(Scientific_Name, nesting(Transect_Number, Cycle, Year, long, lat)) |>
      dplyr::mutate(col_Mean_MaxCover = case_when(is.na(col_Mean_MaxCover) ~ "#FF000000",
                                                  .default = as.character(col_Mean_MaxCover)))
  }

  station_summary_centroids <- station_summary_centroids |>
    dplyr::mutate(Year = as.numeric(as.character(Year))) |>
    dplyr::mutate(col_Mean_MaxCover = case_when(is.na(col_Mean_MaxCover) ~ "#FF000000",
                                                .default = as.character(col_Mean_MaxCover)))


  #Create Null data that can be mapped to show all sites sampled
  all_sites <- station_summary_centroids %>%
    dplyr::ungroup() %>%
    dplyr::distinct(Cycle, Year, Unit_Code, Community, Sampling_Frame,
                    Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
                    Seg_Length_m, Segs_Per_Station, Meters_Per_Station,
                    tran_seg_id, lat, long) %>%
    dplyr::mutate(Scientific_Name = "All Sites",
                  Code = "All Sites",
                  Mean_MaxCover = 0,
                  col_Mean_MaxCover = "#FFFFFF")

  #EIPS_inter_station <- dplyr::bind_rows(all_sites, EIPS_inter_station) %>%
  #  dplyr::mutate(Monitoring_Sites = dplyr::case_when(Code == "All Sites" ~ TRUE,
  #                                                    TRUE ~ FALSE))

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

  # NPS tiles
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

  # PACN/GIS polygons:
  if (agol_sample_frame == "Mauna Loa") {
    agol_sample_frame <- "Subalpine Shrubland"
  }

  url <- httr::parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/")
  url$path <- paste(url$path, "PACN_Vegetation_Sampling_Frames_vlyr/FeatureServer/0/query", sep = "/")
  url$query <- list(where = paste0("Sampling_Frame = '", agol_sample_frame, "'"),
                    outFields = "*",
                    returnGeometry = "true",
                    f = "geojson")
  request <- httr::build_url(url)
  request #print url request


  # sf::st_read() will not silence message when AGOL layer not available
  # message is: "Re-reading with feature count reset from 1 to 0"
  # This is showing up in knitted html so I have to remove following function (highlighted with **)
  # until AGOL public layer works again.
  mgmt_unit <- dplyr::tibble()
  #**mgmt_unit <- sf::st_read(request, quiet = TRUE)*

  if (nrow(mgmt_unit) == 0) {
    # If agol request does not work (zero observation returned), then pull
    # sampling frame polygons from static file in Rmarkdown file (.Rmd) folder:
    gdb <- "spatial/PACN_Vegetation_Sampling_Frames_view/aae67c1a-113a-4c2a-beaa-16139b2d6f7a.gdb"
    gdb_layer <- "PACN_Vegetation_Sampling_Frames"
    mgmt_unit <- sf::read_sf(gdb, gdb_layer, quiet = TRUE)
    mgmt_unit <- sf::st_transform(mgmt_unit, crs = 4326)
    mgmt_unit <- mgmt_unit |>
      dplyr::filter(Sampling_Frame == agol_sample_frame)
  }
  # Colors for polygons:
  factpal <- leaflet::colorFactor(c("#F8573A", "#F4C47B", "#28468B", "#AED5CB"),
                                  mgmt_unit$Zone)


  # Set up cross-talk shared object
  #sd_cover <- crosstalk::SharedData$new(EIPS_inter_station)
  sd_cover <- crosstalk::SharedData$new(station_summary_centroids)
  sd_all_sites <- crosstalk::SharedData$new(station_summary_centroids)

  # Species Cover (no change)----
  if (parameter == "Mean_Species_Cover" & change == FALSE) {

    Palette <- c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41")
    Levels <- as.factor(c(0,1,2,3,4,5,6,7))
    Labels <- c("Out","<1", "1 - 5","5 - 10","10 - 25", "25 - 50", "50 - 75", "75+")
    custom_leg <- data.frame(Palette, Levels, Labels)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_slider("cover data", "Cover Data (Select Years)", sd_cover, ~Year, step = 5, ticks = FALSE, sep = ""),
                               crosstalk::filter_slider("sites", "Sites (Select Years)", sd_all_sites, ~Year, step = 5, sep = "", ticks = FALSE),
                               crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = FALSE)
                             ),
                             leaflet::leaflet(width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet::addPolygons(data = mgmt_unit, group = "Zone", color =  ~factpal(Zone), label = ~Zone) |>
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Zone", "Sites", "Non-native Cover"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(data = sd_all_sites,
                                                         group = "Sites",
                                                         color = "black",
                                                         weight = 3,
                                                         stroke = TRUE,
                                                         radius = 7,
                                                         fillOpacity = 0,
                                                         # popup = ~paste(sep = "<br/>",
                                                         #                paste("Transect", Transect_Number),
                                                         #                Scientific_Name,
                                                         #                Txt_Range)
                               ) %>%

                               leaflet::addCircleMarkers(data = sd_cover,
                                                         group = "Non-native Cover",
                                                         color = ~col_Mean_MaxCover,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        as.character(Year),
                                                                        paste("Transect",
                                                                              Transect_Number),
                                                                        Scientific_Name,
                                                                        Txt_Range)
                               ) %>%



                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Cover (%)",
                                                  opacity = 1))

  }

  # Total Cover (no change) ----
  if (parameter == "Mean_Total_Cover" & change == FALSE) {

    Palette <- c("#FFFFFF", "#F6F4C6", "#EEE98D", "#EAC07D", "#E7976E", "#E36E5F", "#E04550", "#DD1C41")
    Levels <- as.factor(c(0,1,2,3,4,5,6,7))
    Labels <- c("0","<1", "1 - 5","5 - 10","10 - 25", "25 - 50", "50 - 75", "75+")
    custom_leg <- data.frame(Palette, Levels, Labels)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_checkbox("year", "Year", sd_cover, ~Year)#,
                               #crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = TRUE)
                             ),
                             leaflet::leaflet(sd_cover, width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet::addPolygons(data = mgmt_unit, group = "Zone", color =  ~factpal(Zone), label = ~Zone) |>
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Zone", "Invasive Plant Transects (l)"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(group = "Invasive Plant Transects (l)",
                                                         color = ~col_Mean_MaxCover,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        paste("Transect", Transect_Number),
                                                                        #Scientific_Name,
                                                                        Txt_Range)) %>%
                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Cover (%)",
                                                  opacity = 1))

  }
  #  Richness (no change) ----
  if (parameter == "Max_Richness" & change == FALSE) {
    # Richness

    Palette <- c("#FFFFFF", "#FFFFCC", "#C7E9B4", "#7FCDBB", "#41B6C4", "#2C7FB8", "#253494")
    Labels <- c("0", "1", "2-3", "4-5", "6-8", "9-14", "15+")
    custom_leg <- data.frame(Labels, Palette)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_checkbox("year", "Year", sd_cover, ~Year)#,
                               #crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = FALSE)
                             ),
                             leaflet::leaflet(sd_cover, width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet::addPolygons(data = mgmt_unit, group = "Zone", color =  ~factpal(Zone), label = ~Zone) |>
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Zone"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(color = ~col_Richness,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        paste("Transect", Transect_Number),
                                                                        #Scientific_Name,
                                                                        paste("Max non-native species: ", Max_Seg_Richness),
                                                                        paste0("Across ",Segs_Per_Station, " Segments"),
                                                                        paste0("Each segment = 5x", Seg_Length_m, " meters")
                                                         )) %>%
                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Non-Native Species",
                                                  opacity = 1))

  }

  # Change in Cover ----
  if (parameter == "Mean_Species_Cover" & change == TRUE |
      parameter == "Mean_Total_Cover" & change == TRUE) {

    Palette <- c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#EDF8E9",
                 "#FFFFFF", "#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026")
    Labels <- c("- [75+]","-[50-75]", "-[25-50]", "-[10-25]", "-[5-10]", "-[1-5]", "-[0-1]",
                "0", "0-1", "1-5", "5-10", "10-25", "25-50", "50-75", "75+")
    custom_leg <- data.frame(Labels, Palette)

    map <- crosstalk::bscols(widths = c(3,NA),device = "lg",
                             list(
                               crosstalk::filter_select("year", "Year", sd_cover, ~Year, multiple = FALSE),
                               crosstalk::filter_select("species", "Species", sd_cover, ~Scientific_Name, multiple = FALSE)
                             ),
                             leaflet::leaflet(sd_cover, width = "100%", height = 700) %>%
                               leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
                               leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
                               leaflet::addPolygons(data = mgmt_unit, group = "Zone", color =  ~factpal(Zone), label = ~Zone) |>
                               leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                                                         overlayGroups = c("Invasive Plant Transects (f)", "Zone"),
                                                         options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
                               leaflet::addCircleMarkers(group = "Invasive Plant Transects (f)",
                                                         color = ~col_Chg,
                                                         stroke = FALSE,
                                                         radius = 6,
                                                         fillOpacity = 1,
                                                         popup = ~paste(sep = "<br/>",
                                                                        paste("Transect", Transect_Number),
                                                                        Scientific_Name,
                                                                        Txt_Range)) %>%
                               leaflet::addLegend(colors = custom_leg$Palette, labels = custom_leg$Labels,
                                                  title = "Cover Change",
                                                  opacity = 1))

  }


  return(map)


}



