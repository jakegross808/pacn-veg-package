#old stuff -----
testEIPSconn <- DBI::dbConnect(odbc::odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb")

#Sites (e.g. Park Codes)
#Short
tbl_Sites_short <- dplyr::tbl(testEIPSconn, "tbl_Sites") %>%
  dplyr::select(Site_ID, Unit_Code)
#Extra
tbl_Sites_extra <- dplyr::tbl(testEIPSconn, "tbl_Sites") %>%
  dplyr::select(Site_ID, Unit_Code, Site_Name)

#Locations (e.g. Sampling Frame)
#Short
tbl_Locations_short <- dplyr::tbl(testEIPSconn, "tbl_Locations") %>%
  dplyr::select(Location_ID, Site_ID, Community = Plant_Community, Sampling_Frame)
#Extra
tbl_Locations_extra <- dplyr::tbl(testEIPSconn, "tbl_Locations") %>%
  dplyr::select(Location_ID, Site_ID, Community = Plant_Community, Sampling_Frame, Zone, Management_Unit)

# Transects
tbl_Transects_short <- dplyr::tbl(testEIPSconn, "tbl_Transects") %>%
  dplyr::select(Transect_ID, Location_ID, Transect_Number, Transect_Type)
tbl_Transects_extra <- dplyr::tbl(testEIPSconn, "tbl_Transects") %>%
  dplyr::select(Transect_ID, Location_ID, Transect_Type, Transect_Number, Azimuth_Transect = Azimuth, Lat = Latitude, Lat_Dir = Latitude_Dir, Long = Longitude, Long_Dir = Longitude_Dir, GCS, Transect_Notes)

# Events (e.g. The date the plot was sampled, QA/QC records)
# Extra
Events_extra <-  dplyr::tbl(testEIPSconn, "tbl_Events") %>%
  # add "Year" (year sampled) and "Cycle" (sample cycle)
  dplyr::mutate(Year = YEAR(Start_Date)) %>%
  dplyr::mutate(Cycle = ifelse(Year <= 2014, 1,
                               ifelse(Year >= 2015 & Year <= 2020, 2,
                                      ifelse(Year >= 2021, 3, NA)))) %>%
  dplyr::left_join(tbl_Transects_extra, by = "Transect_ID") %>%
  dplyr::left_join(tbl_Locations_extra, by = "Location_ID") %>%
  #Move long text columns to end because of SQL driver error:
  dplyr::relocate(Event_Notes, .after = last_col()) %>%
  dplyr::relocate(Transect_Notes, .after = last_col()) %>%
  dplyr::left_join(tbl_Sites_extra, by = "Site_ID") %>%
  #Move long text columns to end because of SQL driver error:
  dplyr::relocate(Event_Notes, .after = last_col()) %>%
  dplyr::relocate(Transect_Notes, .after = last_col()) %>%
  dplyr::select(Event_ID, Transect_ID, Unit_Code, Community, Sampling_Frame, Start_Date, Year, Cycle, Zone, Management_Unit,
                Transect_Number, Site_Name, Transect_Type, Transect_Number, Azimuth_Transect, Lat, Long,
                GCS, Lat_Dir, Long_Dir, Entered_Date, Updated_Date, Verified, Verified_By, Verified_Date,
                Certified, Certified_By, Certified_Date, Transect_Notes, Event_Notes) #-Start_Date
# Short
Events <- Events_extra %>%
  dplyr::select(Event_ID, Transect_ID, Unit_Code, Community, Sampling_Frame, Year, Cycle, Transect_Type, Transect_Number, Certified, Verified) %>%
  dplyr::collect()

# New Stuff ----
tbl_images <- dplyr::tbl(testEIPSconn, "tbl_Image_Points")
tbl_images_xy_new <- tbl_images %>%
  dplyr::select(Event_ID, Image_Point, Latitude, Latitude_Dir, Longitude, Longitude_Dir, GCS, GPS_Error) %>%
  dplyr::collect()
EIPS_image_pts <- Events %>%
  dplyr::right_join(tbl_images_xy_new, by = "Event_ID") #%>%
  #dplyr::left_join(tlu_Segment_Points, by = "Segment_ID")
