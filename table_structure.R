
library(DBI)
library(dbplyr)
library(tidyverse)

DB <- dbConnect(odbc::odbc(), "pacnveg")

#.----
# SPATIAL & TEMPORAL TABLES ----


# 1 Spatial --------------------------------------------------------------------

# Park Codes
#....Sites----
tbl(DB, "tbl_Sites") %>%
  colnames()

tbl_Sites_short <- tbl(DB, "tbl_Sites") %>%
  select(Site_ID, Unit_Code)


tbl_Sites_extra <- tbl(DB, "tbl_Sites") %>%
  select(Site_ID, Unit_Code, Site_Name)
#select(-Site_Desc, -Site_Notes, -SSMA_TimeStamp)


# Plant Community & Sampling Frames within the parks.
#....Locations----
tbl(DB, "tbl_Locations") %>%
  colnames()

tbl_Locations_short <- tbl(DB, "tbl_Locations") %>%
  select(Location_ID, Site_ID, Community, Sampling_Frame)


tbl_Locations_extra <- tbl(DB, "tbl_Locations") %>%
  select(Location_ID, Site_ID, Community, Sampling_Frame, Zone, Management_Unit)
#(-Loc_Notes,-Updated_date,-SSMA_TimeStamp)


# Plots within Sampling Frames
#....Plots----
tbl(DB, "tbl_Plot") %>%
  colnames()

tbl_Plot_short <- tbl(DB, "tbl_Plot") %>%
  select(Plot_ID, Location_ID, Plot_Number, Plot_Type)

tbl_Plot_extra <- tbl(DB, "tbl_Plot") %>%
  select(Plot_ID, Location_ID, Plot_Number, Azimuth_Plot,
         Start_Lat, Start_Long, Center_Lat, Center_Long, End_Lat, End_Long,
         GCS, GCS_Datum, Lat_Dir, Long_Dir, Plot_Notes)
glimpse(tbl_Plot_extra)
#select(-GPS_Unit, -Waypoint, -Start_X_Coord, Start_Y_Coord, -Start_Est_H_Error,
#-End_X_Coord, -End_Y_Coord, -End_Est_H_Error, -Coord_Units, -Coord_System,
#-UTM_Zone, -Datum, -Accuracy_Notes, -Stake_dist, -Stake_direction, -Declination,
#-Slope_Plot, -Slope_Hillside, -Aspect_Hillside, -Elevation,-Road_Trail,
#-Azimuth_Road, -Route, -Plot_established, -Plot_discontinued, -Plot_entered_date,
#-Plot_entered_by, -Plot_updated_date, -Plot_updated_by, -Plot_verified,
#-Plot_verified_date, -Plot_verified_by, -SSMA_TimeStamp, -End_Tag, -Posts_Missing)

# 2 Temporal -------------------------------------------------------------------

#....tbl_Events----
tbl(DB, "tbl_Events") %>%
  colnames()

tbl_Events_short <- tbl(DB, "tbl_Events") %>%
  select(Event_ID, Plot_ID, Start_Date, QA_Plot)
glimpse(tbl_Events_short)

tbl_Events_extra <- tbl(DB, "tbl_Events") %>%
  select(Event_ID, Plot_ID, Start_Date, Images, Max_veg_ht,
         Entered_date, Updated_date, Verified, Verified_by, Verified_date,
         Certified, Certified_by, Certified_date, Completion_time,
         Event_Notes, QA_notes) #QA_notes,
glimpse(tbl_Events_extra)
#select(-Entered_by, -Entered_date, -Updated_by, -Updated_date,
# -End_Date, -SSMA_TimeStamp)

# 3 Join Spatial and Temporal --------------------------------------------------
#....Events----
Events <- tbl_Events_short %>%
  left_join(tbl_Plot_short, by = "Plot_ID") %>%
  left_join(tbl_Locations_short, by = "Location_ID") %>%
  left_join(tbl_Sites_short, by = "Site_ID") %>%
  select(Start_Date, Unit_Code, Community, Sampling_Frame, Plot_Type,
         Plot_Number, QA_Plot, Event_ID)
glimpse(Events)

#....Events_extra----

Events_extra <- tbl_Events_extra %>%
  left_join(tbl_Plot_extra, by = "Plot_ID") %>%
  left_join(tbl_Locations_extra, by = "Location_ID") %>%
  #Move long text columns to end because of SQL driver error:
  relocate(Plot_Notes, .after = last_col()) %>%
  relocate(Event_Notes, .after = last_col()) %>%
  relocate(QA_notes, .after = last_col()) %>%
  left_join(tbl_Sites_extra, by = "Site_ID") %>%
  #Move long text columns to end because of SQL driver error:
  relocate(Plot_Notes, .after = last_col()) %>%
  relocate(Event_Notes, .after = last_col()) %>%
  relocate(QA_notes, .after = last_col())
glimpse(Events_extra)

Events_extra_QAQC <- Events_extra %>%
  select(Start_Date, Unit_Code, Sampling_Frame, Plot_Number,
         Entered_date, Updated_date, Verified, Verified_by, Verified_date,
         Certified, Certified_by, Certified_date, Completion_time,
         Event_Notes, Plot_Notes, QA_notes)

Events_extra_xy <- Events_extra %>%
  select(Start_Date, Unit_Code, Sampling_Frame, Plot_Number,
         Azimuth_Plot, Start_Lat, Start_Long, Center_Lat, Center_Long,
         End_Lat, End_Long, GCS, GCS_Datum, Lat_Dir, Long_Dir)

Events_extra_max_veg_ht <- Events_extra %>%
  select(Start_Date, Unit_Code, Sampling_Frame, Plot_Number, Max_veg_ht)

Events_extra_images <- Events_extra %>%
  select(Start_Date, Unit_Code, Sampling_Frame, Plot_Number, Images)

Events_extra_places <- Events_extra %>%
  select(Start_Date, Unit_Code, Sampling_Frame, Plot_Number,
         Zone, Management_Unit, Site_Name)


#.----
# SPECIES --------------------------------------------------------------------
tbl(DB, "tlu_Species") %>%
  colnames()

tlu_Species_short <- tbl(DB, "tlu_Species") %>%
  select(Species_ID, Scientific_name, Code, Life_form)
glimpse(tlu_Species_short)

tlu_Species_extra <- tbl(DB, "tlu_Species") %>%
  select(Species_ID, Code, Taxonomic_Order, Taxonomic_Family, Genus, Species,
         Subdivision, Authority, Synonym, Authority_Source, Citation,
         Common_name, Life_cycle, Complete, Update_date, Update_by,
         Update_comments)
#select(-SSMA_TimeStamp, -TSN)

# ....subtbl xref_Park_Species_Nativity ----
# Cross-reference table between parks and nativity of a species.
# Nativity is park specific (example Casuarina equisetifolia
# [Species_ID=20100315154438-239126026.630402] is "Non-Native" in HAVO
# and "Native" in AMME

tbl(DB, "xref_Park_Species_Nativity") %>%
  colnames()

xref_Park_Species_Nativity_short <- tbl(DB, "xref_Park_Species_Nativity") %>%
  select(Species_ID, Park, Nativity)

xref_Park_Species_Nativity_extra <- tbl(DB, "xref_Park_Species_Nativity") %>%
  select(Species_ID, Park, Life_form, Nativity, Park_common_name, Distribution, Conservation_Status)
#select(-Life_form, -Life_form, -Update_date, -Update_by, -Update_comments -SSMA_TimeStamp)

Species <- tlu_Species_short %>%
  right_join(xref_Park_Species_Nativity_short, by = "Species_ID")
glimpse(Species)


Species_extra <- tlu_Species_extra %>%
  right_join(xref_Park_Species_Nativity_extra, by = "Species_ID")
glimpse(Species_extra)

#.----





# DATA TABLES ----

#1 Lrg_Woody_Individual --------------------------------------------------------

# Tree density information collected for small trees (subalpine),
# large trees (forest, coastal, subalpine), and large tree ferns (forest).
tbl(DB, "tbl_Lg_Woody_Individual") %>%
colnames()

tbl_Lg_Woody_Individual <- tbl(DB, "tbl_Lg_Woody_Individual") %>%
  select(Large_Woody_ID, Event_ID, Species_ID, Quad, Status, Height,
         Height_Dead, Boles, DBH, DBH_Other = DBH_Basal, Vigor, Fruit_Flower, Rooting, Foliar,
         Caudex_Length, Shrublike_Growth, Resprouts, Measurement_Type)
#select(-Sort_Order, -SSMA_TimeStamp, -Life_Form)

# ....subtbl Multiple_Boles ----
##Bole DBH for trees that have multiple boles.
tbl(DB, "tbl_Multiple_Boles") %>%
  colnames()

tbl_Multiple_Boles <- tbl(DB, "tbl_Multiple_Boles") %>%
  select(Large_Woody_ID, DBH_Bole = DBH, Status_Bole = Status)
#select(-Bole_ID, -SSMA_TimeStamp, -Root_Sprout)

# ....subtbl Snags ----
# Extra data for dead trees
#tbl(DB, "tbl_Snags") %>%
#  colnames()
#tbl_Snags <- tbl(DB, "tbl_Snags")%>%
#  select(Large_Woody_ID, Basal_diameter, Height)
##select(-Snag_ID, -Sort_Order, -SSMA_TimeStamp)
#glimpse(tbl_Snags)

# ....subtbl Basal_Sprout ----
# Sprout size data collected in recently disturbed sites for individuals
# that have been top killed and are alive only by basal sprouts.

#tbl(DB, "tbl_Basal_Sprout") %>%
#  colnames()
#
#tbl_Basal_Sprout <- tbl(DB, "tbl_Basal_Sprout") %>%
#  select(Large_Woody_ID, Sprout_Ht, W1, W2, Area, Volume)
#  #select(-SSMA_TimeStamp, -Sprout_ID)

# ....JOIN Large Tree ----
Large_Trees <- Events %>%
  right_join(tbl_Lg_Woody_Individual, by = "Event_ID") %>%
  left_join(tbl_Multiple_Boles, by = "Large_Woody_ID") %>%
  left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
  select(-Large_Woody_ID, -Event_ID, -Species_ID)
glimpse(Large_Trees)




#2 Tree_Canopy_Height ----------------------------------------------------------
# Recorded within each plot by measuring the height of ~five canopy trees per
# plot that represent the average canopy height within the plot.
tbl(DB, "tbl_Tree_Canopy_Height") %>%
  colnames()

tbl_Tree_Canopy_Height <- tbl(DB, "tbl_Tree_Canopy_Height") %>%
  select(Event_ID, Species_ID, Quad, Status, Top, Base, Base_ht,
         Distance, Height, Method, DBH, Comments)
  #select(-Tree_Height_ID, -Sort_Order, -SSMA_TimeStamp)

Canopy_Height <- Events %>%
  right_join(tbl_Tree_Canopy_Height, by = "Event_ID") %>%
  left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
  select(-Event_ID, -Species_ID) %>%
  #Move long text columns to end because of SQL driver error:
  relocate(Comments, .after = last_col())
glimpse(Canopy_Height)



#3 Presence --------------------------------------------------------------------
#List of species present within plot.
tbl(DB, "tbl_Presence") %>%
  colnames()

tbl_Presence <- tbl(DB, "tbl_Presence") %>%
  select(Event_ID, Species_ID, Fruit_Flower, Dead, Outside_Plot, cf, Comments)
#select(-Presence_ID, -Sort_Order, -SSMA_TimeStamp)

Presence <- Events %>%
  right_join(tbl_Presence, by = "Event_ID") %>%
  left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
  select(-Event_ID, -Species_ID) %>%
  #Move long text columns to end because of SQL driver error:
  relocate(Comments, .after = last_col())
glimpse(Presence)




#4 Sm_Woody_Tally --------------------------------------------------------------
# Count data, foliar height, and rooting height data tallied for
# vines, seedlings, shrubs, small trees, and small tree ferns.

tbl(DB, "tbl_Sm_Woody_Tally") %>%
  colnames()

tbl_Sm_Woody_Tally <- tbl(DB, "tbl_Sm_Woody_Tally") %>%
  select(Event_ID, Species_ID, Transect, DBH, Status,
         Foliar, Rooting, Count, Comments)
#select(-Small_Woody_ID, -Sort_Order, -SSMA_TimeStamp, -Life_Form)

Sm_Woody <- Events %>%
  right_join(tbl_Sm_Woody_Tally, by = "Event_ID") %>%
  left_join(Species, by = c("Species_ID", "Unit_Code" = "Park")) %>%
  select(-Event_ID, -Species_ID) %>%
  #Move long text columns to end because of SQL driver error:
  relocate(Comments, .after = last_col())
glimpse(Sm_Woody)



#5 Understory_Cover ------------------------------------------------------------
# point-intercept cover of  species and substrate
# two stratum: 1)low [0-1m] 2) high [1-2m]

tbl(DB, "tbl_Understory_Cover") %>%
  colnames()

tbl_Understory_Cover <- tbl(DB, "tbl_Understory_Cover") %>%
  select(Event_ID, Point_ID, Point, Substrate)
#all selected

# ....subtbl xref_Understory_Low ----
# Cross reference table between tbl_Understory and tlu_Species.
# Low = 0-1 meter from ground point-intercept
tbl(DB, "xref_Understory_Low") %>%
  colnames()

xref_Understory_Low <- tbl(DB, "xref_Understory_Low") %>%
select(Event_ID, Point_ID, Species_ID, Dead)
#select(-Sort_Order, -SSMA_TimeStamp)


# ....subtbl xref_Understory_High ----
# Cross reference table between tbl_Understory and tlu_Species.
# Low = 1-2 meter from ground point-intercept

tbl(DB, "xref_Understory_High") %>%
  colnames()

xref_Understory_High <- tbl(DB, "xref_Understory_High") %>%
  select(Event_ID, Point_ID, Species_ID, Dead)





#6 Woody Debris ----------------------------------------------------------------
# Dead, downed wood and tree fern logs
# with a diameter >=7.6 at the point of planar intersection.

tbl(DB, "tbl_Woody_Debris") %>%
  colnames()

tbl_Woody_Debris <- tbl(DB, "tbl_Woody_Debris") %>%
  select(Woody_Debris_ID, Event_ID, Transect, Length)
#select(-Comments, -Sort_Order, -SSMA_TimeStamp)


# ....subtbl Debris_Species  ----
# Additional information on debris type (wood/tree fern), diameter,
# and decay class

tbl(DB, "tbl_Debris_Species") %>%
  colnames()

tbl_Debris_Species <- tbl(DB, "tbl_Debris_Species") %>%
  select(Woody_Debris_ID, Debris_type, Diameter, Decay_Class, Comments)
#select(-Debris_Species_ID, -Sort_Order, SSMA_TimeStamp)




