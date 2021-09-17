##===================== TITLE =========================
##...................................................##
##...US National Park Service........................##
##...Pacific Island Inventory & Monitoring Network...##
##...................................................##
##...Jacob Gross 03/31/2020..........................##
##...................................................##
##...FTPC - Focal Terrestrial Plant Communities......##
##...................................................##
##...Connect to FTPC Database and Export Data........##
##...................................................##
##===================================================##

#...Description:  This code sets up workspace in user's project folder,
# exports tables from the FTPC Database to project folder,
# checks taxonomy for errors, and computes additional advanced tables ready
# for analysis.

#..................
#   Packages   ----
#..................

library(odbc) # connects to many commercial databases (open database connectivity)
library(tidyverse) # Collection of data science R packages by Wickham et al.
library(dbplyr) # A 'dplyr' back end for databases that allows you
# to work with remote database tables as if they are in-memory data frames

#.............................
#   Connect to Database   ----
#.............................

#'* If teleworking, connect to Pulse Secure First! *

DB <- dbConnect(odbc::odbc(), "pacnveg") #open connection (connection string removed)

# summary of the connection
summary(DB)

# show all tables in database
DB_tbls <- dbListTables(DB)
DB_tbls

# show list of filtered tables in database
DB_tbls_filter <- DB_tbls[str_detect(DB_tbls, "tbl|tlu|xref")]
str(DB_tbls_filter)


#................................
#   Directories for Export   ----
#................................

# Save exported tables to timestamped folder in local working directory

# Save a file path in local working directory as R object;
LocDir <- file.path(getwd(), "FTPC_Export2") # new folder will be called "FTPC_Export"
LocDir

# Create the directory, if this is not the first time the script was
# run in the R project then you will get warning that it already exists, this is ok.
dir.create(LocDir)

# Create a new folder
new_folder <- format(Sys.time(), "FTPC_%F_%H%M") #(new folder name = FTPC_YYYY-MM-DD_HHMM)
dir.create(file.path(LocDir, new_folder)) # Create the time stamp folder

LocDir <- file.path(LocDir, new_folder, "/")
# use "LocDir" object to save tables to this location for rest of script
# print LocDir filepath
cat(paste("FTPC export will save to: ", LocDir))

#..............................
#   Standard data tables   ----
#..............................


##...Sample sites – location aggregates, i.e. individual parks.
tbl_Sites <- tbl(DB, "tbl_Sites") %>%
    select(Site_ID, Site_Name, Unit_Code) %>%
    collect()


#...Site_ID : Unique record identifier
#...Site_Name : Unique name or code for a site
#...Site_Desc : Description for a site
#...Unit_Code : Park, Monument or Network code
#...Site_Notes : General notes on the site


##...Sampling frame locations – sampling areas within the parks.
tbl_Locations <- tbl(DB, "tbl_Locations") %>%
  select(Location_ID, Site_ID, Sampling_Frame,
         Community) %>%
  collect()


#...Location_ID : Unique record identifier
#...Site_ID : Link to tbl_Sites
#...Community : Focal community type, i.e. wet forest, subalpine, shrubland, coastal, limestone forest, mangrove, wetland
#...Sampling_Frame : Name of the sampling frame
#...Zone : Name of the zone if applicable
#...Management_Unit : /!\ missing description
#...Loc_Notes : General notes on the location
#...Updated_date : Date of entry or last change
#...Updated_by : Who updated the Location information


##...Sampling frame plot locations.
tbl_Plot <- tbl(DB, "tbl_Plot") %>%
  select(Plot_ID, Location_ID, Plot_Number, Plot_Type,
         Center_X_Coord,Center_Y_Coord,
         Start_X_Coord,Start_Y_Coord,
         End_X_Coord,End_Y_Coord,
         UTM_Zone,Datum,Elevation,
         Plot_entered_date,Plot_updated_date,
         Plot_verified,Plot_verified_date) %>%
  collect()


#...Plot_ID : Unique record identifier
#...Location_ID : Link to tbl_Locations
#...Plot_Number : Plot identification number
#...Plot_Type : What type of plot it is, i.e. fixed or rotational
#...GPS_Unit : What GPS unit was used to collect the plot coordinates
#...Waypoint : GPS waypoint
#...Start_Tag : /!\ missing description
#...End_Tag : /!\ missing description
#...Start_X_Coord : The X coordinate for the start of the plot
#...Start_Y_Coord : The Y coordinate for the start of the plot
#...Start_Est_H_Error : Estimated horizontal accuracy for starting coordinates
#...Center_X_Coord : /!\ missing description
#...Center_Y_Coord : /!\ missing description
#...Center_Est_H_Error : /!\ missing description
#...End_X_Coord : The X coordinate for the end of the plot
#...End_Y_Coord : The Y coordinate for the end of the plot
#...End_Est_H_Error : Estimated horizontal accuracy for ending coordinates
#...Coord_Units : Coordinate distance units
#...Coord_System : Coordinate system
#...UTM_Zone : UTM Zone
#...Datum : Datum of mapping ellipsoid
#...Accuracy_Notes : Positional accuracy notes
#...Stake_dist : Stake offset distance in centimeters
#...Stake_direction : Stake offset direction in degrees
#...Azimuth_Plot : Azimuth of the plot origin
#...Declination : Declination of the plot origin
#...Slope_Plot : Percent slope along center transect azimuth
#...Slope_Hillside : Percent slope along hillside
#...Aspect_Hillside : Direction the hillside is facing
#...Elevation : Elevation of the plot origin
#...Road_Trail : Road and trail used to travel to the plot
#...Azimuth_Road : Azimuth where road and trail is left to access plot
#...Route : The route to the plot. Includes plot layout and any reference features used to access the plot
#...Plot_Notes : Any additional information about the plot, i.e problems encountered, changes, etc.
#...Plot_established : Date the plot was established
#...Plot_discontinued : Date the plot was discontinued
#...Plot_entered_date : /!\ missing description
#...Plot_entered_by : /!\ missing description
#...Plot_updated_date : Date of the last update to this record
#...Plot_updated_by : Person who made the most recent edits
#...Plot_verified : /!\ missing description
#...Plot_verified_date : /!\ missing description
#...Plot_verified_by : /!\ missing description


##...Data collection event for a given location.
tbl_QA_Events <- tbl(DB, "tbl_Events") %>%
  select(Event_ID, Plot_ID, Start_Date, End_Date, Max_veg_ht,
         Entered_by, Entered_date, Updated_by, Updated_date,
         Verified, Verified_by, Verified_date,
         Certified, Certified_by, Certified_date,
         QA_Plot, QA_notes) %>%
  collect()

tbl_Events <- tbl(DB, "tbl_Events") %>%
  select(Event_ID, Plot_ID, Start_Date, End_Date, QA_Plot) %>%
  collect()


#...Event_ID : Unique record identifier
#...Plot_ID : Link to tbl_Plots
#...Protocol_Name : The name or code of the protocol governing the event
#...Start_Date : Starting date for the event
#...End_Date : Ending date for the event
#...Start_Time : Starting time for the event
#...Images : /!\ missing description
#...Max_veg_ht : /!\ missing description
#...Event_Notes : Notes about the sampling event
#...Entered_by : Who entered the data
#...Entered_date : When the data was entered
#...Updated_by : Who updated the data
#...Updated_date : When the data was updated
#...Verified : If the data is verified
#...Verified_by : Who verified the data
#...Verified_date : When the data was verified
#...Certified : If the data is certified
#...Certified_by : Who certified the data
#...Certified_date : When the data was certified
#...QA_Plot : /!\ missing description
#...QA_notes : Notes about the quality assurance procedure
#...Completion_time : /!\ missing description


##...Quality assurance query results for the working data set.
## /!\ missing

##...Edit log for changes made to data after certification.
## /!\ missing

##...Plot photo information linked to a sampling event.
tbl_Images <- tbl(DB, "tbl_Images") %>%
  select(Image_ID, Event_ID, Root_Path, Base_Path, Image_Project_Path,
         File_Name, Subject, Azimuth) %>%
  collect()

#...Image_ID : Unique record identifier for image records
#...Event_ID : Link to tbl_Events
#...Root_Path : The root path for the linked image file. This will change upon re-linking of database.
#...Base_Path : The base file path for the linked image file. This will stay the same no matter where the root folder is.
#...Image_Project_Path : Location of the image from the main project folder or image library
#...File_Name : The file name
#...Subject : The subject of the photo
#...Azimuth : The azimuth from the plot origin that the photo was taken
#...Time : The time the photo was taken
#...Camera_Type : The camera type used to take photo
#...Comments : Any additional information about the photo, i.e. camera type, lens, ASA.


#......................................
#   Project-specific data tables   ----
#......................................


##...Sprout size data collected in recently disturbed sites for individuals
  # that have been top killed and are alive only by basal sprouts.
tbl_Basal_Sprout <- tbl(DB, "tbl_Basal_Sprout") %>%
  select(Sprout_ID, Large_Woody_ID, Sprout_Ht, W1, W2, Area, Volume) %>%
  collect()


##...Sprout_ID : Unique record identifier
##...Large_Woody_ID : Link to tbl_Lg_Woody_Individual
##...Sprout_Ht : The longest basal sprout per tree (cm) measured from the base to the apical meristem
##...W1 : Widest horizontal distance (cm) or maximum diameter across the sprout crown
##...W2 : Perpendicular diameter to W1 horizontal distance (cm) across the sprouts
##...Area : Sprout elliptical crown area calculated from W1 and W2 distances
##...Volume : Sprout crown volume (cm3) calculated by multiplying the height of the tallest sprout by the elliptical crown area.


##...Individual species of dead downed wood and
  # tree fern logs with a diameter >=7.6 cm at the point of planar intersection.
tbl_Debris_Species <- tbl(DB, "tbl_Debris_Species") %>%
  select(Debris_Species_ID, Woody_Debris_ID, Debris_type,
         Diameter, Decay_Class) %>%
  collect()


##...Debris_Species_ID : Unique record identifier
##...Woody_Debris_ID : Link to tbl_Woody_Debris
##...Debris_type : Debris type, i.e. wood or tree fern.
##...Diameter : Diameter of woody debris
##...Decay_Class : Decay class of the woody debris, i.e. sound or rotten.
##...Comments : Comments about debris species.
##...Sort_Order : /!\ missing


##...Tree density information collected for small trees (subalpine),
# large trees (forest, coastal, subalpine), and large tree ferns (forest).
tbl_Lg_Woody_Individual <- tbl(DB, "tbl_Lg_Woody_Individual") %>%
  select(Large_Woody_ID, Event_ID, Species_ID, Life_Form, Quad, Status, Height,
         Height_Dead, Boles, DBH, DBH_Basal, Vigor, Fruit_Flower, Rooting, Foliar,
         Shrublike_Growth, Resprouts) %>%
  collect()


##...Large_Woody_ID : Unique record identifier
##...Event_ID : Link to tbl_Events
##...Species_ID : Link to tlu_Species
##...Life_Form : The life form of the species, i.e. seedling, shrub, small tree, etc.
##...Quad : The quad number the species was found in.
##...Status : Is the tree live or dead?
##...Height : Height of the tree, only populated for dead trees (snags)
##...Height_Dead : /!\ missing
##...Boles : The number of tree trunks
##...DBH : The diameter of the tree measured to the nearest tenth of a centimeter, or the total DBH of multiple boles.
##...DBH_Basal : /!\ missing
##...Vigor : The vigor class of the tree
##...Fruit_Flower : Is the plant fruiting or flowering?
##...Rooting : The rooting height class of the tree
##...Foliar : The foliar height class
##...Stem_Length : The stem length measured to the nearest tenth of a meter
##...Shrublike_Growth : /!\ missing
##...Resprouts : /!\ missing
##...Comments : Any additional comments about the species
##...Sort_Order : /!\ missing


##...Bole DBH for trees that have multiple boles.
tbl_Multiple_Boles <- tbl(DB, "tbl_Multiple_Boles") %>%
  select(Large_Woody_ID, Bole_ID, DBH, Root_Sprout, Status) %>%
  collect()


##...Large_Woody_ID : Link to tbl_Lg_Woody_Individual
##...Bole_ID : The identification of the bole (1, 2, top, etc.)
##...DBH : The diameter of the bole measured to the nearest tenth of a centimeter.
##...Root_Sprout : /!\ missing
##...Status : /!\ missing


##...List of species present within plot.
tbl_Presence <- tbl(DB, "tbl_Presence") %>%
  select(Presence_ID, Event_ID, Species_ID, Fruit_Flower,
         Dead, Outside_Plot, cf) %>%
  collect()


##...Presence_ID : Unique record identifier
##...Event_ID : Link to tbl_Events
##...Species_ID : Link to tlu_Species
##...Fruit_Flower : Is the plant fruiting or flowering?
##...Comments : Any additional comments about the species
##...Sort_Order : /!\ missing
##...Terrestrial : /!\ missing
##...Epiphyte : /!\ missing
##...Dead : /!\ missing
##...Outside_Plot : Species is present just outside of plot boundaries
##...cf : Short for the Latin: confer, "compare with", indicates uncertainty.


##...Slope values for the different segments of a plot transect.
tbl_Segment_Slope <- tbl(DB, "tbl_Segment_Slope") %>%
  select(Slope_ID, Woody_Debris_ID, Segment, Slope) %>%
  collect()


##...Slope_ID :  Unique record identifier
##...Woody_Debris_ID : Link to tbl_Woody_Debris
##...Segment : Slope segment length
##...Slope : Slope of the segment


##...Foliar and rooting height class data tallied for seedlings and shrubs (forest, coastal, subalpine), small trees (forest, coastal), and small tree ferns (forest).
tbl_Sm_Woody_Tally <- tbl(DB, "tbl_Sm_Woody_Tally") %>%
  select(Small_Woody_ID, Event_ID, Species_ID, Transect, Life_Form, DBH, Status,
         Foliar, Rooting, Count, Comments) %>%
  collect()


##...Small_Woody_ID : Unique record identifier
##...Event_ID : Link to tbl_Events
##...Species_ID : Link to tbl_Species
##...Transect : /!\
##...Life_Form : The life form of the species, i.e. seedling, shrub, small tree, etc.
##...DBH : The DBH class of small trees.
##...Status : Is the tree live or dead?
##...Foliar : The foliar height class
##...Rooting : The rooting height class of the tree
##...Count : The count of the species observed for the height class
##...Comments : Any additional comments about the species
##...Sort_Order : /!\


##.../!\ description missing
tbl_Snags <- tbl(DB, "tbl_Snags")%>%
  select(Snag_ID, Large_Woody_ID, Basal_diameter, Height) %>%
  collect()


##...Snag_ID : /!\ description missing
##...Large_Woody_ID : /!\ description missing
##...Basal_diameter : /!\ description missing
##...Height : /!\ description missing
##...Sort_Order : /!\ description missing


##...Tree canopy height data.
##...Recorded within each plot by measuring the height of five canopy trees per plot that represent the average canopy height within the plot.
tbl_Tree_Canopy_Height <- tbl(DB, "tbl_Tree_Canopy_Height") %>%
  select(Tree_Height_ID, Event_ID, Species_ID, Quad, Status, Top, Base, Base_ht,
         Distance, Height, Method, DBH, Comments) %>%
  collect()


##...Tree_Height_ID : Unique record identifier
##...Event_ID : Link to tbl_Events
##...Species_ID : Link to tlu_Species
##...Quad : The quad number the species was found in.
##...Status : Is the tree live or dead?
##...Top : Top percent, used to calculate canopy height if clinometer is used
##...Base : Base percent, used to calculate canopy height if clinometer is used
##...Base_ht : The height of the base of the tree (meters) that isused for the canpoy height calculation
##...Distance : Distance, used to calculate canopy height if clinometer is used
##...Height : Height of the canopy
##...Method : Method used to record canopy height, i.e. measuring rod, clinometer, ocular estimate, other
##...DBH : Diameter basal height
##...Comments : Any additional comments about the tree canopy height
##...Sort_Order : /!\ description missing


##...Understory cover species and substrate data along transects in coastal and forest communities.
tbl_Tree_Canopy_Height <- tbl(DB, "tbl_Tree_Canopy_Height")%>%
  select(Tree_Height_ID, Event_ID, Species_ID, Quad, Status, Top, Base, Base_ht,
         Distance, Height, Method, DBH, Comments) %>%
  collect()


##...Event_ID : Link to tbl_Events
##...Point_ID : Link to tlu_Points
##...Point : The point where the understory was observed
##...Substrate : The substrate that the understory is growing on


##...Transect information on dead downed wood and tree fern logs with a diameter >=7.6 at the point of planar intersection.
tbl_Woody_Debris <- tbl(DB, "tbl_Woody_Debris") %>%
  select(Woody_Debris_ID, Event_ID, Transect, Length, Azimuth, Slope, Comments) %>%
  collect()


##...Woody_Debris_ID : Unique record identifier
##...Event_ID : Link to tbl_Events
##...Transect : Transect number
##...Length : Length of the transect. For all forest and shrubland plots, will be either 50 or 20 meters. For coastal plots, will be either 20 or 10 meters.
##...Azimuth : Azimuth of the transect
##...Slope : Slope of the transect
##...Comments : Comments on the woody debris transect
##...Sort_Order : /!\ description missing



#.................................
#   Standard lookup tables   ----
#.................................


##...Contact data for project-related personnel.
tlu_Contacts <- tbl(DB, "tlu_Contacts") %>%
  select(Contact_ID, Last_Name, First_Name, Middle_Init, Initials, Organization,
         Position_Title, Address_Type, Address, Address2, City, State_Code, Zip_Code, Country,
         Email_Address, Cell_Phone, Work_Phone, Work_Extension, Contact_Location, Active) %>%
  collect()


##...Contact_ID : Unique record identifier
##...Last_Name : Last name
##...First_Name : First name
##...Middle_Init : Middle initial
##...Initials : Three letter initials of observer
##...Organization : Organization or employer
##...Position_Title : Title or position description
##...Address_Type : Address (mailing, physical, both) type
##...Address : Street address
##...Address2 : Address line 2, suite, apartment number
##...City : City or town
##...State_Code : State or province
##...Zip_Code : Zip code
##...Country : Country
##...Email_Address : E-mail address
##...Cell_Phone : Cell phone number
##...Work_Phone : Phone number
##...Work_Extension : Phone extension
##...Fax_Number : Fax number
##...Contact_Location : Contact's work location
##...Contact_Notes : Contact notes
##...Contact_created : Date the contact record was created
##...Contact_updated : Date the contact record was updated
##...Active : Allows users to choose which names show up in the drop down list


##...Enumerated lookup table.
tlu_Enumerations <- tbl(DB, "tlu_Enumerations") %>%
  select(Enum_Code, Enum_Group, Enum_Description) %>%
  collect()

##...Enum_Code : Code for lookup values
##...Enum_Description : Lookup value description
##...Enum_Group : Category for lookup value
##...Sort_Order : Order in which to sort lookup values


##...Lookup table of plant species.

# Life_form removed from this table,
# use Life_form from xref_Park_Species_Nativity
tlu_Species <- tbl(DB, "tlu_Species") %>%
  select(Species_ID, TSN, Taxonomic_Order, Taxonomic_Family, Scientific_name,
         Genus, Species, Authority, Authority_Source, Common_name, Code,
         Life_cycle, Complete, Update_date,
         Update_comments, Synonym, Citation, Subdivision) %>%
  collect()

##...Species_ID : No description
##...TSN : No description
##...Taxonomic_Order : No description
##...Taxonomic_Family : No description
##...Genus : No description
##...Species : No description
##...Subdivision : No description
##...Scientific_name : No description
##...Authority : No description
##...Authority_Source : No description
##...Synonym : No description
##...Citation : No description
##...Common_name : No description
##...Code : 6 letter code to identify the plant species. First 3 letters of genus and the first 3 letters of species.
##...Life_form : No description
##...Life_cycle : No description
##...Species_complete : No description
##...Update_date : No description
##...Update_comments : No description


##...Point identification table for all plots.
tlu_Points_All <- tbl(DB, "tlu_Points_All") %>%
  select(Point_ID, Point, Transect, Tape) %>%
  collect()

##write.csv(tlu_Points_All,paste(NEW,"I_tlu_Points_All.csv",sep=""), row.names=FALSE)

##...Point_ID : Unique record identifier
##...Point : Point identification
##...Transect : Transect identification
##...Tape : Distance in meters from the start of the transect


##...Point identification table for coastal plots.
tlu_Points_Coastal <- tbl(DB, "tlu_Points_Coastal") %>%
  select(Point_ID, Point, Transect, Tape) %>%
  collect()

##...Point_ID : Unique record identifier
##...Point : Point identification
##...Transect : Transect identification
##...Tape : Distance in meters from the start of the transect


##...Point identification table for forest plots.
tlu_Points_Forest <- tbl(DB, "tlu_Points_Forest") %>%
  select(Point_ID, Point, Transect, Tape) %>%
  collect()

##...Point_ID : Unique record identifier
##...Point : Point identification
##...Transect : Transect identification
##...Tape : Distance in meters from the start of the transect


#.................................
#   Cross reference tables   ----
#.................................

##...Cross reference table between tbl_Understory and tlu_Species. Records at which points species 1 - 2 m tall were found.
xref_Understory_High <- tbl(DB, "xref_Understory_High") %>%
  select(Event_ID, Point_ID, Species_ID, Dead) %>%
  collect()

##...Event_ID : Together with Point_ID, a link to tbl_Understory
##...Point_ID : Together with Event_ID, a link to tbl_Understory
##...Species_ID : Link to tlu_Species
##...Dead : Is the species dead?
##...Sort_Order : Link to tlu_Species


##...Cross reference table between tbl_Understory and tlu_Species. Records at which points species less than 1 m tall were found.
xref_Understory_Low <- tbl(DB, "xref_Understory_Low") %>%
  select(Event_ID, Point_ID, Species_ID, Dead) %>%
  collect()

##...Event_ID : Together with Point_ID, a link to tbl_Understory
##...Point_ID : Together with Event_ID, a link to tbl_Understory
##...Species_ID : Link to tlu_Species
##...Dead : Is the species dead?
##...Sort_Order : Link to tlu_Species


##...Cross-reference table between events and contacts.
xref_Event_Contacts <- tbl(DB, "xref_Event_Contacts") %>%
  select(Event_ID, Contact_ID, Contact_Role) %>%
  collect()

##...Event_ID : Link to tbl_Events
##...Contact_ID : Link to tlu_Contacts
##...Contact_Role : The contact's role in the protocol


##...Cross-reference table between parks and nativity of a species.
xref_Park_Species_Nativity <- tbl(DB, "xref_Park_Species_Nativity") %>%
  select(TSN, Park, Park_common_name, Nativity, Distribution, Life_form) %>%
  collect()

xref_Park_Species_Nativity$Nativity[xref_Park_Species_Nativity$Nativity=="Non-native"]<-"Non-Native"

##...TSN : Taxonomic serial number of the species
##...Park : 4 letter park abbreviation
##...Park_common_name :
##...Life_form :
##...Nativity : Nativity of the species
##...Distribution :
##...Conservation_Status: ? 8 rows == TRUE, the rest are NA


#............................
#   TABLE PREPARATION   ----
#............................

##...............
##  > Events  ----
##...............

##...Merge
temp<-merge(tbl_Locations,tbl_Sites,by.x="Site_ID",by.y="Site_ID",all.x==TRUE)

##...Geography
Geography<-data.frame(Location_ID=temp$Location_ID,
Archipelago=c("Mariana",
"Hawaii","Hawaii","Hawaii","Hawaii","Hawaii","Hawaii","Hawaii","Hawaii","Hawaii",
"Hawaii","Hawaii","Hawaii",
"Hawaii","Hawaii",
"Hawaii",
"American Samoa","American Samoa",
"Mariana","Mariana","Mariana","Mariana"),
Island=c("Saipan",
"Hawaii","Hawaii","Hawaii","Hawaii","Hawaii","Hawaii","Hawaii","Hawaii","Hawaii",
"Molokai","Molokai","Molokai",
"Maui","Maui",
"Hawaii",
"Tutuila","Tau",
"Guam","Guam","Guam","Guam"))

temp2<-merge(temp,Geography,by.x="Location_ID",by.y="Location_ID",all.x=TRUE)

Plot<-merge(tbl_Plot,temp2,by.x="Location_ID",by.y="Location_ID",all.x==TRUE)

Event<-merge(tbl_Events,Plot,by.x="Plot_ID",by.y="Plot_ID",all.x==TRUE)

write_csv(Event,paste(LocDir,"Event.csv",sep=""))
#write_csv(Event,paste(IMDir,"Event.csv",sep=""))

##..............
##  > Species  ----
##..............

##...tlu_Species_simplification
tlu_Species_s <- tlu_Species %>%
  select(Species_ID,TSN,
         Order = Taxonomic_Order, #change column name to "Order"
         Family = Taxonomic_Family, #change column name to "Family"
         Genus, Species, Code,
         Life_cycle, Subdivision)


##...Check taxonomic names for errors and create working species "Name"

##...Check all cells in Order & Family column for spaces or tabs
tlu_Species_s %>%
  filter(str_detect(Order, "[:blank:]")) %>% #[:blank:] = space or tab
  .$Order # return just the column named "Order"

tlu_Species_s %>%
  filter(str_detect(Family, "[:blank:]")) %>% #[:blank:] = space or tab
  .$Family # return just the column named "Family"

## Change the taxonomic Order of for 'Snag' to 'NA'
snag <- tlu_Species_s %>%
  filter(str_detect(Order, "Snag")) #%>%
  #.$Order

Sp_s_Unknown <- tlu_Species_s %>%
  filter(is.na(Order) |
           is.na(Family) |
           is.na(Genus) |
           str_detect(Order, "Unknown") |
           str_detect(Family, "Unknown") |
           str_detect(Genus, "Unknown")
  )

Sp_s_CODESP <- tlu_Species_s %>%
  filter(is.na(Species) |
         str_detect(Species, "Unknown")
  )


tlu_Species_s$Order[which(tlu_Species_s$Order=="Snag")]<-NA
tlu_Species_s$Family[which(tlu_Species_s$Family == "Snag")] <- NA

## fix mispellings
tlu_Species_s$Family[which(tlu_Species_s$Family == "Fabales")] <- "Fabaceae"
tlu_Species_s$Family[which(tlu_Species_s$Family=="Davalliaeceae")]<-"Davalliaceae"
tlu_Species_s$Family[which(tlu_Species_s$Family=="Ptericaceae")]<-"Pteridaceae"


##...Genus check
tlu_Species_s %>%
  filter(str_detect(Genus, "\\w\\s\\w")) %>% #find any word, space, then word
  .$Genus # return just the column named "Genus"

tlu_Species_s$Genus[which(tlu_Species_s$Genus==
                            "Thelypteridaceae genus")]<-"Thelypteris"



## > Hybrids and Cultivars ----
tlu_Species_s %>%
  filter(str_detect(Species, "\\w\\s\\w")) %>% #find any word, space, then word
.$Species # return just the column named "Species"


tlu_Species_s$Species<-as.character(tlu_Species_s$Species)

#Remove hybrids from species names and move into comment field
Comment<-character(length(tlu_Species_s$Species))
# " x " or " X "-> hybrid
# "x " or "X " -> cultivar
for (i in 1:length(tlu_Species_s$Species)){
foo<-str_split(tlu_Species_s$Species[i]," x ")
foo1<-length(foo[[1]])
if (foo1>1){
Comment[i]<-"hybrid"
tlu_Species_s$Species[i]<-foo[[1]][1] # --> take the 1st species name
}
foo<-str_split(tlu_Species_s$Species[i]," X ")
foo1<-length(foo[[1]])
if (foo1>1){
Comment[i]<-"hybrid"
tlu_Species_s$Species[i]<-foo[[1]][1] # --> take the 1st species name
}
foo<-str_split(tlu_Species_s$Species[i],"x ")
foo1<-length(foo[[1]])
if (foo1>1){
Comment[i]<-"cultivar"
tlu_Species_s$Species[i]<-foo[[1]][2] # --> take species name
}
foo<-str_split(tlu_Species_s$Species[i],"X ")
foo1<-length(foo[[1]])
if (foo1>1){
Comment[i]<-"cultivar"
tlu_Species_s$Species[i]<-foo[[1]][2] # --> take species name
}
}

for (i in 1:length(tlu_Species_s$Species)){
foo<-str_split(tlu_Species_s$Species[i]," ")
foo1<-length(foo[[1]])
if (foo1>1){
tlu_Species_s$Species[i]<-foo[[1]][1]
}
}


##  > Change Genus of 'Unknowns' to NA  ----

tlu_Species_s$Genus[which(
  tlu_Species_s$Genus=="Unknown" |
    tlu_Species_s$Genus=="Unknown " |
    tlu_Species_s$Genus=="alien " |
    tlu_Species_s$Genus=="Alien" |
    tlu_Species_s$Genus=="Moss " |
    tlu_Species_s$Genus=="Lichen " |
    tlu_Species_s$Genus=="Araceae" |
    tlu_Species_s$Genus=="Brassicaceae" |
    tlu_Species_s$Genus=="Cyperaceae" |
    tlu_Species_s$Genus=="Fabaceae" |
    tlu_Species_s$Genus=="Malvaceae" |
    tlu_Species_s$Genus=="Zingiberaceae")] <- NA

tlu_Species_s$Species[is.na(tlu_Species_s$Genus)]<-NA

tlu_Species_s$Species[which(tlu_Species_s$Species=="spp.")]<-NA

# Add Name and Comment to Species table
Name<-character()
for (i in 1:dim(tlu_Species_s)[1]){
if (is.na(tlu_Species_s$Genus[i])==FALSE & is.na(tlu_Species_s$Species[i])==FALSE) Name[i]<-paste(tlu_Species_s$Genus[i],tlu_Species_s$Species[i],sep=" ")
if (is.na(tlu_Species_s$Genus[i])==FALSE & is.na(tlu_Species_s$Species[i])==TRUE) Name[i]<-paste(tlu_Species_s$Genus[i],"sp.",sep=" ")
if (is.na(tlu_Species_s$Family[i])==FALSE & is.na(tlu_Species_s$Genus[i])==TRUE & is.na(tlu_Species_s$Species[i])==TRUE) Name[i]<-paste(tlu_Species_s$Family[i],"sp.",sep=" ")
if (is.na(tlu_Species_s$Family[i])==FALSE & is.na(tlu_Species_s$Family[i])==TRUE & is.na(tlu_Species_s$Genus[i])==TRUE & is.na(tlu_Species_s$Species[i])==TRUE) Name[i]<-paste(tlu_Species_s$Order[i],"sp.",sep=" ")
}

tlu_Species_s<-data.frame(tlu_Species_s,Name,Comment)


##...xref_Park_Species_Nativity_s <- corrections to nativity not reflected yet in database.
xref_Park_Species_Nativity_s<-xref_Park_Species_Nativity




#xref_Park_Species_Nativity_s<-xref_Park_Species_Nativity[,c("TSN","Park","Nativity")] #No simplification
TSN_Park<-paste(xref_Park_Species_Nativity_s$Park,xref_Park_Species_Nativity_s$TSN,sep="_")
xref_Park_Species_Nativity_s<-data.frame(xref_Park_Species_Nativity_s,TSN_Park)
xref_Park_Species_Nativity_s$TSN_Park <- as.character(xref_Park_Species_Nativity_s$TSN_Park)
write_csv(xref_Park_Species_Nativity_s,paste(LocDir,"xref_Park_Species_Nativity_s.csv",sep=""))


##-------------------------------------------------------##
## Repeated function to merge species and event data ----
##-------------------------------------------------------##

mergeESN <- function(df){
  # Merges Event, Species ID, and Nativity with input table
  #
  # Args:
  #   df: Dataframe
  #
  # Returns:
  #   Dataframe with merged information

  temp<-merge(df,Event,by.x="Event_ID",by.y="Event_ID",all.x=TRUE)
  temp2<-merge(temp,tlu_Species_s,by.x="Species_ID",by.y="Species_ID",all.x=TRUE)
  TSN_Park<-paste(temp2$Unit_Code,temp2$TSN,sep="_")
  temp2<-data.frame(temp2,TSN_Park)
  temp3<-merge(temp2,xref_Park_Species_Nativity_s,by.x="TSN_Park",by.y="TSN_Park",all.x=TRUE)
  df <- temp3
  return(df)
}

##---------------------------------------------------##
##...Large Trees >= 10 cm DBH - 20 m x 50 m sensus ----
##---------------------------------------------------##

##...tbl_Lg_Woody_Individual_simplification all
tbl_Lg_Woody_Individual_s <- tbl_Lg_Woody_Individual
mtbl_Lg_Woody_Individual_s <- mergeESN(tbl_Lg_Woody_Individual_s)
write_csv(mtbl_Lg_Woody_Individual_s,paste(LocDir,"tbl_Lg_Woody_Individual.csv",sep=""))

##...Only live woody plants >= 10 cm
tbl_Lg_Woody_Indiv_DBH10 <- tbl_Lg_Woody_Individual_s[which(
    tbl_Lg_Woody_Individual_s$DBH>=10 & # greater than or equal to 10 DBH
    tbl_Lg_Woody_Individual_s$Life_Form!="Large Tree Fern"),] # remove tree ferns
mtbl_Lg_Woody_Indiv_DBH10 <- mergeESN(tbl_Lg_Woody_Indiv_DBH10)
write_csv(mtbl_Lg_Woody_Indiv_DBH10,paste(LocDir,"Lg_Trees.csv",sep=""))

##...tbl_Tree_Canopy_Height_simplification
tbl_Tree_Canopy_Height_s <- tbl_Tree_Canopy_Height
write_csv(tbl_Tree_Canopy_Height_s,paste(LocDir,"tbl_Tree_Canopy_Height.csv",sep=""))

##---------------------------------------------------##
##...Tree Ferns >=  10 cm DBH - 10 m x 25 m sensus ----
##---------------------------------------------------##

tbl_tree_fern <- tbl_Lg_Woody_Individual_s[which(
  tbl_Lg_Woody_Individual_s$DBH >= 10 &
    #tbl_Lg_Woody_Individual_s$Status=="Live" &
    tbl_Lg_Woody_Individual_s$Life_Form=="Large Tree Fern"),]

mtbl_tree_fern <- mergeESN(tbl_tree_fern)
write_csv(mtbl_tree_fern,paste(LocDir,"Lg_Tree_Ferns.csv",sep=""))

##--------------------------------------------------##
##...Small Woody <10 DBH ----
##--------------------------------------------------##

# all small woody ("Seedling" "Shrub" "Small Tree" "Small Tree Fern" "Tree" "Vine")
mtbl_Sm_Woody_Tally <- mergeESN(tbl_Sm_Woody_Tally)
table(mtbl_Sm_Woody_Tally$DBH)
table(mtbl_Sm_Woody_Tally$Life_Form)

mtbl_Sm_Woody_Tally %>%
  filter(Life_Form == "Grass") %>%
  glimpse()

mtbl_Sm_Woody_Tally %>%
  filter(Life_Form == "Herb") %>%
  .$Name

mtbl_Sm_Woody_Tally %>%
  filter(Life_Form == 'Tree') %>%
  .$Name

write_csv(mtbl_Sm_Woody_Tally,paste(LocDir,"tbl_Sm_Woody_Tally.csv",sep=""))

##--------------------------------------------------##
##...Small Trees (1-10cm) & Alive ----
##--------------------------------------------------##

tbl_small_tree<-tbl_Sm_Woody_Tally[which(
  #tbl_Sm_Woody_Tally$Status=="Live" &
  tbl_Sm_Woody_Tally$Life_Form=="Small Tree"),]
mtbl_small_tree <- mergeESN(tbl_small_tree)
write_csv(mtbl_small_tree,paste(LocDir,"Sm_Trees.csv",sep=""))

##----------------------------------------------##
##...Seedlings (< 1 cm DBH) & Alive ----
##----------------------------------------------##

tbl_seedling<-tbl_Sm_Woody_Tally[which(
  #tbl_Sm_Woody_Tally$Status=="Live" &
  tbl_Sm_Woody_Tally$Life_Form=="Seedling"),]
mtbl_seedling <- mergeESN(tbl_seedling)
write_csv(mtbl_seedling,paste(LocDir,"Seedlings.csv",sep=""))

##----------------------------------------------##
##...Shrubs - 2 m x 50 m sensus ----
##----------------------------------------------##

tbl_shrub<-tbl_Sm_Woody_Tally[which(
  #tbl_Sm_Woody_Tally$Status=="Live" &
  tbl_Sm_Woody_Tally$Life_Form=="Shrub"),]
mtbl_shrub <- mergeESN(tbl_shrub)
write_csv(mtbl_shrub,paste(LocDir,"Shrubs.csv",sep=""))

##----------------------------------------------##
##...Vines - 2 m x 50 m sensus ----
##----------------------------------------------##

tbl_vine<-tbl_Sm_Woody_Tally[which(
  #tbl_Sm_Woody_Tally$Status=="Live" &
  tbl_Sm_Woody_Tally$Life_Form=="Vine"),]
mtbl_vine <- mergeESN(tbl_vine)
write_csv(mtbl_vine,paste(LocDir,"Vines.csv",sep=""))

##-----------------------------------------------------##
##...Small tree ferns < 10 cm DBH - 2 m x 50 m sensus ----
##-----------------------------------------------------##

tbl_small_tree_fern<-tbl_Sm_Woody_Tally[which(
  #tbl_Sm_Woody_Tally$Status=="Live" &
  tbl_Sm_Woody_Tally$Life_Form=="Small Tree Fern"),]
mtbl_small_tree_fern <- mergeESN(tbl_small_tree_fern)
write_csv(mtbl_small_tree_fern,paste(LocDir,"Sm_Tree_Ferns.csv",sep=""))

##----------------------------##
##...Species occurence list ----
##----------------------------##

##...tbl_Presence_simplification
tbl_Presence_s<-tbl_Presence #Removed simplification below
#[which(tbl_Presence$Dead=="FALSE"),c("Presence_ID","Event_ID","Species_ID")]
mtbl_Presence_s <- mergeESN(tbl_Presence_s)
write_csv(mtbl_Presence_s,paste(LocDir,"Presence.csv",sep=""))


##---------------------------##
##...Species coverage High...##
##---------------------------##

##...tbl_Presence_simplification
xref_Understory_High_s<-xref_Understory_High[,c("Event_ID","Point_ID","Species_ID","Dead")]


temp<-merge(xref_Understory_High_s,Event,by.x="Event_ID",by.y="Event_ID",all.x=TRUE)
temp2<-merge(temp,tlu_Points_All,by.x="Point_ID",by.y="Point_ID",all.x=TRUE)
temp3<-merge(temp2,tlu_Species_s,by.x="Species_ID",by.y="Species_ID",all.x=TRUE)
TSN_Park<-paste(temp3$Unit_Code,temp3$TSN,sep="_")
temp3<-data.frame(temp3,TSN_Park)
temp4<-merge(temp3,xref_Park_Species_Nativity_s,by.x="TSN_Park",by.y="TSN_Park",all.x=TRUE)

sum(is.na(temp4$Species_ID))

temp004 <- xref_Understory_High %>%
  # Join Event Table. full_join() insures plots with no cover are still included in table with NAs
  full_join(Event, by = "Event_ID") %>%
  # Join Points look up table. full_join() because all plots should have 300 points
  full_join(tlu_Points_All, by = "Point_ID") %>%
  # Join Species info. left_join() because we don't want species included that weren't detected on the cover line
  left_join(tlu_Species_s, by = "Species_ID") %>%
  # Create new key 'TSN_Park' to join species nativity
  unite(TSN_Park, Unit_Code, TSN, remove = FALSE) %>%
  # Join species nativity
  left_join(xref_Park_Species_Nativity_s, by = "TSN_Park")
  #filter(Dead == TRUE)

sum(is.na(temp004$Species_ID))

write_csv(temp004,paste(LocDir,"Species_coverage_High.csv",sep=""))



##---------------------------##
##...Species coverage Low...##
##---------------------------##

##...tbl_Presence_simplification
xref_Understory_Low_s<-xref_Understory_Low[,c("Event_ID","Point_ID","Species_ID","Dead")]

temp<-merge(xref_Understory_Low_s,Event,by.x="Event_ID",by.y="Event_ID",all.x=TRUE)
temp2<-merge(temp,tlu_Points_All,by.x="Point_ID",by.y="Point_ID",all.x=TRUE)
temp3<-merge(temp2,tlu_Species_s,by.x="Species_ID",by.y="Species_ID",all.x=TRUE)
TSN_Park<-paste(temp3$Unit_Code,temp3$TSN,sep="_")
temp3<-data.frame(temp3,TSN_Park)
temp4<-merge(temp3,xref_Park_Species_Nativity_s,by.x="TSN_Park",by.y="TSN_Park",all.x=TRUE)

sum(is.na(temp4$Species_ID))

temp004 <- xref_Understory_Low %>%
  # Join Event Table. full_join() insures plots with no cover are still included in table with NAs
  full_join(Event, by = "Event_ID") %>%
  # Join Points look up table. full_join() because all plots should have 300 points
  full_join(tlu_Points_All, by = "Point_ID") %>%
  # Join Species info. left_join() because we don't want species included that weren't detected on the cover line
  left_join(tlu_Species_s, by = "Species_ID") %>%
  # Create new key 'TSN_Park' to join species nativity
  unite(TSN_Park, Unit_Code, TSN, remove = FALSE) %>%
  # Join species nativity
  left_join(xref_Park_Species_Nativity_s, by = "TSN_Park")
#filter(Dead == TRUE)

sum(is.na(temp004$Species_ID))

write_csv(temp004,paste(LocDir,"Species_coverage_Low.csv",sep=""))


##------------------------------------##
##...All Species recorded checklist...##
##------------------------------------##
# All dead removed!

# Get tables for large woody (LW), Small woody (SW),
#   Understory high (UH), and Understory low (UL), and merge into
#   one table.
chk_spLW<-tbl_Lg_Woody_Individual_s[which(tbl_Lg_Woody_Individual_s$Status!="FALSE"),c("Large_Woody_ID","Event_ID","Species_ID")]
chk_spLW$tbl <- paste("LargewoodyID",chk_spLW$Large_Woody_ID,sep="_")
chk_spLW <- chk_spLW[ , -which(names(chk_spLW) %in% c("Large_Woody_ID"))]

chk_spSW<-tbl_Sm_Woody_Tally[which(tbl_Sm_Woody_Tally$Status!="FALSE"),c("Small_Woody_ID","Event_ID","Species_ID")]
chk_spSW$tbl <- paste("SmallwoodyID",chk_spSW$Small_Woody_ID,sep="_")
chk_spSW <- chk_spSW[ , -which(names(chk_spSW) %in% c("Small_Woody_ID"))]

chk_spUH<-xref_Understory_High_s[which(xref_Understory_High_s$Dead=="FALSE"),c("Point_ID","Event_ID","Species_ID")]
chk_spUH$tbl <- paste("PointID",chk_spUH$Point_ID,sep="_")
chk_spUH <- chk_spUH[ , -which(names(chk_spUH) %in% c("Point_ID"))]

chk_spUL<-xref_Understory_Low_s[which(xref_Understory_Low_s$Dead=="FALSE"),c("Point_ID","Event_ID","Species_ID")]
chk_spUL$tbl <- paste("PointID",chk_spUL$Point_ID,sep="_")
chk_spUL <- chk_spUL[ , -which(names(chk_spUL) %in% c("Point_ID"))]

mtbl_PSpp_chk <- rbind(chk_spLW, chk_spSW, chk_spUH, chk_spUL)
mtbl_PSpp_chk <- mtbl_PSpp_chk[!duplicated(mtbl_PSpp_chk[c("Event_ID", "Species_ID")]),] #removes duplicates of Event_ID + Species_ID

mtbl_PSpp_chk_s <- mergeESN(mtbl_PSpp_chk)

write_csv(mtbl_PSpp_chk_s,paste(LocDir,"PSpp_chk.csv",sep=""))

sum(is.na(mtbl_PSpp_chk_s$Name))
sum(is.na(mtbl_Presence_s$Name))

#############
##...END...##
#############
