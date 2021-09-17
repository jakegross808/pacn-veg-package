
library(DBI)
library(dbplyr)
library(tidyverse)

DB <- dbConnect(odbc::odbc(), "pacnveg")


# DATA TABLES ----

#1 Lrg_Woody_Individual --------------------------------------------------------

# Tree density information collected for small trees (subalpine),
# large trees (forest, coastal, subalpine), and large tree ferns (forest).
tbl(DB, "tbl_Lg_Woody_Individual") %>%
colnames()

tbl_Lg_Woody_Individual <- tbl(DB, "tbl_Lg_Woody_Individual") %>%
  select(Large_Woody_ID, Event_ID, Species_ID, Life_Form, Quad, Status, Height,
         Height_Dead, Boles, DBH, DBH_Basal, Vigor, Fruit_Flower, Rooting, Foliar,
         Caudex_Length, Shrublike_Growth, Resprouts, Measurement_Type)
#select(-Sort_Order, -SSMA_TimeStamp)

# ....subtbl Multiple_Boles ----
##Bole DBH for trees that have multiple boles.
tbl(DB, "tbl_Multiple_Boles") %>%
  colnames()

tbl_Multiple_Boles <- tbl(DB, "tbl_Multiple_Boles") %>%
  select(Large_Woody_ID, DBH_Bole, Root_Sprout, Status_Bole)
#select(-Bole_ID, -SSMA_TimeStamp)

# ....subtbl Snags ----
# Extra data for dead trees
tbl(DB, "tbl_Snags") %>%
  colnames()
tbl_Snags <- tbl(DB, "tbl_Snags")%>%
  select(Large_Woody_ID, Basal_diameter, Height_Snag)
#select(-Snag_ID, -Sort_Order, -SSMA_TimeStamp)

# ....subtbl Basal_Sprout ----
# Sprout size data collected in recently disturbed sites for individuals
# that have been top killed and are alive only by basal sprouts.

tbl(DB, "tbl_Basal_Sprout") %>%
  colnames()

tbl_Basal_Sprout <- tbl(DB, "tbl_Basal_Sprout") %>%
  select(Large_Woody_ID, Sprout_Ht, W1, W2, Area, Volume)
  #select(-SSMA_TimeStamp, -Sprout_ID)




#2 Tree_Canopy_Height ----------------------------------------------------------
# Recorded within each plot by measuring the height of ~five canopy trees per
# plot that represent the average canopy height within the plot.
tbl(DB, "tbl_Tree_Canopy_Height") %>%
  colnames()

tbl_Tree_Canopy_Height <- tbl(DB, "tbl_Tree_Canopy_Height") %>%
  select(Event_ID, Species_ID, Quad, Status, Top, Base, Base_ht,
         Distance, Height, Method, DBH, Comments)
  #select(-Tree_Height_ID, -Sort_Order, -SSMA_TimeStamp)





#3 Presence --------------------------------------------------------------------
#List of species present within plot.
tbl(DB, "tbl_Presence") %>%
  colnames()

tbl_Presence <- tbl(DB, "tbl_Presence") %>%
  select(Event_ID, Species_ID, Fruit_Flower, Dead, Outside_Plot, cf, Comments)
#select(-Presence_ID, -Sort_Order, -SSMA_TimeStamp)
tbl_Presence




#4 Sm_Woody_Tally --------------------------------------------------------------
# Count data, foliar height, and rooting height data tallied for
# vines, seedlings, shrubs, small trees, and small tree ferns.

tbl(DB, "tbl_Sm_Woody_Tally") %>%
  colnames()

tbl_Sm_Woody_Tally <- tbl(DB, "tbl_Sm_Woody_Tally") %>%
  select(Event_ID, Species_ID, Transect, Life_Form, DBH, Status,
         Foliar, Rooting, Count, Comments)
#select(-Small_Woody_ID, -Sort_Order, -SSMA_TimeStamp)



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





#5 Woody Debris ----------------------------------------------------------------
# Dead, downed wood and tree fern logs
# with a diameter >=7.6 at the point of planar intersection.

tbl(DB, "tbl_Woody_Debris") %>%
  colnames()

tbl_Woody_Debris <- tbl(DB, "tbl_Woody_Debris") %>%
  select(Woody_Debris_ID, Event_ID, Transect, Length)
#select(-Sort_Order, -SSMA_TimeStamp)


# ....subtbl Debris_Species  ----
# Additional information on debris type (wood/tree fern), diameter,
# and decay class

tbl(DB, "tbl_Debris_Species") %>%
  colnames()

tbl_Debris_Species <- tbl(DB, "tbl_Debris_Species") %>%
  select(Woody_Debris_ID, Debris_type, Diameter, Decay_Class, Comments) %>%
  #select(-Debris_Species_ID, -Sort_Order, SSMA_TimeStamp)
  collect()



# SPECIES --------------------------------------------------------------------------

