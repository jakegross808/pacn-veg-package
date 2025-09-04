library(pacnvegetation)
library(tidyverse)
library(leaflet.esri)
library(sp)

LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_1_2021_20220120.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
            cache = TRUE, force_refresh = FALSE)

MapPACNVeg(protocol = c("FTPC"), sample_frame = "Guam")

sites <- PlotAndTransectLocations(protocol = c("FTPC", "EIPS"), park = "WAPA")


# Make sf object from dataframe with coordinates:
sites_sf <- st_as_sf(sites, coords = c("Long", "Lat"))
sites_sf
str(sites_sf)
st_crs(sites_sf) <- 4326
st_crs(sites_sf)
st_write(sites_sf, "C:/Users/JJGross/Downloads/sf/sites_sf2", driver = "ESRI Shapefile")
# to force the save:
st_write(sites_sf, "C:/Users/JJGross/Downloads/sf/sites_sf2", driver = "ESRI Shapefile", delete_layer = TRUE)

# Make sf object from AGOL REST:
library(httr)

url <- parse_url("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services")
url$path <- paste(url$path, "USA_Railroads_1/FeatureServer/0/query", sep = "/")
url$query <- list(where = "STATE = 'FL'",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)
request
Florida_Railroads <- st_read(request)


#agol_samp_frame <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/ArcGIS/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0?f=pjson"

url <- parse_url("https://services1.arcgis.com/fBc8EJBxQRMcHlei/ArcGIS/rest/services")
url$path <- paste(url$path, "/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0/query", sep = "/")
url$query <- list(where = "Unit_Code = 'WAPA'",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)
request
sf_samp_frame <- st_read(request)

sf_samp_asan <- sf_samp_frame %>%
  filter(Zone == "Asan")

pts_asan <- sites_sf[sf_samp_asan, ]

# sp ----
# Make a spatial lines object:
runif(7,min = 1, max = 10)

ln1 <- Line(matrix(runif(6), ncol=2))
ln2 <- Line(matrix(runif(6), ncol=2))
lns1 <- Lines(list(ln1), ID = c("hwy1"))
lns1
lns2 <- Lines(list(ln2), ID = c("hwy2"))
str(lns1)
sp_lns <- SpatialLines(list(lns1, lns2))
str(sp_lns)
sp_lns
# dataframe to join to spatial libes object
dfr <- data.frame(id = c("hwy1", "hwy2"), # note how we use the same IDs from above!
                  cars_per_hour = c(78, 22))
sp_lns_dfr <- SpatialLinesDataFrame(sp_lns, dfr, match.ID = "id")
str(sp_lns_dfr)
# simple plot
spplot(sp_lns_dfr)


# sf ----
library(sf)
lnstr_sfg1 <- st_linestring(matrix(runif(6), ncol=2))
lnstr_sfg2 <- st_linestring(matrix(runif(6), ncol=2))
class(lnstr_sfg1)
(lnstr_sfc <- st_sfc(lnstr_sfg1, lnstr_sfg2)) # just one feature here
lnstr_sfc
(lnstr_sf <- st_sf(dfr , lnstr_sfc))
# sf object
lnstr_sf %>%
  ggplot2::ggplot(lnstr_sf)
# can display in ggplot:
ggplot(lnstr_sf) +
  geom_sf()
#works with tidyverse:
lnstr_sf %>%
  filter(id == "hwy1") %>%
  ggplot2::ggplot() +
  geom_sf()






m
m %>%
  addLegend()

MapPACNVeg(protocol = c("FTPC", "EIPS"), sample_frame = "Olaa")

leaflet() %>%
  addEsriBasemapLayer(esriBasemapLayers$NationalGeographic) %>%
  setView(lat = 19.478431, lng =  -155.239881, 12) %>%
  leaflet.esri::addEsriFeatureLayer(url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/PACN_DBO_VEG_sampling_frames_ply/FeatureServer/0",
                      useServiceSymbology = TRUE,
                      labelProperty = "Sampling_Frame")
    #,
    #useServiceSymbology = TRUE,
    #labelProperty = "COMMON_NAM", labelOptions = labelOptions(textsize = "12px"),
    #popupProperty = JS(paste0(
    #  "function(feature) {",
    #  "  return L.Util.template(",
    #  "    \"<h3>{COMMON_NAM}</h3><hr />",
    #  "      <p>This tree is located at {ADDRESS} and its scientific name is {SCIENTIFIC}.</p>",
    #  "    \",",
    # "    feature.properties",
    #  "  );",
    #  "}"
    #)))

## for more examples see
# browseURL(system.file("examples/featureLayers.R", package = "leaflet.esri"))
# browseURL(system.file("examples/multipleFeatureLayers.R", package = "leaflet.esri"))



MapCoverChange(crosstalk = TRUE,
               sample_frame = params$sample_frame,
               cycle = max(cycles),
               paired_cycle = min(cycles))





check <- PlotAndTransectLocations(protocol = c("FTPC", "EIPS"), sample_frame = "Olaa", cycle = 3)
