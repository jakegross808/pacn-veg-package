# Get iNaturalist observations for a species, user, or location

library(tidyverse)
library(rinat)
library(leaflet)

# Hawaii County <- place_id = 2351
# Hawaii Volcanoes National Pakr <- place_id = 7222
# All Vascular Plants <- taxon_id = 211194
# "Pseudognaphalium attenum" <- taxon_id = 167487

# Pull observations from iNaturalist website
PSEATT <- rinat::get_inat_obs(taxon_id = 167487,
                              place_id = 7222)


leaflet(data = PSEATT) %>%
  addTiles() %>%
  addCircleMarkers(radius = 5,
                   color= "red",
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   popup = ~paste("<a href=\"", image_url , "\">", "photo", "</a>"))

HAVO_2022 <- rinat::get_inat_obs(place_id = 7222, taxon_id = 211194, maxresults = 10000, year = 2022)

HAVO_2022_distinct <- HAVO_2022 %>%
  dplyr::distinct(scientific_name) %>%
  mutate(scientific_name = map_chr(scientific_name, ~stringr::str_replace(.x ," × ", "_x_"))) %>%
  tidyr::separate(col = scientific_name, sep = " ", into = c("genus", "specific name", "intraspecific name"))


