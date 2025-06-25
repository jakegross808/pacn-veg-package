# Get iNaturalist observations for a species, user, or location

# Using httr and jsonlite ----

library("tidyverse")
library("httr")
library("jsonlite")

get_inat_pacn_obs <- function(max_id){
  # an API call that has "id_above =" at the end
  call <- paste("https://api.inaturalist.org/v1/observations?quality_grade=any&identifications=any&user_id=pacn_plants&options=photourl&per_page=200&order_by=id&order=asc&id_above=",
                max_id, sep="")

# making the API call, parsing it to JSON and then flatten
GET(url = call) %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE) -> get_call_json

# this grabs just the data we want and makes it a data frame
as.data.frame(get_call_json$results)

}


# get the first page
obs <- get_inat_pacn_obs(max_id = 0)
max_id <- max(obs[["id"]])
thisisalist <- list(page_1 = obs)
page <- 1

while (nrow(obs) == 200) {
  Sys.sleep(0.5)
  page <- page + 1
  page_count <- paste("page", page, sep = "_")
  obs <- get_inat_pacn_obs(max_id = max_id)
  thisisalist[[page_count]] <- obs
  max_id <- max(obs[["id"]])
  print(page_count)
  print(max_id)
}

all_pacn_plants_obs <- bind_rows(thisisalist)

look <- all_pacn_plants_obs |>
  dplyr::filter(taxon.name == "Cyrtandra giffardii") |>
  purrr::pluck("photos", 1, "url") |>
  stringr::str_replace_all("square", "original")

look[1]


dplyr::pull("photos") #|>
  purrr::pluck(url)
  #purrr::map_df(.f = pull(url))
  #purrr::map_dbl(~ .x |> pull(url))

obs_date <- all_pacn_plants_obs %>%
  mutate(observed_on_date = as.Date(observed_on, "%Y-%m-%d"),
         day_of_year = as.numeric(strftime(observed_on_date,
                                           format = "%j")) )

obs_date %>%
  ggplot(aes(x = observed_on_date)) +
  geom_bar(color = "#264653") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Day observed")

obs_date %>%
  ggplot(aes(x= as.factor(observed_on_details.month))) +
  geom_bar(fill = "grey90", color = "#264653") +
  labs(x= "Month observed")

obs_date %>%
  group_by(taxon.name, taxon.preferred_common_name) %>%
  summarise(median_day = median(day_of_year), n_obs = n()) %>%
  ungroup() %>%
  # filter to sp with more than 20 observations
  # to make sure we get a representitive sample size
  filter(n_obs > 20) %>%
  slice_min(order_by = median_day, n = 15) %>%
  knitr::kable()







# Old way using irnat --------------------
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

pacn_plants_obs <- get_inat_obs_user(username = "pacn_plants", maxresults = 99999)


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



