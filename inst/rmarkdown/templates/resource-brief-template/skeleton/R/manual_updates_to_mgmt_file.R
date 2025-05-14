
# Events_extra_xy_mgmt.csc

# These changes need to be made to AGOL layer:
# https://gcc02.safelinks.protection.outlook.com/?url=https%3A%2F%2Fnps.maps.arcgis.com%2Fhome%2Fitem.html%3Fid%3D669e2de095c44b3bb25ac4050a30d64c&data=05%7C02%7CJacob_Gross%40nps.gov%7C8f377a192d8542076ca708dd8910975d%7C0693b5ba4b184d7b9341f32f400a5494%7C0%7C0%7C638817426525849390%7CUnknown%7CTWFpbGZsb3d8eyJFbXB0eU1hcGkiOnRydWUsIlYiOiIwLjAuMDAwMCIsIlAiOiJXaW4zMiIsIkFOIjoiTWFpbCIsIldUIjoyfQ%3D%3D%7C0%7C%7C%7C&sdata=j7p7DNNfmFlr039t1Kc3fklNmXhkzBoe1ekcJowkGMs%3D&reserved=0
# whenever Mark can get to making the changes and making layer public again...

# This file is a hard copy which is intended to be periodically updated using
# the add_mgmt_unit() function from the spatial.R file

# The code below makes changes to the mgmt csv file directly instead of the spatial
# join (between plots/transects and sampling frame public view layer) intended in the add_mgmt_unit() function

read_mgmt_location <- "C:/Users/JJGross/Documents/R_projects/pacnvegetation/inst/rmarkdown/templates/resource-brief-template/skeleton/R/Events_extra_xy_mgmt.csv"
mgmt <- readr::read_csv(file = read_mgmt_location)

mgmt2 <- mgmt |>
  mutate(Zone = case_when(is.na(Zone) ~ Sampling_Frame, .default = Zone))

mgmt2 |>
  filter(Sampling_Frame == "Hoolehua" & Zone != "Sandy")
# Plot number 4
mgmt3 <- mgmt2 |>
  mutate(Zone = case_when((Sampling_Frame == "Hoolehua" & Zone != "Sandy") ~ "Sandy", .default = Zone))
mgmt3 |>
  filter(Sampling_Frame == "Hoolehua" & Zone != "Sandy")
# None

mgmt2 |>
  filter(Sampling_Frame == "Kalawao" & Zone != "Rocky")
# None


# overwrite with updates:
readr::write_csv(mgmt3, file = read_mgmt_location)
