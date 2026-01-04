library(pacnvegetation)
library(tidyverse)

#--- 1. alternatively Read from "data" folder
# Load the data (use csv files)
data <- here::here("data", "vital_signs")
path_file_info <- file.info(list.files(data, full.names = T))
latest_folder <- rownames(path_file_info)[which.max(path_file_info$mtime)]

LoadPACNVeg(data_path = latest_folder,
            data_source = "file")


# ---
names(FilterPACNVeg())

all_presence <- FilterPACNVeg(data_name = "Presence")

Haleakala_presence <- FilterPACNVeg(data_name = "Presence", sample_frame = "Haleakala")




all_cover <- FilterPACNVeg(data_name = "Understory")

all_EIPS <- FilterPACNVeg(data_name = "EIPS_data")

all_shrubbelt <- FilterPACNVeg(data_name = "SmWoody")

small_tree <- all_shrubbelt |>
  filter(LF_Sm_Woody == "Small Tree")

all_shrubbelt |>
  select(LF_Sm_Woody) |>
  unique()
