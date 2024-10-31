# FTPC Photos ---------

# Load FTPC data
chk <- process_photos(AGOL_Layer = "FTPC",
                      gdb_name = "FTPC_OL_ER_20220503.gdb",
                      gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                      gdb_layer = "FTPC_OL_ER_20220503",
                      return_table = TRUE)



# Get list of missing fixed photos
chk_fixed <- chk %>%
  filter(Site_Type == "Fixed") %>%
  filter(Subject1 != "Staff_Photo") %>%
  filter(Subject1 != "Other") %>%
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::ungroup() %>%
  complete(tidyr::nesting(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type), Subject1) %>%
  filter(is.na(n))

# Get list of missing rotational photos
chk_rotational <- chk %>%
  filter(Site_Type == "Rotational") %>%
  filter(Subject1 != "Staff_Photo") %>%
  filter(Subject1 != "Other") %>%
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::ungroup() %>%
  complete(tidyr::nesting(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type), Subject1) %>%
  filter(is.na(n))

# Combine fixed and rotational into one table
chk_missing <- bind_rows(chk_fixed, chk_rotational)


# Create a table of duplicate points
# missing photos may be hiding as a mislabeled point,
# if so, it would likely be a duplicate

chk_dupes <- chk %>%
  # Count number of photos per subject
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, REL_GLOBALID) %>%
  dplyr::summarize(n_photos = n()) %>%
  # Count number of points per subject (disregards multiple photos at one point)
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  dplyr::summarize(n_points = n()) %>%
  filter(n_points > 1) %>%
  filter(Subject1 != "Other")

# join with original data to check created date, etc.
dupes <- chk_dupes %>%
  dplyr::left_join(chk) %>%
  select(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, n_points, Staff_list, created_date, last_edited_date, last_edited_user)

# EIPS Photos ---------

# Load EIPS data
EIPS_chk <- process_photos(AGOL_Layer = "EIPS",
                           gdb_name = "EIPS_OL_ER_20220502.gdb",
                           gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                           gdb_layer = "EIPS_OL_ER_20220502",
                           return_table = TRUE)

# Look for missing points
EIPS_missing <- EIPS_chk %>%
  # remove subjects that are not photo points
  filter(!Subject_EIPS == "Staff" & !Subject_EIPS == "Other") %>%
  tidyr::separate(Subject_EIPS, sep = "_", into = c("distance", "direction"), remove = FALSE) %>%
  dplyr::group_by(Sampling_Frame, Site_numb, Site_Type, distance) %>%
  summarise(n_direct = n_distinct(direction)) %>%
  filter(n_direct != 3 & Site_Type == "Fixed" |
           n_direct != 2 & Site_Type == "Rotational" )

EIPS_chk_dupes <- EIPS_chk %>%
  # Count number of photos per subject
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, REL_GLOBALID) %>%
  dplyr::summarize(n_photos = n()) %>%
  # Count number of points per subject (disregards multiple photos at one point)
  dplyr::group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  dplyr::summarize(n_points = n()) %>%
  filter(n_points > 1)

# join with original data to check created date, etc.
EIPS_dupes <- EIPS_chk_dupes %>%
  dplyr::left_join(EIPS_chk) %>%
  select(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, n_points, Staff_list, created_date, last_edited_date, last_edited_user)



# Code to Rotate an upsidedown image -------------

upsidedown <- chk %>%
  filter(Samp_Frame == "OL",
         Site_numb == "EIPS 05",
         Subject_EIPS == "600m_Post")

# apply() function doesn't like blobs so change to list before running apply()
upsidedown$DATA <- as.list(upsidedown$DATA)
# Load photo
library(magick)
image_r <- image_read(upsidedown$DATA[[1]])
image_r
image_r <- image_flip(image_r)
upsidedown$DATA[[1]] <- image_write(image_r)
image_r <- image_read(upsidedown$DATA[[1]])
image_r
# applyr the "watermark" function to each record (ie photo)
apply(X = upsidedown, MARGIN = 1, FUN = watermark, new_folder = "upsidedown")

# --------------------------------

# example of function parameters to process photos

process_photos(AGOL_Layer = "EIPS",
               gdb_name = "EIPS_OL_ER_20220502.gdb",
               gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
               gdb_layer = "EIPS_OL_ER_20220502",
               return_table = FALSE)


# ------------------------------------------------------------------------------


# Fix specific photos (plots)

update_photos <- process_photos(AGOL_Layer = "FTPC",
                                gdb_name = "FTPC_OL_ER_20220503.gdb",
                                gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                                gdb_layer = "FTPC_OL_ER_20220503",
                                return_table = TRUE)

update_photos <- update_photos %>%
  mutate(samp_plot = paste(Samp_Frame,Site_Number, sep = "_"))

distinct(update_photos$Site_numb)

plots2update <- c("OL_2",
                  "OL_3",
                  "OL_4",
                  "OL_35",
                  "OL_47",
                  "OL_51",
                  "OL_53",
                  "OL_57",
                  "ER_46",
                  "ER_54",
                  "ER_55")

#plots2update <- "OL_47"

plots2update

update_photos2 <- update_photos %>%
  filter(samp_plot %in% plots2update) %>%
  mutate(DATA = as.list(DATA))

apply(X = update_photos2, MARGIN = 1, FUN = watermark, new_folder = "watermark_EIPS_20220922")


# ------------------------------------------------------------------------------
# Fix specific photos (transects)

update_photos <- process_photos(AGOL_Layer = "EIPS",
                                gdb_name = "EIPS_OL_ER_20220502.gdb",
                                gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                                gdb_layer = "EIPS_OL_ER_20220502",
                                return_table = TRUE)

update_photos <- update_photos %>%
  mutate(samp_plot = paste(Samp_Frame,Site_Number, sep = "_"))

plots2update <- c("OL_2",
                  "OL_3",
                  "OL_4",
                  "OL_35")

#plots2update <- "OL_47"

update_photos2 <- update_photos %>%
  filter(samp_plot %in% plots2update) %>%
  mutate(DATA = as.list(DATA))

update_photos2$Site_numb

apply(X = update_photos2, MARGIN = 1, FUN = watermark, new_folder = "watermark_EIPS__20220922")
