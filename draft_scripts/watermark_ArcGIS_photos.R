# Author: Jacob Gross jacob_gross@nps.gov
# Date: April 12th 2022

# Install these packages if not already installed (if installed, skip this line)
install.packages(c("here", "tidyverse", "sf", "magick"))

# Load the packages:
library(here) # Helps working with relative file paths
library(tidyverse) # numerous packages that make R programming more readable
library(sf) # simple features spatial package for R
library(magick) # photo editing packages (for watermarking photos)

# Return current working folder
here()

# Create a folder called "geodatabase" for the geodatabase
dir.create(here("geodatabase"))
gdb_folder <- here("geodatabase")

# Now go to file explorer and move or copy the geodatabase (.gdb)
# to the "geodatabase" folder just created:
gdb_folder
# The geodatabase needs to contain both the layer file and
# the attachments table (__ATTACH) containing the photos

# Enter the name of the geodatabase (replace my_geodatabase.gdb with correct name):
gdb <- "my_geodatabase.gdb"

# path to geodatabase:
gdb_path <- here(gdb_folder, gdb)

# display layers inside geodatabase:
st_layers(gdb_path)

# Photos are stored in the "__ATTACH table" and will have geometry_type == NA
# Copy and past both the layer name and the _ATTACH table below
# (replace my_layer and my_layer__ATTACH with correct name):
gdb_layer_name <- "my_layer"
gdb_attach_name <- "my_layer__ATTACH"

# read the layer:
gdb_layer <- sf::read_sf(gdb_path, gdb_layer_name)
gdb_layer

# read the layer__ATTACH table:
gdb_attach <- sf::read_sf(gdb_path, gdb_attach_name)
gdb_attach

# The "DATA" Column in the _attach table contains the raw binary (BLOB) for each photo
# This is what the first raw photo in the table looks like:
gdb_attach$DATA[[1]]

# Note that the class of the DATA column (containing the BLOB) is a list
class(gdb_attach$DATA)
str(gdb_attach$DATA)

# (optional) If you tell R that the field is a BLOB, then you can view table in Rstudio
# gdb_attach$DATA <- blob::as_blob(gdb_attach$DATA)
# ... However, if using automation the apply() function doesn't like blobs
# so make sure to change column back to a list before feeding into apply()
# gdb_attach$DATA <- as.list(gdb_attach$DATA)

# also note that the objects within the list are recognized as "raw" by R
gdb_attach$DATA[[1]]
class(gdb_attach$DATA[[1]])
str(gdb_attach$DATA[[1]])

# select the first photo to use as demo
test_photo <- gdb_attach$DATA[[1]]

# save the first photo as a jpg
filename <- "renamed_photo"
fc <- file(paste0(filename, ".jpg"), "wb")    # OPEN FILE CONNECTION
writeBin(test_photo, con = fc, useBytes=TRUE) # TRANSFER RAW DATA, useBytes=TRUE passes blob byte-by-byte to file without re-encoding
close(fc)                                     # CLOSE FILE CONNECTION
# look in working directory to see saved photo
# use here() to see where working directory is:
here()

# Test watermarking capabilities:
# this is the test photo:
str(test_photo)

# read the photo using "magick" package function "image_read()"

# Load photo
img <- image_read(test_photo)
print(image_attributes(img))


# ---- Watermark photo -----

# create labels in the corner of photos. Examples with text options:

# northwest corner
nw1 <- "PARK UNIT"
nw2 <- "sampling frame"
nw <- paste(nw1, nw2, sep = "\n")
img <- image_annotate(img, nw,
                        size = 25,
                        gravity = "northwest",
                        font = "Helvetica",
                        color = "white",
                        strokecolor = "black",
                        weight = 900)

# northeast corner
ne <- paste("PROTOCOL", "subject", sep = "\n")
img <- image_annotate(img, ne,
                        size = 25,
                        gravity = "northeast",
                        font = "Helvetica",
                        color = "white",
                        strokecolor = "black",
                        weight = 900)
# southwest corner
sw <- paste("YYYYMMDD")
img <- image_annotate(img, sw,
                        size = 25,
                        gravity = "southwest",
                        font = "Helvetica",
                        color = "white",
                        strokecolor = "black",
                        weight = 900)

# Save photo
image_write(img, path = here("watermarked_demo.jpg"), format = "jpg")
# Open "watermarked_demo.jpg" to see watermarked photo



# - Automation -----------------------------

# To automate process, create a function that
# pulls information straight from the layer file

# First step is to "join" or "relate" the layer data with the _attach table
joined_table <- gdb_attach %>%
  dplyr::left_join(gdb_layer, by = c("REL_GLOBALID" = "GlobalID"))
head(joined_table)

# Make a date_time column appropriate for file names
joined_table <- joined_table %>%
  mutate(date_time_photo = as.character(created_date)) %>%
  mutate(date_time_file = str_replace_all(created_date, ":", "")) %>%
  mutate(date_time_file = str_replace_all(date_time_file, " ", "_"))

str(joined_table$created_date[1])
str(joined_table$date_time_photo[1])
paste(joined_table$date_time_file[1])

# Create a R function to apply to each photo (i.e. each row of the joined table)
watermark <- function(x, new_folder) {
  # Get watermarking info from the table (x)
  p.dt_photo <- x["date_time_photo"]
  p.name <- x["ATT_NAME"]
  p.user <- x["created_user"]
  p.dt_file <- x["date_time_file"]

  # Create paths and folders to save each photo
  dir.create(here(new_folder), recursive = TRUE, showWarnings = FALSE )
  out.path <- here(new_folder)
  out.name <- file.path(out.path, paste0(p.dt_file,".jpg"))
  print(out.name)

  # Load photo
  image.x <- x["DATA"] %>%
    purrr::pluck(1)

  img.x <- image_read(image.x)


  # ---- Watermark photo -----

  # northwest corner
  nw <- paste(p.name)
  img.x <- image_annotate(img.x, nw,
                          size = 25,
                          gravity = "northwest",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  # northeast corner
  ne <- paste(p.user)
  img.x <- image_annotate(img.x, ne,
                          size = 25,
                          gravity = "northeast",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  # southwest corner
  sw <- paste(p.dt_photo)
  img.x <- image_annotate(img.x, sw,
                          size = 25,
                          gravity = "southwest",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)

  # Save photo
  image_write(img.x, path = out.name, format = "jpg")

}

# Run the function above on the "joined_table"
apply(X = joined_table, MARGIN = 1, FUN = watermark, new_folder = "watermarked")
# open "watermarked" folder in working path to see results

