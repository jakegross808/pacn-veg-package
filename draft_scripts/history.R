
# QA/QC ----
spp_list_HAVO <- master_spp_list(veg_species_db_full_path, park = "HAVO")

pacnvegetation::qc_presence_complete(sample_frame = "Olaa")
pacnvegetation::qc_presence_complete(sample_frame = "Nahuku/East Rift")
pacnvegetation::qc_presence_complete(sample_frame = "Mauna Loa")
pacnvegetation::qc_presence_complete(sample_frame = "Kahuku")

# ---- Understory spp consistency chk ---
pacnvegetation::v_cover_bar_stats(plant_grouping = "Species",
                                  sample_frame = "Olaa",
                                  combine_strata = TRUE,
                                  cycle = c(1,2,3),
                                  plot_number = 1)

# ---- Presence spp consistency chk ---
chk_pres <- pacnvegetation::FilterPACNVeg(data_name = "Presence",
                              sample_frame = "Olaa",
                              plot_number = 2)

chk_pres <- chk_pres %>%
  mutate(Cycle = as.integer(Cycle)) %>%
  arrange(Scientific_Name)

# find the middle of the data set
df_length <- length(chk_pres$Scientific_Name)
df_middle <- df_length/2

# Which species is in the middle of the dataset
split_at_sp <- chk_pres$Code[df_middle]

# select the row after the last record for that species to split evenly accross species
split_at <- max(which(chk_pres$Code == split_at_sp)) + 1
split_at

chk_pres$split <- "first"
chk_pres$split[split_at:df_length] <- "second"

# Plot
# A function factory for getting integer y-axis values.
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# Nativity discrete scale Colors:
nativity_colors <- c("Native" = "#1b9e77",
                     "No_Veg" = "grey",
                     "Non-Native" = "#d95f02",
                     "Unknown" = "#7570b3")


ggplot(chk_pres, aes(x= Scientific_Name, y=Cycle, color = Nativity)) +
  geom_point(size=4) +   # Draw points
  geom_segment(aes(x=Scientific_Name,
                   xend=Scientific_Name,
                   y=min(Cycle),
                   yend=max(Cycle)),
               linetype="dashed",
               size=0.1) +   # Draw dashed lines
  labs(title="Check Presence",
       subtitle= (paste0(chk_pres$Sampling_Frame[1], " Plot ", chk_pres$Plot_Number[1])),
       caption= (paste0("QA/QC"))) +
  scale_color_manual(values = nativity_colors) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = integer_breaks()) +
  coord_flip() +
  facet_wrap(scales = "free", vars(chk_pres$split)) +
  theme(strip.background = element_blank(),
    strip.text.x = element_blank())








# master_spp_list ----


# Local Path to Veg Spp database
veg_species_db_folder <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
# If only one database in folder, this will grab full path:
veg_species_db_full_path <- list.files(veg_species_db_folder,full.names = TRUE)

look <- master_spp_list(db_path = veg_species_db_full_path, park = "All", presence_matrix = TRUE)

look2 <- look %>%
  select(Code, tail(names(.), 15))

# Create Abbrev Dataframe
Sampling_Frame <- c("Olaa", "Nahuku/East Rift", "Mauna Loa", "Kahuku", "Kaloko-Honokohau",
                    "Kipahulu District", "Haleakala", "Puu Alii", "Kalawao", "Hoolehua",
                    "Tutuila", "Tau", "Guam", "Muchot")
SF_Abbrev <- c("OL", "ER",	"ML",	"KU",	"KH",	"KD",	"HA",	"PA",	"KW",	"HO",	"TT",	"TA",	"GU",	"MU")

Abbrev <- data.frame(Sampling_Frame, SF_Abbrev)

# Code occurrence by Plant Community
ftpc_occ <- pacnvegetation::FilterPACNVeg(data_name = "Presence") %>%
  dplyr::left_join(Abbrev, by = join_by(Sampling_Frame)) %>%
  dplyr::mutate(SF = SF_Abbrev) %>%
  dplyr::select(SF, Sampling_Frame, Plot_Number, Cycle, Code) %>%
  # Only count a species once for a fixed plot
  dplyr::group_by(SF, Sampling_Frame, Code, Plot_Number) %>%
  dplyr::summarise(count = dplyr::n_distinct(Code), .groups = "drop") %>%
  # Get total across each plant community per park
  dplyr::group_by(SF, Code) %>%
  dplyr::summarise(plots = dplyr::n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = SF, values_from = plots, values_fill = 0)

# Get master_list from Veg Spp DB using park (Unit_Code)
park <- "All"
master_list <- pacnveg_master_spp_list %>%
  dplyr::arrange(desc(Update_date_park))

if (park == "All") {
  master_list <- pacnveg_master_spp_list %>%
    dplyr::arrange(desc(Update_date_park)) %>%
    dplyr::distinct(Species_ID, .keep_all = TRUE)
  warning("dupicate species records removed, if species is present in more than one park, only the most recently edited record is selected. For example, common name in table may be specific to NPSA and missing Hawaii common name")
} else {
  master_list <- pacnveg_master_spp_list %>%
  dplyr::filter(Park == park)
}

master_list <- master_list %>%
  dplyr::arrange(Taxonomic_Family, Genus, Species) %>%
  dplyr::left_join(ftpc_occ, by = "Code")

if (presence_matrix == TRUE) {
  return(master_list)
}

# get new ftpc occurrence columns as a vector
new_cols <- dplyr::setdiff(names(master_list), names(pacnveg_master_spp_list))
new_cols

master_list2 <- master_list %>%
  dplyr::mutate_at(.vars = new_cols, ~ replace_na(., 0)) %>%
  # Take first common name
  tidyr::separate(col = Park_common_name, into = "common1", sep = ",",
                  extra = "drop", remove = FALSE ) %>%
  dplyr::mutate(LF = tidyr::replace_na(Life_form_park, replace = "_NO LIFEFORM_")) %>%
  dplyr::mutate(presence_rank = rowSums(across(all_of(new_cols)))) #%>%
  #dplyr::mutate(pres_rank_text = stringr::str_pad(presence_rank, 3, pad = "0"))


# Add column name to each value of each occurrence column

# var <- "mpg"
# Doesn't work: mtcars$var
# These both work, but note that what they return is different
# the first is a vector, the second is a data.frame
# mtcars[[var]]   dataframe
# mtcars[var]     vector

for (col in new_cols) {
  master_list2[new_cols][col] <- paste0(col, ":", master_list2[new_cols][[col]])
}

# Unit the new ftpc occurrence columns into one column:
master_list3 <- master_list2 %>%
  tidyr::unite("PC_presence", all_of(new_cols), sep = ", ")


if (length(new_cols) > 1) {
  master_list4 <- master_list3 %>%
    dplyr::mutate(FTPC_pres = paste0(presence_rank, " (", PC_presence, ")"))
} else {
  master_list4 <- master_list3 %>%
    dplyr::mutate(FTPC_pres = paste0(" (", PC_presence, ")"))
}

master_list5 <- master_list4 %>%
  dplyr::mutate(Field_ID = paste0(Scientific_name, " (", Code, ") ",
                                  Taxonomic_Family, " / ", Nativeness, " / ", LF,
                                  " / " , common1, " / ", FTPC_pres, " [syn: ", Synonym, "]")) %>%
  dplyr::arrange(desc(presence_rank), Field_ID) %>%
  dplyr::select(-Species_ID, -TSN, -common1, -Life_form, -Life_cycle, -Omit_in_NPSpecies, -Complete,
                -TSN_park, -PC_presence, -LF)








# figure out why new entries are not showing in spp DB----

# Issue was trying to join by "TSN" apparently some of the new records to not
# have a "TSN" number so just removed and instead just joined by "Species_ID" only
veg_species_db_path <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
veg_species_db <- list.files(veg_species_db_path,full.names = TRUE)
veg_species_db

c_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", veg_species_db)

# Establish connection with access database
c <- DBI::dbConnect(odbc::odbc(), .connection_string = c_string)

  # Download Species info table
tlu_Species <- dplyr::tbl(c, "tlu_Species") %>%
  dplyr::collect()
  names(tlu_Species)

  # Download park specific Species checklists
xref_Park_Species <- dplyr::tbl(c, "xref_Park_Species") %>%
  dplyr::collect()

  # Join Park Species checklists to with the additional species information
pacnveg_master_spp_list2 <- tlu_Species %>%
  dplyr::right_join(xref_Park_Species, by = c("Species_ID"), suffix = c("","_park"))

  DBI::dbDisconnect(conn)



# Kevin Spp Occurrence data request ----
selected_spp <- c(
  "Tibouchina herbacea", "Leptospermum scoparium","Toona ciliata",
  "Pinus luchuensis", "Angiopteris evecta","Sphaeropteris cooperi = (Cyathea cooperi)",
  "Pterolepis glomerata","Elephantopus mollis","Melochia umbellata",
  "Spathodea campanulata","Rhodomyrtus tomentosa",
  "Erigeron karvinskianus","Hedychium gardnerianum","Oxyspora paniculata",
  "Alstonia macrophylla")

# Not_found <- "Leptospermum scoparium" ,"Toona ciliata", "Pinus luchuensis",
# "Angiopteris evecta", "Pterolepis glomerata", "Melochia umbellata", "Rhodomyrtus tomentosa",
# "Oxyspora paniculata", "Alstonia macrophylla"

# 109 species occurrences from veg map:
hi_vegmap_spp <- hi_vegmap_data %>%
  filter(!is.na(lat), !is.na(long)) %>%
  filter(Sci_Name %in% c(selected_spp))

Vegmap <- hi_vegmap_spp %>%
  select(Plot_Code, Event_Date, Family, Sci_Name, ComName, lat, long, GPS_Error)

unique(Vegmap$Sci_Name)

write_csv2(Vegmap, file = paste0("C:/Users/JJGross/Downloads/Vegmap_", Sys.Date() ,".csv"))

# FTPC
names(FilterPACNVeg())
hi_FTPC_spp <- FilterPACNVeg(data_name = "Presence", is_qa_plot = FALSE) %>%
  filter(Unit_Code %in% c("HALE", "HAVO", "KALA", "KAHO")) %>%
  select(-QA_Plot) %>%
  filter(Scientific_Name %in% c(selected_spp))

pts_FTPC <- FilterPACNVeg(data_name = "Events_extra_xy", is_qa_plot = FALSE) %>%
  select(-QA_Plot) %>%
  right_join(y = hi_FTPC_spp, by = join_by(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number))

unique(hi_FTPC_spp$Scientific_Name)

FTPC <- pts_FTPC %>%
  select(Unit_Code, Community, Sampling_Frame, Cycle, Year,
         Plot_Number, Plot_Type, Start_Lat, Start_Long,
         Scientific_Name, Code, Nativity, Outside_Plot, cf, Certified = Certified.y) %>%
  mutate(Nativity = case_when(Scientific_Name == "Angiopteris evecta" ~ "Non-Native",
                               .default = as.character(Nativity)))
write_csv2(FTPC, file = paste0("C:/Users/JJGross/Downloads/FTPC_", Sys.Date() ,".csv"))

# EIPS

test_data <- v_EIPS_prep() %>%
  filter(Unit_Code %in% c("HALE", "HAVO", "KALA", "KAHO")) %>%
  filter(Scientific_Name %in% c(selected_spp))

# Mean Species Cover by inter-station
station_summary <- test_data %>%
dplyr::group_by(Unit_Code, Community, Sampling_Frame, Cycle, Year,
                Transect_Type, Transect_Number, Start_Station_m, End_Station_m,
                Seg_Length_m, Segs_Per_Station, Meters_Per_Station,
                Code, Scientific_Name, Life_Form, Nativity) %>%
  dplyr::summarize(Actual_Segs = dplyr::n_distinct(Segment),
                   Tot_Station_Cov_Min = sum(Cov_Range_Min),
                   Tot_Station_Cov_Max = sum(Cov_Range_Max)) %>%
  dplyr::mutate(Actual_Meters = Actual_Segs * Seg_Length_m,
                Mean_Seg_Cov_Min = Tot_Station_Cov_Min/Actual_Segs,
                Mean_Seg_Cov_Max = Tot_Station_Cov_Max/Actual_Segs)

ipts_EIPS <- FilterPACNVeg(data_name = "EIPS_image_pts") %>%
  filter(Unit_Code %in% c("HALE", "HAVO", "KALA", "KAHO"))

#Change Year to first year of the Cycle and prep data
ipts_EIPS2 <- ipts_EIPS %>%
  dplyr::group_by(Unit_Code, Sampling_Frame, Cycle) %>%
  dplyr::mutate(Year = min(Year)) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Transect_Number = as.factor(Transect_Number)) %>%
  dplyr::select(-Latitude_Dir, -Longitude_Dir, -GCS, -GPS_Error)

#join to GPS points:
station_summary2 <- station_summary %>%
  dplyr::mutate(Start_Image_Point = as.character(Start_Station_m)) %>%
  dplyr::left_join(ipts_EIPS2, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "Start_Image_Point" = "Image_Point")) %>%
  dplyr::rename(Lat_Start = Latitude,
                Long_Start = Longitude) %>%
  dplyr::mutate(End_Image_Point = as.character(End_Station_m)) %>%
  dplyr::left_join(ipts_EIPS2, by = c("Unit_Code", "Community", "Sampling_Frame", "Cycle", "Year", "Transect_Type", "Transect_Number", "End_Image_Point" = "Image_Point")) %>%
  dplyr::rename(Lat_End = Latitude,
                Long_End = Longitude) %>%
  dplyr::ungroup()

EIPS_start_stations <- station_summary2 %>%
  select(Unit_Code, Community, Sampling_Frame, Cycle, Year,
         Transect_Number, Transect_Type, Start_Station_m, Meters_Per_Station,
         Scientific_Name, Code, Nativity,
         Start_Image_Point, Lat_Start, Long_Start)

unique(EIPS_start_stations$Scientific_Name)

EIPS <- EIPS_start_stations

write_csv2(EIPS, file = paste0("C:/Users/JJGross/Downloads/EIPS_", Sys.Date() ,".csv"))






# Get labels for herbarium voucher specimens ----
library(tidyverse)

# Load ED plant records that need voucher labels:
gdb <- "C:/Users/JJGross/Downloads/ED_HAVO_2022_new/ED_HAVO_2022_new.gdb"
gdb_layer <- "ED_HAVO_2022"
ED_HAVO_2022 <- sf::read_sf(gdb, gdb_layer)
ED_HAVO_2022_vouchers <- ED_HAVO_2022 %>%
  filter(Specimen == "Yes")
ED_HAVO_2022_vouchers$Code <- stringr::str_extract(ED_HAVO_2022_vouchers$Photo_Taxon, "(?<=\\().*?(?=\\))")


# Load ED plant records that need voucher labels:
veg_species_db_path <-  "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
veg_species_db <- list.files(veg_species_db_path,full.names = TRUE)
veg_species_db
pacnveg_master_spp_list <- read_spp_db(veg_species_db) %>%
  filter(Park == "HAVO")

# Specimens already known to park
join1 <- ED_HAVO_2022_vouchers %>%
  filter(New_Species == "No") %>%
  dplyr::left_join(y = pacnveg_master_spp_list, by = "Code")

# Specimens new to park (Veg Spp Database)
join2 <- ED_HAVO_2022_vouchers %>%
  filter(New_Species != "No") %>%
  dplyr::left_join(y = pacnveg_master_spp_list, by = c("New_sp_name" = "Scientific_name"))







# Veg Spp Database ----

dbpath <- "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db/PACN_veg_species_list_20230810.accdb"
pacnveg_master_spp_list <- read_vegspp_db(dbpath)





#' Load and Query The PACN Veg Species Database
#' Vital Signs > 05_focal_terr_plant_communities > Data > Database > Veg_species_db
#'
#' @param db_paths Database path (downloaded from sharepoint to location on computer)
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' dbpath <- "C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db/PACN_veg_species_list_20230810.accdb"
#' pacnveg_master_spp_list <- read_vegspp_db(dbpath)
#'
#' }
read_vegspp_db <- function(db_paths) {
  conn_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", db_paths)

  # Establish connection with access database
  conn <- DBI::dbConnect(odbc::odbc(), .connection_string = conn_string)

  # Download Species info table
  tlu_Species <- dplyr::tbl(conn, "tlu_Species") %>%
    dplyr::collect()
  names(tlu_Species)

  # Download park specific Species checklists
  xref_Park_Species <- dplyr::tbl(conn, "xref_Park_Species") %>%
    dplyr::collect()

  # Join Park Species checklists to with the additional species information
  pacnveg_master_spp_list <- tlu_Species %>%
    dplyr::right_join(xref_Park_Species, by = c("Species_ID", "TSN"), suffix = c("","_park"))

  return(pacnveg_master_spp_list)

}



