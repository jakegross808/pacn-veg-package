# ----Merge NPSpecies Lists ----

# Pohue Inventory Species List
library(readxl)

HAVO_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/NPSpecies_FullListWithDetails_HAVO_20230126115210.xlsx")
# Pohue species list from Kahuku Village EIS (Botanical Resource Assessment - Maya LeGrande)
Pohue_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/Pohue_Species_List.xlsx")

names(HAVO_veg)
names(Pohue_veg)

# Make edits to species names if needed

Pohue_veg2 <- Pohue_veg


# **********************************************************************
#Issue with weird white space (after converting pdf to excel table)
look <- Pohue_veg2$`Scientific Name`[Pohue_veg2$Family == "Euphorbiaceae"]
look2 <- Pohue_veg2$`Scientific Name`[Pohue_veg2$Family == "Asteraceae"]
look
look2
# below lines should replace a space " " with no space ""
# This works for second set but not for first
gsub(" ", "", look)
gsub(" ", "", look2)

gsub("\s", " ", look)
gsub("\s", "", look2)

gsub("\\W", " ", "test var. test") # this removes periods so not good solution
# Convert utf8 characters to integers to see that the space should be "32"
# Instead it shows up as "160"
utf8ToInt(" ")
look2[1]
utf8ToInt(look2[1])

look[1]
utf8ToInt(look[1])

intToUtf8(160)

# Use \\W to grab all weird spaces
#gsub("\\W", "", look) # \\W does not work because it also includes periods
gsub(intToUtf8(160), "", look)
# *******************************************



Pohue_veg2 <- Pohue_veg2 %>%
  mutate(`Scientific Name`= gsub(intToUtf8(160), " ", `Scientific Name`))

Pohue_veg2$`Scientific Name` <- str_replace_all(Pohue_veg2$`Scientific Name`, "subsp.", "ssp.")
Pohue_veg2$`Scientific Name` <- str_trim(Pohue_veg2$`Scientific Name`)

# HAVO_veg2 <- HAVO_veg
# Rename some species in HAVO NPSpecies
#HAVO_veg2$`Scientific Name`[HAVO_veg2$`Scientific Name` == ""] <- ""

# Rename some species in Pohue Kahuku Village Report (to match NPSpecies - even though NPSpecies might be out of date)
Pohue_veg2$`Scientific Name`[Pohue_veg2$`Scientific Name` == "Andropogon virginicus var. virginicus"] <- "Andropogon virginicus"
Pohue_veg2$`Scientific Name`[Pohue_veg2$`Scientific Name` == "Asclepias physocarpus"] <- "Asclepias physocarpa"
Pohue_veg2$`Scientific Name`[Pohue_veg2$`Scientific Name` == "Emilia sonchifolia var. sonchifolia"] <- "Emilia sonchifolia"

HAVO_bind2 <- HAVO_veg %>%
  mutate(Units = "HAVO") %>%
  select(Family, Scientific_Name = "Scientific Name", Nativeness,
         Common_Name = "Common Names", Units, Occurrence, Abundance)

Pohue_bind2 <- Pohue_veg2 %>%
  mutate(Units = "Pohue") %>%
  mutate(Occurrence = "") %>%
  mutate(Abundance = "") %>%
  select(Family, Scientific_Name = "Scientific Name", Nativeness = "Nativity",
         Common_Name = "Common name", Units)

HAVO_Pohue2 <- bind_rows(HAVO_bind2, Pohue_bind2) %>%
  dplyr::mutate(Occurrence = replace_na(Occurrence,"")) %>%
  dplyr::mutate(Abundance = replace_na(Abundance,"")) %>%
  dplyr::group_by(Scientific_Name) %>%
  dplyr::mutate(Units = paste(Units, collapse = ", ")) %>%
  #Occurrence = paste(Occurrence, collapse = ", "),
  #Abundance = paste(Abundance, collapse = ", ")) %>%
  tidyr::separate(Common_Name, c("Common_Name", NA), sep = ",", remove = FALSE) %>%
  dplyr::distinct(Scientific_Name, .keep_all = TRUE) %>%
  dplyr::arrange(dplyr::desc(Units), Family, Scientific_Name)

readr::write_excel_csv(HAVO_Pohue2, "C:/Users/JJGross/Downloads/NPSpecies_and_Kahuku_village_20230126.csv")

# KAHO Early Detection Field Maps Species List

KAHO_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/NPSpecies_FullListWithDetails_KAHO_20221118154653.xlsx")
PUHE_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/NPSpecies_FullListWithDetails_PUHE_20221118154822.xlsx")
PUHO_veg <- readxl::read_xlsx("C:/Users/JJGross/Downloads/NPSpecies_FullListWithDetails_PUHO_20221118154945.xlsx")

KONA_veg <- bind_rows(KAHO_veg, PUHE_veg, PUHO_veg)

names(KONA_veg)

KONA_veg2 <- KONA_veg %>%
  dplyr::select(Unit = "Park Code", Family, Scientific_Name = "Scientific Name", Common_Names = "Common Names", Occurrence, Nativeness, Abundance) %>%
  dplyr::group_by(Scientific_Name) %>%
  dplyr::mutate(Units = paste(Unit, collapse = ", "),
                Occurrence = paste(Occurrence, collapse = ", "),
                Abundance = paste(Abundance, collapse = ", ")) %>%
  tidyr::separate(Common_Names, c("Common_Name", NA), sep = ",", remove = FALSE) %>%
  dplyr::select(Family, Scientific_Name, Nativeness, Common_Name, Units, Occurrence, Abundance) %>%
  dplyr::distinct(Scientific_Name, .keep_all = TRUE) %>%
  dplyr::arrange(Family, Scientific_Name)

unique(KONA_veg$Abundance)

# use write_excel_csv to preserve hawaiian diacriticals
readr::write_excel_csv(KONA_veg2, "C:/Users/JJGross/Downloads/NPSpecies_Kona_Parks_20221118.csv")
