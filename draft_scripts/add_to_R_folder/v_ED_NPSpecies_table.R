# ----Merge NPSpecies Lists ----
library(readxl)

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


