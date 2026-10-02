# 1. Read the CSVs with explicit UTF-8 so the Hawaiian diacriticals load correctly
#    (readr::read_csv defaults to UTF-8; this is the key to the non-ASCII fix)
events_extra_xy_mgmt   <- readr::read_csv("data-raw/Events_extra_xy_mgmt.csv",
                                          locale = readr::locale(encoding = "UTF-8"))
PACN_veg_names         <- readr::read_csv("data-raw/PACN_veg_names_excel_csv.csv",
                                          locale = readr::locale(encoding = "UTF-8"))

# 2. Save them as package datasets (writes data/*.rda)
usethis::use_data(events_extra_xy_mgmt, PACN_veg_names, overwrite = TRUE)
