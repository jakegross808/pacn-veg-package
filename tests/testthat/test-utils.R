context("Reading from database and csv")

# Load data from db, skip this test if no db connection
tryCatch(db <- LoadPACNVeg(ftpc_params = "pacnveg",
                           eips_paths = paste0("../../scratchpad/", c("2021_established_invasives_1_20210129.mdb",
                                                                "2021_established_invasives_2_20210129.mdb",
                                                                "established_invasives_BE_master_20210818.mdb")),
                           cache = FALSE),
         error = function(e) {
           if (grepl(".*Could not open a connection to SQL Server.*", e$message)) {
             skip("No database connection")
           } else {stop(e)}
         })

# Write temporary csv files
dir <- tempdir()
if (dir.exists(dir)) {
  unlink(dir, recursive = TRUE)
}
WritePACNVeg(dir, create.folders = TRUE, overwrite = TRUE)

# Load data from csv
csv <- LoadPACNVeg(data_path = dir, data_source = "file", cache = FALSE)

data.names <- names(GetColSpec())

for (d.name in data.names) {
  test_that(paste0(d.name, ".csv matches data read from database"), {
    db <- db[[d.name]]
    csv <- csv[[d.name]]
    expect_dataframe_equal(csv, db)
  })
}

# Remove temporary csv's
unlink(dir, recursive = TRUE)

test_that("Events_extra_QAQC column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Sampling_Frame', 'Plot_Number', 'Entered_Date', 'Updated_Date', 'Verified', 'Verified_By', 'Verified_Date', 'Certified', 'Certified_By', 'Certified_Date', 'Completion_Time', 'Event_Notes', 'Plot_Notes', 'QA_notes')
  actual <- names(FilterPACNVeg("Events_extra_QAQC"))
  expect_equal(actual, expected)
})

test_that("Events_extra_xy column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Sampling_Frame', 'Plot_Number', 'Azimuth_Plot', 'Start_Lat', 'Start_Long', 'Center_Lat', 'Center_Long', 'End_Lat', 'End_Long', 'GCS', 'GCS_Datum', 'Lat_Dir', 'Long_Dir')
  actual <- names(FilterPACNVeg("Events_extra_xy"))
  expect_equal(actual, expected)
})

test_that("Events_extra_other column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Sampling_Frame', 'Zone', 'Management_Unit', 'Plot_Number', 'Max_Veg_Ht', 'Site_Name', 'Images')
  actual <- names(FilterPACNVeg("Events_extra_other"))
  expect_equal(actual, expected)
})

test_that("Species_extra column names are correct", {
  expected <- c('Species_ID', 'Scientific_Name', 'Code', 'Taxonomic_Order', 'Taxonomic_Family', 'Genus', 'Species', 'Subdivision', 'Authority', 'Synonym', 'Authority_Source', 'Citation', 'Common_Name', 'Life_Cycle', 'Complete', 'Update_Date', 'Update_By', 'Update_Comments', 'Park', 'Life_Form', 'Nativity', 'Park_Common_Name', 'Distribution', 'Conservation_Status')
  actual <- names(FilterPACNVeg("Species_extra"))
  expect_equal(actual, expected)
})

test_that("LgTrees column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Community', 'Sampling_Frame', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Quad', 'Status', 'Height', 'Height_Dead', 'Boles', 'DBH', 'DBH_Other', 'Vigor', 'Fruit_Flower', 'Rooting', 'Foliar', 'Caudex_Length', 'Shrublike_Growth', 'Resprouts', 'Measurement_Type', 'DBH_Bole', 'Status_Bole', 'Scientific_Name', 'Code', 'Life_Form', 'Nativity')
  actual <- names(FilterPACNVeg("LgTrees"))
  expect_equal(actual, expected)
})

test_that("Canopy column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Community', 'Sampling_Frame', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Quad', 'Status', 'Top', 'Base', 'Base_Ht', 'Distance', 'Height', 'Method', 'DBH', 'Scientific_Name', 'Code', 'Life_Form', 'Nativity', 'Comments')
  actual <- names(FilterPACNVeg("Canopy"))
  expect_equal(actual, expected)
})

test_that("Presence column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Community', 'Sampling_Frame', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Fruit_Flower', 'Dead', 'Outside_Plot', 'cf', 'Scientific_Name', 'Code', 'Life_Form', 'Nativity', 'Comments')
  actual <- names(FilterPACNVeg("Presence"))
  expect_equal(actual, expected)
})

test_that("SmWoody column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Community', 'Sampling_Frame', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Transect', 'DBH', 'Status', 'Foliar', 'Rooting', 'Count', 'Scientific_Name', 'Code', 'Life_Form', 'Nativity', 'Comments')
  actual <- names(FilterPACNVeg("SmWoody"))
  expect_equal(actual, expected)
})

test_that("Understory column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Community', 'Sampling_Frame', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Point', 'Substrate', 'Dead', 'Stratum', 'Scientific_Name', 'Code', 'Life_Form', 'Nativity')
  actual <- names(FilterPACNVeg("Understory"))
  expect_equal(actual, expected)
})

test_that("Debris column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Community', 'Sampling_Frame', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Transect', 'Length', 'Debris_Type', 'Diameter', 'Decay_Class', 'Comments')
  actual <- names(FilterPACNVeg("Debris"))
  expect_equal(actual, expected)
})

test_that("Events_extra_QAQC_EIPS column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Sampling_Frame', 'Transect_Type', 'Transect_Number', 'Entered_Date', 'Updated_Date', 'Verified', 'Verified_By', 'Verified_Date', 'Certified', 'Certified_By', 'Certified_Date', 'Transect_Notes', 'Event_Notes')
  actual <- names(FilterPACNVeg("Events_extra_QAQC_EIPS"))
  expect_equal(actual, expected)
})

test_that("Events_extra_xy_EIPS column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Sampling_Frame', 'Transect_Number', 'Azimuth_Transect', 'Lat', 'Long', 'GCS', 'Lat_Dir', 'Long_Dir')
  actual <- names(FilterPACNVeg("Events_extra_xy_EIPS"))
  expect_equal(actual, expected)
})

test_that("Events_extra_other_EIPS column names are correct", {
  expected <- c('Start_Date', 'Unit_Code', 'Sampling_Frame', 'Zone', 'Management_Unit', 'Transect_Number', 'Site_Name')
  actual <- names(FilterPACNVeg("Events_extra_other_EIPS"))
  expect_equal(actual, expected)
})

test_that("Species_extra_EIPS column names are correct", {
  expected <- c('Species_ID', 'Scientific_Name', 'Code', 'Taxonomic_Order', 'Taxonomic_Family', 'Genus', 'Species', 'Subdivision', 'Authority', 'Synonym', 'Authority_Source', 'Citation', 'Common_Name', 'Life_Cycle', 'Complete', 'Update_Date', 'Update_By', 'Update_Comments', 'Park', 'Life_Form', 'Nativity', 'Park_Common_Name', 'Distribution', 'Conservation_Status')
  actual <- names(FilterPACNVeg("Species_extra_EIPS"))
  expect_equal(actual, expected)
})

test_that("EIPS_data column names are correct", {
  expected <- c('Unit_Code', 'Community', 'Sampling_Frame', 'Start_Date', 'Transect_Number', 'Transect_Type', 'Species_ID', 'Cover_Class', 'Dead', 'Code', 'Scientific_Name', 'Life_Form', 'Nativity')
  actual <- names(FilterPACNVeg("EIPS_data"))
  expect_equal(actual, expected)
})
