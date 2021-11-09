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
  expected <- c('Unit_Code', 'Sampling_Frame', 'Start_Date', 'Year', 'Cycle', 'Plot_Number', 'Entered_Date', 'Updated_Date', 'Verified', 'Verified_By', 'Verified_Date', 'Certified', 'Certified_By', 'Certified_Date', 'Completion_Time', 'Event_Notes', 'Plot_Notes', 'QA_notes')
  actual <- names(FilterPACNVeg("Events_extra_QAQC"))
  expect_equal(actual, expected)
})

test_that("Events_extra_xy column names are correct", {
  expected <- c('Unit_Code', 'Sampling_Frame', 'Year', 'Cycle', 'Plot_Number', 'Azimuth_Plot', 'Start_Lat', 'Start_Long', 'Center_Lat', 'Center_Long', 'End_Lat', 'End_Long', 'GCS', 'GCS_Datum', 'Lat_Dir', 'Long_Dir', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("Events_extra_xy"))
  expect_equal(actual, expected)
})

test_that("Events_extra_other column names are correct", {
  expected <- c('Unit_Code', 'Sampling_Frame', 'Year', 'Cycle', 'Zone', 'Management_Unit', 'Plot_Number', 'Max_Veg_Ht', 'Site_Name', 'Images', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("Events_extra_other"))
  expect_equal(actual, expected)
})

test_that("Species_extra column names are correct", {
  expected <- c('Species_ID', 'Scientific_Name', 'Code', 'Taxonomic_Order', 'Taxonomic_Family', 'Genus', 'Species', 'Subdivision', 'Authority', 'Synonym', 'Authority_Source', 'Citation', 'Common_Name', 'Life_Cycle', 'Complete', 'Update_Date', 'Update_By', 'Update_Comments', 'Park', 'Life_Form', 'Nativity', 'Park_Common_Name', 'Distribution', 'Conservation_Status')
  actual <- names(FilterPACNVeg("Species_extra"))
  expect_equal(actual, expected)
})

test_that("LgTrees column names are correct", {
  expected <- c('Unit_Code', 'Community', 'Sampling_Frame', 'Year', 'Cycle', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Quad', 'Status', 'Height', 'Height_Dead', 'Boles', 'DBH', 'DBH_Other', 'Vigor', 'Fruit_Flower', 'Rooting', 'Foliar', 'Caudex_Length', 'Shrublike_Growth', 'Resprouts', 'Measurement_Type', 'DBH_Bole', 'Status_Bole', 'Scientific_Name', 'Code', 'Life_Form', 'Nativity', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("LgTrees"))
  expect_equal(actual, expected)
})

test_that("Canopy column names are correct", {
  expected <- c('Unit_Code', 'Community', 'Sampling_Frame', 'Year', 'Cycle', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Quad', 'Status', 'Top', 'Base', 'Base_Ht', 'Distance', 'Height', 'Method', 'DBH', 'Scientific_Name', 'Code', 'Life_Form', 'Nativity', 'Comments', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("Canopy"))
  expect_equal(actual, expected)
})

test_that("Presence column names are correct", {
  expected <- c('Unit_Code', 'Community', 'Sampling_Frame', 'Year', 'Cycle', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Fruit_Flower', 'Dead', 'Outside_Plot', 'cf', 'Scientific_Name', 'Code', 'Life_Form', 'Nativity', 'Comments', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("Presence"))
  expect_equal(actual, expected)
})

test_that("SmWoody column names are correct", {
  expected <- c('Unit_Code', 'Community', 'Sampling_Frame', 'Year', 'Cycle', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Transect', 'DBH', 'Status', 'Foliar', 'Rooting', 'Count', 'Scientific_Name', 'Code', 'Life_Form', 'Nativity', 'Comments', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("SmWoody"))
  expect_equal(actual, expected)
})

test_that("Understory column names are correct", {
  expected <- c('Unit_Code', 'Community', 'Sampling_Frame', 'Year', 'Cycle', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Point', 'Substrate', 'Dead', 'Stratum', 'Scientific_Name', 'Code', 'Life_Form', 'Nativity', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("Understory"))
  expect_equal(actual, expected)
})

test_that("Debris column names are correct", {
  expected <- c('Unit_Code', 'Community', 'Sampling_Frame', 'Year', 'Cycle', 'Plot_Type', 'Plot_Number', 'QA_Plot', 'Transect', 'Length', 'Debris_Type', 'Diameter', 'Decay_Class', 'Comments', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("Debris"))
  expect_equal(actual, expected)
})

test_that("Events_extra_QAQC_EIPS column names are correct", {
  expected <- c('Unit_Code', 'Sampling_Frame', 'Start_Date', 'Year', 'Cycle', 'Transect_Type', 'Transect_Number', 'Entered_Date', 'Updated_Date', 'Verified', 'Verified_By', 'Verified_Date', 'Certified', 'Certified_By', 'Certified_Date', 'Transect_Notes', 'Event_Notes')
  actual <- names(FilterPACNVeg("Events_extra_QAQC_EIPS"))
  expect_equal(actual, expected)
})

test_that("Events_extra_xy_EIPS column names are correct", {
  expected <- c('Unit_Code', 'Sampling_Frame', 'Year', 'Cycle', 'Transect_Number', 'Azimuth_Transect', 'Lat', 'Long', 'GCS', 'Lat_Dir', 'Long_Dir', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("Events_extra_xy_EIPS"))
  expect_equal(actual, expected)
})

test_that("Events_extra_other_EIPS column names are correct", {
  expected <- c('Unit_Code', 'Sampling_Frame', 'Year', 'Cycle', 'Zone', 'Management_Unit', 'Transect_Number', 'Site_Name', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("Events_extra_other_EIPS"))
  expect_equal(actual, expected)
})

test_that("Species_extra_EIPS column names are correct", {
  expected <- c('Species_ID', 'Scientific_Name', 'Code', 'Taxonomic_Order', 'Taxonomic_Family', 'Genus', 'Species', 'Subdivision', 'Authority', 'Synonym', 'Authority_Source', 'Citation', 'Common_Name', 'Life_Cycle', 'Complete', 'Update_Date', 'Update_By', 'Update_Comments', 'Park', 'Life_Form', 'Nativity', 'Park_Common_Name', 'Distribution', 'Conservation_Status')
  actual <- names(FilterPACNVeg("Species_extra_EIPS"))
  expect_equal(actual, expected)
})

test_that("EIPS_data column names are correct", {
  expected <- c('Unit_Code', 'Community', 'Sampling_Frame', 'Year', 'Cycle', 'Transect_Type', 'Transect_Number', 'Segment', 'Species_ID', 'Cover_Class', 'Dead', 'Code', 'Scientific_Name', 'Life_Form', 'Nativity', 'Certified', 'Verified')
  actual <- names(FilterPACNVeg("EIPS_data"))
  expect_equal(actual, expected)
})

test_that("FilterPACNVeg filters on park", {
  actual <- FilterPACNVeg(park = "AMME")
  actual <- sapply(actual, function(df) {
    if ("Unit_Code" %in% names(df) & nrow(df) > 0) {
      unique(df$Unit_Code)
    } else if ("Park" %in% names(df) & nrow(df) > 0) {
      unique(df$Park)
    } else {
      NA
    }
    })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, "AMME")
})

test_that("FilterPACNVeg filters on community", {
  actual <- FilterPACNVeg(community = "Coastal")
  actual <- sapply(actual, function(df) {
    if ("Community" %in% names(df) & nrow(df) > 0) {
      unique(df$Community)
    } else {
      NA
    }
  })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, "Coastal")
})

test_that("FilterPACNVeg filters on sampling frame", {
  actual <- FilterPACNVeg(sample_frame = "Kaloko-Honokohau")
  actual <- sapply(actual, function(df) {
    if ("Sampling_Frame" %in% names(df) & nrow(df) > 0) {
      unique(df$Sampling_Frame)
    } else {
      NA
    }
  })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, "Kaloko-Honokohau")
})

test_that("FilterPACNVeg filters on plot type", {
  actual <- FilterPACNVeg(plot_type = "Rotational")
  actual <- sapply(actual, function(df) {
    if ("Plot_Type" %in% names(df) & nrow(df) > 0) {
      unique(df$Plot_Type)
    } else {
      NA
    }
  })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, "Rotational")
})

test_that("FilterPACNVeg filters on QA plots", {
  actual <- FilterPACNVeg(is_qa_plot = TRUE)
  actual <- sapply(actual, function(df) {
    if ("QA_Plot" %in% names(df) & nrow(df) > 0) {
      unique(df$QA_Plot)
    } else {
      NA
    }
  })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, TRUE)
})

test_that("FilterPACNVeg filters on transect type", {
  actual <- FilterPACNVeg(transect_type = "Fixed")
  actual <- sapply(actual, function(df) {
    if ("Transect_Type" %in% names(df) & nrow(df) > 0) {
      unique(df$Transect_Type)
    } else {
      NA
    }
  })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, "Fixed")
})

test_that("FilterPACNVeg filters on species code", {
  actual <- FilterPACNVeg(species_code = "GERHOM")
  actual <- sapply(actual, function(df) {
    if ("Code" %in% names(df) & nrow(df) > 0) {
      unique(df$Code)
    } else {
      NA
    }
  })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, "GERHOM")
})

test_that("FilterPACNVeg filters on species code", {
  actual <- FilterPACNVeg(sci_name = "Passiflora edulis")
  actual <- sapply(actual, function(df) {
    if ("Scientific_Name" %in% names(df) & nrow(df) > 0) {
      unique(df$Scientific_Name)
    } else {
      NA
    }
  })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, "Passiflora edulis")
})

test_that("FilterPACNVeg filters on species code", {
  actual <- FilterPACNVeg(nativity = "Native")
  actual <- sapply(actual, function(df) {
    if ("Nativity" %in% names(df) & nrow(df) > 0) {
      unique(df$Nativity)
    } else {
      NA
    }
  })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, "Native")
})

test_that("FilterPACNVeg filters on certification status", {
  actual <- FilterPACNVeg(certified = TRUE)
  actual <- sapply(actual, function(df) {
    if ("Certified" %in% names(df) & nrow(df) > 0) {
      unique(df$Certified)
    } else {
      NA
    }
  })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, TRUE)
})

test_that("FilterPACNVeg filters on verification status", {
  actual <- FilterPACNVeg(verified = TRUE)
  actual <- sapply(actual, function(df) {
    if ("Verified" %in% names(df) & nrow(df) > 0) {
      unique(df$Verified)
    } else {
      NA
    }
  })
  actual <- unique(actual[!is.na(actual)])

  expect_equal(actual, TRUE)
})

test_that("RemoveSingleVisits removes data from plots with no revisits",{
  data <- FilterPACNVeg("Understory")
  result <- RemoveSingleVisits(data)
  visit_count <- result %>%
    dplyr::select(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Number) %>%
    unique() %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Plot_Number) %>%
    dplyr::summarise(plot_count = dplyr::n())

  expect_true(all(visit_count$plot_count > 1))
})
