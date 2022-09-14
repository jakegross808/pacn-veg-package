map <- MapPACNVeg()
map_eips <- MapPACNVeg(protocol = "EIPS")
map_ftpc <- MapPACNVeg(protocol = "FTPC")
map_havo <- MapPACNVeg(park = "HAVO")
map_crosstalk <- MapPACNVeg(crosstalk = TRUE)
map_crosstalk_hale <- MapPACNVeg(crosstalk = TRUE, park = "HALE")

locs <- PlotAndTransectLocations()
locs_havo <- PlotAndTransectLocations(park = "HAVO")
locs_crosstalk <- PlotAndTransectLocations(crosstalk = TRUE)

test_that("MapPACNVeg returns a Leaflet map", {
  expect_s3_class(map, "leaflet")
  expect_s3_class(map_eips, "leaflet")
  expect_s3_class(map_ftpc, "leaflet")
  expect_s3_class(map_havo, "leaflet")
  expect_s3_class(map_crosstalk, "leaflet")
  expect_s3_class(map_crosstalk_hale, "leaflet")
})

test_that("MapPACNVeg output hasn't changed", {
  expect_snapshot_output(map)
})

test_that("PlotAndTransectLocations returns the correct columns", {
  expected <- c("Protocol", "Unit_Code", "Sampling_Frame", "Sample_Unit", "Sample_Unit_Type", "Sample_Unit_Number", "Lat", "Long", "Cycle", "Year", "Tsect_Line_Cycle", "Tsect_Line_Year", "Transect_Line")

  expect_equal(names(locs), expected)
  expect_equal(names(locs_havo), expected)
})

test_that("PlotAndTransectLocations returns a tibble when crosstalk == FALSE", {
  expect_true(tibble::is_tibble(locs))
  expect_true(tibble::is_tibble(locs_havo))
})

test_that("PlotAndTransectLocations returns a SharedData object when crosstalk == TRUE", {
  expect_s3_class(locs_crosstalk, "SharedData")
})
