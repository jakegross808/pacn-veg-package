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
