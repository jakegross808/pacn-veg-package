context("Reading from database and csv")

# Load data from db, skip this test if no db connection
tryCatch(db <- LoadPACNVeg("pacnveg"),
         error = function(e) {
           if (message(e) %like% "Could not open a connection to SQL Server") {
             skip("No database connection")
           } else {e}
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
    expect_dataframe_equal(db, csv)
  })
}

# Remove temporary csv's
unlink(dir, recursive = TRUE)