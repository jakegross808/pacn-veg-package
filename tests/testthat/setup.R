# Load data from db, skip this test if no db connection
tryCatch(db <- LoadPACNVeg(ftpc_params = "pacnveg",
                           eips_paths = paste0("dbs/", c("2021_established_invasives_1_20210129.mdb",
                                                         "2021_established_invasives_2_20210129.mdb",
                                                         "established_invasives_BE_master_20210818.mdb")),
                           cache = FALSE
),
error = function(e) {
  if (grepl(".*Could not open a connection to SQL Server.*", e$message)) {
    skip("No database connection")
  } else {stop(e)}
})
