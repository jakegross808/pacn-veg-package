# Load data from db, skip this test if no db connection
tryCatch(db <- LoadPACNVeg(ftpc_params = "pacnveg",
                           eips_paths = here::here("tests", "testthat", "dbs", c("2021_established_invasives_20220902.mdb",
                                                                                 "2022_established_invasives_20220907.mdb",
                                                                                 "established_invasives_BE_master_20220503.mdb")),
                           cache = FALSE),
         error = function(e) {
           if (grepl(".*Could not open a connection to SQL Server.*", e$message)) {
             skip("No database connection")
           } else {stop(e)}
         })
