pacnveg_cache_path <- "C:/LocalDocuments/Databases_copied_local/EIPS"
eips_file_info <- list.files(pacnveg_cache_path, full.names = TRUE)
db <- LoadPACNVeg(ftpc_params = "FTPC",
                  eips_paths = eips_file_info,
                  cache = FALSE)
