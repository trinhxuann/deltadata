# This script runs all necessary data scripts to save to the internal datasets
# within the package

tryCatch({
  load("R/sysdata.rda")
}, error = function(e) {
  message("R/sysdata.rda not found. Start fresh.")
})

# --- CDEC metadata ---
source(file.path("data-raw", "cdecMetadata.R"))
# --- 20 mm relational tables ---
source(file.path("data-raw", "qaqcVignette20mm.R"))

usethis::use_data(cdecStations, cdecMetadata, schema20mm, tables20mm, tableNames20mm,
                  overwrite = TRUE, compress = 'xz', internal = T)

