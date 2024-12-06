# This script runs all necessary data scripts to save to the internal datasets
# within the package

# --- CDEC metadata ---
source(file.path("data-raw", "cdecMetadata.R"))
# --- 20 mm relational tables ---
source(file.path("data-raw", "qaqcVignette20mm.R"))

usethis::use_data(cdecStations, cdecMetadata, schema, tables, tableNames,
                  overwrite = TRUE, compress = 'xz', internal = T)

