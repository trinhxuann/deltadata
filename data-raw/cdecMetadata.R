# An internal script to pull the metadata for all cdec stations in the system.
# https://cdec.water.ca.gov/dynamicapp/staMeta?station_id

library(rvest)
library(dplyr)
session <- session("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id")

gageNames <- session %>%
  html_elements("a") %>%
  html_attr("href") %>%
  {.[which(grepl("/dynamicapp/staMeta\\?station_id=", .))]} %>%
  {sub(".+id=(.+)", "\\1", .)}

# --- Get station coordinates ---
# Depending on the speed of the CDEC servers, you may time-out before finishing
# Splitting into small chunks
gageNamesChunks <- split(gageNames, ceiling(seq_along(gageNames) / (length(gageNames) / 3)))
cdecStations <- vector("list", 3)
for (i in seq_along(gageNamesChunks)) {
  cdecStations[[i]] <- lapply(gageNamesChunks[[i]], function(x) {
    match(x, gageNames)
    cat(x, match(x, gageNames), "of", length(gageNames), "\n")
    pullCoordinates(x)
  }) %>%
    bind_rows() %>%
    mutate(across(c(latitude, longitude), ~as.numeric(.x))) %>%
    filter(!latitude %in% c(0.00000, 99.99900) | !longitude %in% c(0.00000, -999.9990))
}
cdecStations <- bind_rows(cdecStations)

# --- Get station metadata ---
cdecMetadata <- lapply(cdecStations$station, function(s) {
  cat("Processing station:", s, "\n")  # Print the station being processed

  tryCatch(
    {
      pullCDEC(station = s, verbose = FALSE)
    },
    error = function(e) {
      cat("Error processing station", s, ":", e$message, "\n") # Print error message
      return(NA) # Or handle the error in a different way
    }
  )
})
numberErrors <- which(sapply(cdecMetadata, function(x) !is.data.frame))
if (length(numberErrors) = 0) {
  cdecMetadata <- bind_rows(cdecMetadata)
} else {
  cdecMetadata[numberErrors]
}

usethis::use_data(cdecStations, cdecMetadata, overwrite = TRUE, compress = 'xz', internal = T)
