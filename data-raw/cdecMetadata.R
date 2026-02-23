# An internal script to pull the metadata for all cdec stations in the system.
# https://cdec.water.ca.gov/dynamicapp/staMeta?station_id

load("R/sysdata.rda")

library(rvest)
library(dplyr)
session <- session("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id")

gageNames <- session %>%
  html_elements("a") %>%
  html_attr("href") %>%
  {.[which(grepl("/dynamicapp/staMeta\\?station_id=", .))]} %>%
  {sub(".+id=(.+)", "\\1", .)}

# --- Get Station Metadata ---

fullMetadata <- lapply(
  gageNames,
  function(x) {
    match(x, gageNames)
    cat(x, match(x, gageNames), "of", length(gageNames), "\n")
    pullMetadataCDEC(x)
  }
)

# --- Parsing out location data ---

cdecStation <- lapply(
  fullMetadata,
  function(x) {

    df <- data.frame(
      station = x$location[(x$location$key == "Station ID"), "value"],
      latitude = x$location[(x$location$key == "Latitude"), "value"],
      longitude = x$location[(x$location$key == "Longitude"), "value"]
    )

    df$latitude <- as.numeric(gsub("[^\\d.-]", "", df$latitude, perl = TRUE))
    df$longitude <- as.numeric(gsub("[^\\d.-]", "", df$longitude, perl = TRUE))
    df
  }
) %>%
  bind_rows()

# --- Parsing out the metadata tables ---

cdecMetadata <- lapply(
  fullMetadata,
  function(x) {

    stationName <- filter(x$location, key == "Station ID") %>%
      pull(value)

    x$sensor %>%
      mutate(gage = stationName)
  }
) %>%
  bind_rows() %>%
  janitor::clean_names(case = "lower_camel")

# --- Can run this manually to update the metadata tables once in a while ---
# Be careful here. Must update ALL internal objects at once.
usethis::use_data(cdecStation, cdecMetadata, schema20mm, tables20mm, tableNames20mm,
                  overwrite = TRUE, compress = 'xz', internal = T)
