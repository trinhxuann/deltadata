# An internal script to pull the metadata for all cdec stations in the system.
# https://cdec.water.ca.gov/dynamicapp/staMeta?station_id

library(rvest)
session <- session("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id")

gageNames <- session %>%
  html_elements("a") %>%
  html_attr("href") %>%
  {.[which(grepl("/dynamicapp/staMeta\\?station_id=", .))]} %>%
  {sub(".+id=(.+)", "\\1", .)}

cdecStations <- lapply(gageNames, pullCoordinates) %>%
  bind_rows() %>%
  mutate(across(c(latitude, longitude), ~as.numeric(.x))) %>%
  filter(!latitude %in% c(0.00000, 99.99900) | !longitude %in% c(0.00000, -999.9990))

cdecMetadata <- pullMetadataCDEC(cdecStations$station, list = F)

save(cdecStations, cdecMetadata, file = file.path("data", paste0("cdecStations_", format(Sys.time(), "%Y-%m-%d"), ".RData")))

