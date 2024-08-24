# An internal script that supports various data tables to support the qaqcData()
# function:
# 1. crosswalk table of recommended IEP names tied to survey names
# 2. tow schedules of each survey
# 3. meter schedules of each survey

# crosswalk ---------------------------------------------------------------

crosswalk <- readxl::read_xlsx(file.path("data-raw", "vocab.xlsx"),
                               sheet = "Variables")

# Tow Schedule ------------------------------------------------------------
# A tow schedule dictates the amount of cable to let out based on the depth
# of the sampling location.
towSchedule <- list()

# 20 mm
# Informed by protocol SOP, internal document on the UDrive. Contact 20 mm lead
towSchedule$ttmm <- data.frame(
  duration = c(2.5, 5, 10)[1:7],
  maxDepth = c(10, 13, 17, 21, 26, 32, Inf),
  depth = cut(c(10, 13, 17, 21, 26, 32, Inf),
              breaks = c(0, 10, 13, 17, 21, 26, 32, Inf),
              right = T, include.lowest = T),
  cableLength = c(75, 100, 125, 150, 175, 200, 225)
)

# sls
# Informed by protocol SOP, internal document on the UDrive. Contact 20 mm lead

towSchedule$sls <- data.frame(
  duration = c(2.5, 5, 10)[1:8],
  maxDepth = c(5, 10, 15, 20, 25, 30, 35, Inf),
  depth = cut(c(5, 10, 15, 20, 25, 30, 35, Inf),
              breaks = c(0, 5, 10, 15, 20, 25, 30, 35, Inf),
              right = T, include.lowest = T),
  cableLength = c(45, 60, 75, 90, 105, 120, 135, 150)
)

# fmwt
# Informed by protocol SOP that can be found on the FTP website
# https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%20Protocol.pdf
towSchedule$fmwt <- data.frame(
  duration = NA,
  maxDepth = c(10, 15, 20, 25, 30, 35, Inf),
  depth = cut(c(10, 15, 20, 25, 30, 35, Inf),
              breaks = c(0, 10, 15, 20, 25, 30, 35, Inf),
              right = T, include.lowest = T),
  cableLength = c(0, 50, 100, 125, 150, 175, 200)
)

# Meter Schedule ----------------------------------------------------------
# A meter schedule is defined as a table that indicates the expected range of the
# flow meter for a tow duration for a gear type.
meterSchedule <- list()

# 20 mm
# Internal QAQC values informed by field experience
meterSchedule$ttmm <- data.frame(
  duration = c(5, 10),
  meterMin = c(5000, 10000),
  meterMax = c(15000, 30000),
  gear = c(2, 2)
)

# SLS
# Internal QAQC values informed by field experience
meterSchedule$sls <- data.frame(
  duration = c(2.5, 5., 10),
  meterMin = c(2500, 5000, 10000),
  meterMax = c(12000, 15000, 30000),
  gear = NA
)

# Saving the list to be made available in the package ---------------------

usethis::use_data(crosswalk, overwrite = TRUE, compress = "xz")
usethis::use_data(towSchedule, overwrite = TRUE, compress = 'xz')
usethis::use_data(meterSchedule, overwrite = TRUE, compress = 'xz')
