# Downloading from the FTP

bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip")

slsTables <- bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
                          tables = c("Catch", "FishCodes", "Lengths", "Meter Corrections",
                                     "SLS Stations", "Tow Info", "Water Info"))

schema <- bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
                       tables = c("MSysRelationships"))

# Manipulate the data to be similar to the EDI data
getEDI("https://portal.edirepository.org/nis/mapbrowse?packageid=edi.534.7")
shell.exec(getEDI("https://portal.edirepository.org/nis/mapbrowse?packageid=edi.534.7", "SLSIntegrateEDI")[[1]])

library(dplyr)
library(wql)
library(tidyr)

# Lines 45-124, reading in the data ---------------------------------------
SLSTables <- list()

SLSTables$Catch <- slsTables$Catch %>%
  transmute(Date = as.Date(Date, format = "%Y-%m-%d"),
            Station = as.character(Station),
            Tow = as.integer(Tow),
            FishCode = as.integer(FishCode),
            Catch = as.integer(Catch),
            CatchID = as.integer(CatchID))

SLSTables$Lengths <- slsTables$Lengths %>%
  transmute(Date = as.Date(Date, format = "%Y-%m-%d"),
            Station = as.character(Station),
            Tow = as.integer(Tow),
            FishCode = as.integer(FishCode),
            Length = as.integer(Length),
            EntryOrder = as.integer(entryorder))

SLSTables$`Meter Corrections` <- slsTables$`Meter Corrections` %>%
  transmute(StudyYear = as.double(StudyYear),
            MeterSerial = as.integer(MeterSerial),
            CalibrationDate = as.Date(CalibrationDate, format = "%Y-%m-%d"),
            kFactor = as.double(kfactor),
            Notes = as.character(Notes))

SLSTables$`Tow Info` <- slsTables$`Tow Info` %>%
  transmute(Date = as.Date(Date, format = "%Y-%m-%d"),
            Station = as.character(Station),
            Tow = as.integer(Tow),
            Time = as.character(Time),
            Tide = as.character(Tide),
            BottomDepth = as.integer(BottomDepth),
            CableOut = as.integer(CableOut),
            Duration = as.double(Duration),
            NetMeterSerial = as.integer(NetMeterSerial),
            NetMeterStart = as.integer(NetMeterStart),
            NetMeterEnd = as.integer(NetMeterEnd),
            NetMeterCheck = as.integer(NetMeterCheck),
            Comments = as.character(Comments))

SLSTables$`Water Info` <- slsTables$`Water Info` %>%
  transmute(Survey = as.integer(Survey),
            Date = as.Date(Date, format = "%Y-%m-%d"),
            Station = as.character(Station),
            Temp = as.double(TopTemp),
            TopEC = as.integer(TopEC),
            BottomEC = as.integer(BottomEC),
            Secchi = as.integer(Secchi),
            NTU = as.double(NTU),
            FNU = as.double(FNU),
            Comments = as.character(Comments))

SLSTables$`SLS Stations` <- slsTables$`SLS Stations` %>%
  transmute(Station = as.character(Station),
            Description = as.character(Location),
            LatD = as.character(LatD),
            LatM = as.character(LatM),
            LatS = as.character(LatS),
            LonD = as.character(LonD),
            LonM = as.character(LonM),
            LonS = as.character(LonS))

# Lines 133-200, manipulating the data ------------------------------------

waterInfo <- SLSTables$`Water Info` %>%
  dplyr::mutate(
    # Converting secchi from cm to m
    Secchi = Secchi/100,
    # Converting EC to salinity
    # Per SOP normalized at temp = 25C; units are in millisiemens
    Sal_surf = wql::ec2pss(TopEC/1000, t = 25),
    Sal_bot = wql::ec2pss(BottomEC/1000, t = 25),
    # This is to take care of how the floats are read between Access and readr methods...
    Temp_surf = round(Temp, 2)) %>%
  dplyr::rename(Notes_env = Comments)

towInfo <- SLSTables$`Tow Info` %>%
  # manipulation of time should not be required if pulling from the RDS file,
  # but won't change so leaving to cover when you are using the csv files
  dplyr::mutate(Time = strptime(Time, format = "%Y-%m-%d %H:%M:%S",
                                tz = "America/Los_Angeles"),
                StudyYear = as.numeric(format(Date, format = "%Y"))) %>%
  dplyr::rename(Notes_tow = Comments) %>%
  # 1 parsing error because time was not recorded for that row
  # Now, joining to the Meter Corrections table to calculate tow volume later
  # This is based on the "WEB_Raw_Catch_Volum_Info" query in the SLS Access query database
  # First need to create a study year column; from the query above, the "WEB_VolumeInfo" query calculates year from the
  # towInfo table.
  dplyr::left_join(SLSTables$`Meter Corrections` %>%
                     # There are duplicated values here in this table; will simply distinct() them
                     # Confirmed via email with Adam, ES of Native Fish unit as of 10-27-2021
                     dplyr::distinct(),
                   by = c("StudyYear", "NetMeterSerial" = "MeterSerial")) %>%
  # Moving on to various required calculations
  dplyr::mutate(Datetime = as.POSIXct(paste(Date, format(Time, "%H:%M:%S")),
                                      format = "%Y-%m-%d %H:%M:%S",
                                      tz = "America/Los_Angeles"),
                # Now to turn tide into factors as outlined in the metadata file
                Tide = dplyr::case_when(Tide == 1 ~ "High Slack",
                                        Tide == 2 ~ "Ebb",
                                        Tide == 3 ~ "Low Slack",
                                        Tide == 4 ~ "Flood",
                                        TRUE ~ NA_character_),
                # Converting bottom depth to meters
                Depth = BottomDepth * 0.3048,
                # Convert cable out from feet to meters
                CableOut = CableOut * 0.3048,
                # Calculating tow volume for the fish net, 0.37 as the area of the mout of the net (per Access + SLS SOP document)
                Tow_volume = NetMeterCheck * kFactor * 0.37)

stationLookup <- SLSTables$`SLS Stations` %>%
  # Removing 2 rows of trailing white space
  # dplyr::mutate(across(c(Lat, Long), trimws)) %>%
  # tidyr::separate(Lat, into = c("LatD", "LatM", "LatS"), sep = " ") %>%
  # tidyr::separate(Long, into = c("LonD", "LonM", "LonS"), sep = " ") %>%
  dplyr::mutate(across(c(LatD, LatM, LatS, LonD, LonM, LonS), ~as.numeric(.x)),
                Latitude = (LatD + LatM/60 + LatS/3600),
                Longitude = -(LonD + LonM/60 + LonS/3600))

catch <- SLSTables$Catch

lengths <- SLSTables$Lengths %>%
  # Calculating total number of fish measured (across all lengths) and # of fish measured
  # per Date, Station, Tow, and FishCode
  # This is to calculate plus counts later in dfFin
  dplyr::group_by(Date, Station, Tow, FishCode, Length)%>%
  dplyr::summarise(LengthFrequency = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(Date, Station, Tow, FishCode) %>%
  dplyr::mutate(TotalLengthMeasured = sum(LengthFrequency)) %>%
  dplyr::ungroup()

tablesToJoin <- list(waterInfo, towInfo, stationLookup, catch, lengths) %>%
  setNames(c("Water Info", "Tow Info", "SLS Stations", "Catch", "Lengths"))

joinedData <- schemaJoin(schema[[1]], tablesToJoin) %>%
  dplyr::left_join(read.csv("https://github.com/Tham-E/LTMRdata/raw/master/data-raw/Species%20Codes.csv") %>%
                     dplyr::select(TMM_Code,
                                   Taxa) %>%
                     dplyr::filter(!is.na(TMM_Code)),
                   by = c("FishCode" = "TMM_Code")) %>%
  dplyr::left_join(tablesToJoin$`SLS Stations`,
                   by = "Station") %>%
  # Merging the two comment columns together; they both have data in them
  dplyr::mutate(Notes_tow = paste(Notes_tow, Notes_env, sep = "; ")) %>%
  dplyr::arrange(Date, Datetime, Survey, Station, Tow, Taxa, Length) %>%
  dplyr::mutate(Source = "SLS",
                SampleID = paste(Source, Date, Station, Tow), # Creating SampleID index
                Count = dplyr::if_else(is.na(Length),
                                       as.numeric(Catch),
                                       (LengthFrequency/TotalLengthMeasured) * Catch),
                # Creating Length_NA_flag to parallel the other survey datasets in LTMR
                Length_NA_flag = dplyr::if_else(is.na(Count), "No fish caught", NA_character_),
                # Creating Method column; Adam described this as an "Olbique tow", significantly diff from WMT
                Method = "Oblique tow",
                Station = as.character(Station)) %>%
  rename(Cable_length = CableOut, Tow_duration = Duration, Notes_flowmeter = Notes)

# Reading in the SLS edi data ---------------------------------------------

waterTowJoin <- waterInfo %>%
  dplyr::full_join(towInfo,
                   c("Date", "Station"))

waterTowCatchJoin <- waterTowJoin %>%
  dplyr::full_join(catch,
                   by = c("Date", "Station", "Tow"))

waterTowCatchLengthJoin <- waterTowCatchJoin %>%
  dplyr::full_join(lengths,
                   by = c("Date", "Station", "Tow", "FishCode"))

finJoin <- waterTowCatchLengthJoin %>%
  dplyr::left_join(read.csv("https://github.com/Tham-E/LTMRdata/raw/master/data-raw/Species%20Codes.csv") %>%
                     dplyr::select(TMM_Code,
                                   Taxa) %>%
                     dplyr::filter(!is.na(TMM_Code)),
                   by = c("FishCode" = "TMM_Code")) %>%
  dplyr::left_join(stationLookup,
                   by = "Station")

SLS <- finJoin %>%
  # Merging the two comment columns together; they both have data in them
  dplyr::mutate(Notes_tow = paste(Notes_tow, Notes_env, sep = "; ")) %>%
  dplyr::arrange(Date, Datetime, Survey, Station, Tow, Taxa, Length) %>%
  dplyr::mutate(Source = "SLS",
                SampleID = paste(Source, Date, Station, Tow), # Creating SampleID index
                Count = dplyr::if_else(is.na(Length),
                                       as.numeric(Catch),
                                       (LengthFrequency/TotalLengthMeasured) * Catch),
                # Creating Length_NA_flag to parallel the other survey datasets in LTMR
                Length_NA_flag = dplyr::if_else(is.na(Count), "No fish caught", NA_character_),
                # Creating Method column; Adam described this as an "Olbique tow", significantly diff from WMT
                Method = "Oblique tow",
                Station = as.character(Station)) %>%
  dplyr::rename(Cable_length = CableOut, Tow_duration = Duration, Notes_flowmeter = Notes) %>%
  data.frame()

# Are the equal? ----------------------------------------------------------

all.equal(SLS %>% select(names(joinedData)), joinedData)

joinedDataFulljoin <- schemaJoin(schema[[1]] %>%
                                   mutate(joinType = "full_join"), tablesToJoin) %>%
  dplyr::left_join(read.csv("https://github.com/Tham-E/LTMRdata/raw/master/data-raw/Species%20Codes.csv") %>%
                     dplyr::select(TMM_Code,
                                   Taxa) %>%
                     dplyr::filter(!is.na(TMM_Code)),
                   by = c("FishCode" = "TMM_Code")) %>%
  dplyr::left_join(tablesToJoin$`SLS Stations`,
                   by = "Station") %>%
  # Merging the two comment columns together; they both have data in them
  dplyr::mutate(Notes_tow = paste(Notes_tow, Notes_env, sep = "; ")) %>%
  dplyr::arrange(Date, Datetime, Survey, Station, Tow, Taxa, Length) %>%
  dplyr::mutate(Source = "SLS",
                SampleID = paste(Source, Date, Station, Tow), # Creating SampleID index
                Count = dplyr::if_else(is.na(Length),
                                       as.numeric(Catch),
                                       (LengthFrequency/TotalLengthMeasured) * Catch),
                # Creating Length_NA_flag to parallel the other survey datasets in LTMR
                Length_NA_flag = dplyr::if_else(is.na(Count), "No fish caught", NA_character_),
                # Creating Method column; Adam described this as an "Olbique tow", significantly diff from WMT
                Method = "Oblique tow",
                Station = as.character(Station)) %>%
  rename(Cable_length = CableOut, Tow_duration = Duration, Notes_flowmeter = Notes)

all.equal(SLS %>% select(names(joinedData)), joinedDataFulljoin)

# Does plotGPS work here? -------------------------------------------------

plottingTables <- bridgeAccess("C:\\Users\\TXNguyen\\Downloads\\SLS_Query.accdb",
                               c("WaterInfo", "Station_Lookup"))
library(lubridate)
library(dplyr)
library(tidyr)
gpsDF <- plottingTables$WaterInfo %>%
  mutate(
    # Converting Lat/Long to numeric
    across(c(StartLatDeg, StartLatMin, StartLatSec,
             StartLongDeg, StartLongMin, StartLongSec,
             EndLatDeg, EndLatMin, EndLatSec,
             EndLongDeg, EndLongMin, EndLongSec),
           ~as.numeric(.x)),
    # Now finally converting to lat/long in
    StartLat = StartLatDeg + StartLatMin/60 + StartLatSec/3600,
    StartLong = -(StartLongDeg + StartLongMin/60 + StartLongSec/3600),
    EndLat = EndLatDeg + EndLatMin/60 + EndLatSec/3600,
    EndLong = -(EndLongDeg + EndLongMin/60 + EndLongSec/3600),
    # Creating season year to help with the plotting function below
    SeasonYear = year(Date) + (month(Date) > 11)) %>%
  rename(Start_lat = StartLat, End_lat = EndLat, Start_lon = StartLong , End_lon = EndLong) %>%
  pivot_longer(cols = c(Start_lat, End_lat, Start_lon, End_lon),
               names_to = c("legend", ".value"), names_sep="_") %>%
  bind_rows(plottingTables$Station_Lookup %>%
              mutate(# Current structure of the lat/long is each component separated by spaces: pull that out
                LatD = sapply(strsplit(.$Lat, "\\s"), "[", 1),
                LatM = sapply(strsplit(.$Lat, "\\s"), "[", 2),
                LatS = sapply(strsplit(.$Lat, "\\s"), "[", 3),
                LonD = sapply(strsplit(.$Long, "\\s"), "[", 1),
                LonM = sapply(strsplit(.$Long, "\\s"), "[", 2),
                LonS = sapply(strsplit(.$Long, "\\s"), "[", 3),
                across(c(LatD, LatM, LatS, LonD, LonM, LonS), as.numeric)) %>%
              transmute(Station,
                        lat = LatD + LatM/60 + LatS/3600,
                        lon = -(LonD + LonM/60 + LonS/3600),
                        legend = "Theoretical"))

gpsDF %>%
  transmute(date = SeasonYear, Station,
            legend = factor(legend, levels = c("Start", "End", "Theoretical")),
            layer = ifelse(is.na(Survey), "Theoretical", Survey),
            lat, lon) %>%
  filter(date == 2023 | legend == "Theoretical") %>%
  plotGPS(layerName = "Survey", dateName = "Year",
          provider = leaflet::providers$CartoDB.VoyagerNoLabels)

gpsDF %>%
  transmute(date = SeasonYear, Station,
            legend = factor(legend, levels = c("Start", "End", "Theoretical")),
            layer = ifelse(is.na(Survey), "Theoretical", Survey),
            lat, lon) %>%
  filter(date == 2023 | legend == "Theoretical") %>%
  gpsOutlier() %>%
  plotGPS(layerName = "Survey", dateName = "Year")

# Finding the nearest cdec station
closestCDEC <- gpsDF %>%
  filter(year(Date) == 2023, Station %in% c(306, 609), Survey == 1,
         legend == "Start") %>%
  calcNearestCDEC()

gpsDF %>%
  filter(year(Date) == 2023, Station %in% c(306, 609), Survey == 1,
         legend == "Start") %>%
  mutate(time = paste(Date, "10:00:00")) %>%
  popCDEC(cdecClosest = closestCDEC)


gpsDF %>%
  filter(year(Date) == 2023, Station %in% c(306, 609), Survey == 1,
         legend == "Start") %>%
  mutate(time = paste(Date, "10:00:00"),
         temp = TopTemp) %>%
  popCDEC(variable = "temp")

gpsDF %>%
  filter(year(Date) == 2023, Station %in% c(306, 609), Survey == 1,
         legend == "Start") %>%
  mutate(time = paste(Date, "10:00:00"),
         turbidity = FNU) %>%
  popCDEC(variable = "turbidity")

gpsDF %>%
  filter(year(Date) == 2023, Station %in% c(306, 609), Survey == 1,
         legend == "Start") %>%
  mutate(time = paste(Date, "10:00:00"),
         ec = TopEC) %>%
  popCDEC(variable = "ec")
