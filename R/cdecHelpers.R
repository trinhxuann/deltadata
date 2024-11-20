#' Pulling CDEC gage data
#'
#' @description
#' This function will first pull the metadata of a CDEC gage. This allows the
#' function to direct the user accordingly if various arguments are missing.
#' Once all arguments are provided, the query is created and data downloaded.
#'
#'
#' @param station A character vector of station names. Can be multiple stations.
#' @param sensor A singular sensor value of interest.
#' @param duration The duration data, can be `event`, `hourly`, or `daily` and
#' depends on data availability for the gage of interest.
#' @param dateStart Beginning date for the period of interest.
#' @param dateEnd Ending date for the period of interest. Will default to today
#' if left as `NULL`
#' @param temperatureUnits Either `C` or `F` to convert temperature. Only
#' applicable to temperature data.
#' @param coordinates A vector of length = 2 containing the lat and lon, in
#' that order. This argument can be used instead of `station`. See 'Details'
#' for additional comments.
#' @param verbose Logical. Should the function guide the user through the
#' argument selection if arguments are missing? This also prints the link that
#' the function will download from. Defaults to `TRUE`.
#'
#' @details
#' The `coordinates` argument can be used in place of the `station` argument.
#' The \code{\link{calcNearestCDEC}} function will be used to calculate the nearest
#' CDEC station to your point of interest and pull data from that gage. use
#' that function if you are specifically only interested in the metadata
#' of the nearest CDEC gage.
#'
#' @return A data frame of the requested data pull.
#' @export
#'
#' @importFrom rvest session html_elements html_text html_table
#'
#' @examples
#' \dontrun{
#' pullCDEC("MAL")
#' pullCDEC(coordinates = c(38.04281, -121.9201))
#' pullCDEC("MAL", 25, "hourly", "06/13/1986", "06/14/1986")
#' # If coordinates are used instead, must specify the argument names.
#' pullCDEC(coordinates = c(38.04281, -121.9201), sensor = 25,
#' duration = "hourly", dateStart = "06/13/1986", dateEnd = "06/14/1986")
#' }
pullCDEC <- function(station, sensor = NULL,
                     duration = c("event", "hourly", "daily"),
                     dateStart, dateEnd = NULL, temperatureUnits = c("C", "F"),
                     coordinates,
                     verbose = T) {

  if (!missing(coordinates) & !missing(station)) {
    warning("Both `station` and `coordinates` are provided. Ignoring `coordinates`.", call. = F)
  }

  if (!missing(coordinates) & missing(station)) {
    if (length(coordinates) != 2) stop("`coordinates` should be a vector of two numbers, lat and lon.", call. = F)
    cdecClosest <- calcNearestCDEC(data.frame(lat = coordinates[[1]], lon = coordinates[[2]]))
    station <- unique(cdecClosest[[1]][["cdecStation"]])
  }

  if (is.null(sensor)) {

    webpage <- rvest::session(paste0("http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", station))

    table <- rvest::read_html(webpage)
    table <- rvest::html_elements(table, "table")

    if (length(table) != 0) {

      session <- table[[which(grepl("Sensor Description", table))]]
      # session <- table %>%
      #   {.[[which(grepl("Sensor Description", .))]]}
    } else {
      stop("Check station name. No table was found.")
    }

    tableNames <- rvest::html_elements(session, "th")
    tableNames <- rvest::html_text(tableNames)
    tableNames <- gsub("\\s", "", tableNames)
    tableNames <- gsub("(^[A-Z])", "\\L\\1", tableNames, perl = T)

    availableData <- rvest::html_table(session)

    if (nrow(availableData) == 0) {

      availableData <- lapply(tableNames, function(x) {
        df <- data.frame(name = NA)
        names(df) <- x
        df
      })
      names(availableData) <- tableNames
      availableData <- do.call(cbind, availableData)

    } else {

      availableData <- setNames(availableData, tableNames)
      availableData <- availableData[order(availableData[["sensorNumber"]]), ]
      availableData$duration <- gsub("\\(|\\)", "", availableData[["duration"]])
      availableData$gage <- station

    }

    if (is.null(sensor)) {
      if (isTRUE(verbose)) {
        print(availableData, n = Inf, width = Inf)
        message("Please provide sensor # and duration")
      }
      return(invisible(availableData))
    }

    if (length(duration) > 1) {
      returnDF <- availableData[availableData[["sensorNumber"]] == sensor, ]

      if (isTRUE(verbose)) {
        print(returnDF, n = Inf, width = Inf)
        message("Select a duration.")
      }
      return(invisible(returnDF))
    }

    if (missing(dateStart)) {
      returnDF <- availableData[availableData[["sensorNumber"]] == sensor, ]

      if (isTRUE(verbose)) {
        print(returnDF, n = Inf, width = Inf)
        message("Select a start date")
      }
      return(invisible(returnDF))
    }
  }

  temperatureUnits <- match.arg(temperatureUnits)
  duration <- match.arg(duration)

  if (duration == "event") {
    duration <- "E"
  } else {
    if (duration == "hourly") {
      duration <- "H"
    } else {
      duration <- "D"
    }
  }

  parseDate <- function(date) {
    firstDigits <- sub("(\\d+).*", "\\1", date)

    if (nchar(firstDigits) < 3) {
      date <- tryCatch(as.Date(date, tryFormat = c("%m/%d/%Y", "%m-%d-%Y")),
                       error = function(cond) {
                         stop("Cannot parse date as provided. Supply the date as `YYYY-MM-DD` or `MM-DD-YYYY` format seperated by `-` or `/`.",
                              call. = F)
                       })
    } else {
      date <- tryCatch(as.Date(date, tryFormat = c("%Y/%m/%d", "%Y-%m-%d")),
                       error = function(cond) {
                         stop("Cannot parse date as provided. Supply the date as `YYYY-MM-DD` or `MM-DD-YYYY` format seperated by `-` or `/`.",
                              call. = F)
                       })
    }
    return(date)
  }

  dateStart <- parseDate(dateStart)

  if (is.null(dateEnd)) {
    dateEnd <- Sys.Date()
  } else {
    dateEnd <- parseDate(dateEnd)
  }

  urlLink <- paste0(
    "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=",
    paste(station, collapse = ","),
    "&SensorNums=", sensor,
    "&dur_code=", duration,
    "&Start=", dateStart,
    "&End=", dateEnd
  )

  if (isTRUE(verbose)) cat("Reading from:", urlLink, "\n")

  df <- read.csv(urlLink, check.names = F)

  names(df) <- gsub("((?<=[_\\s])+.)", "\\U\\1", tolower(names(df)), perl = T)
  names(df) <- gsub("_|\\s", "", names(df))

  df$value <- as.double(ifelse(df[["value"]] == "---", NA, df[["value"]]))
  df$dataFlag <- as.character(df[["dataFlag"]])
  df$dateTime <- as.POSIXct(df[["dateTime"]], format = "%Y%m%d %H%M", tz = "America/Los_Angeles")
  df$obsDate <- as.Date(df[["obsDate"]], format = "%Y%m%d %H%M")

  if (nrow(df) == 0) {
    warning("No data available for station ", station, " as specified.")
    return()
  }

  if (any(unique(df[["units"]]) %in% "DEG F") & temperatureUnits == "C") {
    df$value <- ifelse(df[["units"]] == "DEG F", (df[["value"]] - 32) * 5/9, df[["value"]])
    df$units <- ifelse(df[["units"]] == "DEG F", "DEG C", df[["units"]])
  }

  if (any(unique(df[["units"]]) %in% "DEG C") & temperatureUnits == "F") {
    df$value <- ifelse(df[["units"]] == "DEG C", (df[["value"]] * 9/5) + 32, df[["value"]])
    df$units <- ifelse(df[["units"]] == "DEG C", "DEG F", df[["units"]])
  }

  df
}

#' Pulling CDEC gage metadata
#'
#' @description
#' A function to pull the metadata table associated with a CDEC gage.
#'
#' @param station A character vector that supports one or more CDEC gage.
#' @param list Logical, should the output be provided as a list or data.frame?
#'
#' @return A list or data frame of the metadata associated with the requested
#' CDEC gage.
#' @export
#'
#' @examples
#' \dontrun{
#' pullMetadataCDEC("MAL")
#' }
pullMetadataCDEC <- function(station, list = T) {
  metadata <- mapply(pullCDEC, station = station, MoreArgs = list(verbose = F), SIMPLIFY = F)

  if (!isTRUE(list)) {
    return(do.call(rbind.data.frame, metadata))
  }
  metadata
}

#' Pull CDEC gage lat/lon
#'
#' @param gage Name of the gage of interest, as a character.
#'
#' @return A data frame containing the station name, lat, and lon.
#' @export
#'
#' @importFrom rvest session html_element html_text
#'
#' @examples
#' \dontrun{
#' pullCoordinates("MAL")
#' }
pullCoordinates <- function(gage) {

  dataString <- rvest::session(paste0("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", gage))
  dataString <- rvest::html_element(dataString, "table")
  dataString <- rvest::html_text(dataString)

  data.frame(station = regmatches(dataString, regexpr("(?<=Station ID)(.*)(?=Elevation)", dataString, perl = T)),
             latitude = regmatches(dataString, regexpr("(?<=Latitude)([\\d.-]+)", dataString, perl = T)),
             longitude = regmatches(dataString, regexpr("(?<=Longitude)([\\d.-]+)", dataString, perl = T)))
}

#' Find the closest CDEC gage
#'
#' @description
#' Identifies the nearest CDEC gage to a lat/lon of interest. This function
#' requires metadata of all CDEC stations of interest. By default, all CDEC
#' stations are used.
#'
#'
#' @param df A data frame that contains at least the lat/lon of station(s) of
#' interest, named as `lat` and `lon`.
#' @param cdecGPS A data frame containing the GPS coordinates of the CDEC gages
#' of interest, as `lat` and `lon`.
#' @param cdecMetadata A data frame containing the metadata table of the CDEC
#' gages of interest. This table must match the format provided by the DWR
#' website. It is recommended to use `pullMetadataCDEC()` to get this data.
#' @param variable The water quality variable of interest. Currently only
#' supports water temperature as `temp`, turbidity as `turbidity`, and
#' electro-conductivity as `ec`. This defaults to water temperature.
#' @param waterColumn Where in the water column should the variable of interest
#' be prioritized? Supports only `top` and `bottom`, defaulting to `top`. For
#' now, top data will be used in the calculation even if you ask for bottom
#' data.
#'
#' @return A data frame of the metadata of the closest CDEC station to your
#' point of interest that has data for the variable of interest.
#' @export
#'
#' @importFrom geosphere distm distVincentyEllipsoid
#'
#' @examples
#' \dontrun{
#' df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)
#'
#' calcNearestCDEC(df)
#' }
calcNearestCDEC <- function(df, cdecGPS = deltadata:::cdecStations,
                            cdecMetadata = deltadata:::cdecMetadata,
                            variable = c("temp", "turbidity", "ec"),
                            waterColumn = c("top", "bottom")) {

  names(df) <- tolower(names(df))

  if (all(!c("station", "lat", "lon") %in% names(df))) stop("Your `df` must contain at least `station`, `lat, `lon`.", call. = F)

  if (length(variable) > 1) variable <- match.arg(variable)
  if (length(waterColumn) > 1) waterColumn <- match.arg(waterColumn)

  if (variable == "ec" & variable != "elec.* conduct.* micro") {
    variableWanted <- "elec.* conduct.* micro"
  } else {
    if (variable == "temp" & variable != "(temp).*(water)") {
      variableWanted <- "(temp).*(water)"
    } else variableWanted <- variable
  }

  waterColumnWanted <- ifelse(waterColumn %in% "bottom", "(lower|bottom)", waterColumn)

  variableWanted <- ifelse(waterColumn == "bottom",
                           paste0(variableWanted, ".*", waterColumnWanted),
                           variableWanted)

  # Closest gages with the required data for variable of interest
  closestGages <- cdecMetadata[grepl(variableWanted, cdecMetadata[["sensorDescription"]], ignore.case = T), ]


  cdecGPSFiltered <- cdecGPS[cdecGPS[["station"]] %in% closestGages[["gage"]], ]

  lapply(1:nrow(df), function(x) {

    distanceMatrix <- geosphere::distm(data.frame(longitude = df[["lon"]][[x]],
                                                  latitude = df[["lat"]][[x]]),
                                       data.frame(longitude = cdecGPSFiltered[["longitude"]],
                                                  latitude = cdecGPSFiltered[["latitude"]]),
                                       fun = geosphere::distVincentyEllipsoid)

    distanceData <- data.frame(cdecStation = cdecGPSFiltered[["station"]],
                               distance = as.vector(distanceMatrix)/1609.344
                               # stationOfInterest = df[["station"]][x]
                               )
    distanceData <- distanceData[order(distanceData[["distance"]]), ]

    # If you are asking for top temperature, removing sensors that are on the bottom; if you are asking for
    # bottom sensors, will also give you top sensor--very few bottom sensors out there.
    if (waterColumn != "bottom") {
      gageWaterColumn <- closestGages[!grepl("(lower|bottom)", x = closestGages[["sensorDescription"]],
                                             ignore.case = T), ]
    }

    metadata <- merge(distanceData[which.min(distanceData[["distance"]]),  ], gageWaterColumn,
          by.x = "cdecStation", by.y = "gage", all.x = T)

    metadata$rowIndex <- x
    metadata
  })
}

#' Populate the closest CDEC station data.
#'
#' @description
#' This function will search for the closest CDEC station to each point within
#' your data frame and retrieve water tempearture, turbidity, or
#' electroconductivity data from the closest time point to the sampling time of
#' your point of interest. If a CDEC gage does not have data of interest, either
#' in terms of the sensor of interest or at the time point of interest, `NA` or
#' the value of the next closest time point for that station will be provided,
#' if available.
#'
#'
#' @param df A data frame with your station name (`station`), latitude (`lat`),
#' longitude (`lon`), and time (`time`). Ensure that `time` is a date-time
#' format, YYYY-MM-DD HH:MM:SS.
#' @param cdecClosest A list of the closest stations per coordinate of interest.
#' If not provided, `calcNearestCDEC()` will be used on the provided `df`.
#' @param variable Which water quality variable are you after. Supports only
#' water temperature (`temp`), turbidity (`turbidity`), or electroconductivity
#' (`ec`). Will default to `temp`.
#' @param waterColumn Where in the water column to look for sensor data, top
#' (`top`) or bottom (`bottom`)? Will default to `top`
#'
#' @return A data frame with water quality of interest from the closest CDEC
#' gage at the closest time stamp.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(station = "306", lat = 38.00064,
#' lon = -122.4136, time = "2023-01-01 10:00:00", temp = 10)
#'
#' popCDEC(df)
#' }
popCDEC <- function(df,
                    cdecClosest = NULL,
                    variable = c("temp", "turbidity", "ec"),
                    waterColumn = c("top", "bottom")) {

  # Input validation and preprocessing
  requiredCols <- c("station", "lat", "lon", "time")
  names(df) <- tolower(names(df))

  if (!all(requiredCols %in% names(df))) {
    stop(sprintf("Missing required columns: %s",
                 paste(setdiff(requiredCols, names(df)), collapse = ", ")),
         call. = FALSE)
  }

  if (nrow(df) == 0) return(data.frame())

  # Validate and match arguments
  variable <- match.arg(variable)
  if (!variable %in% names(df)) {
    stop(sprintf("Variable '%s' not found in dataset", variable),
         call. = FALSE)
  }

  waterColumn <- match.arg(waterColumn)

  # Define variable patterns
  variablePatterns <- list(
    ec = "elec.* conduct.* micro",
    temp = "(temp).*(water)",
    turbidity = "turbidity"
  )

  variableWanted <- variablePatterns[[variable]]
  waterColumnWanted <- if(waterColumn == "bottom") "(lower|bottom)" else waterColumn

  # Get CDEC data
  if (is.null(cdecClosest)) {
    cdecClosest <- calcNearestCDEC(df,
                                   variable = variableWanted,
                                   waterColumn = waterColumnWanted)
  }

  # Process CDEC closest stations
  cdecClosestDF <- do.call(rbind, lapply(cdecClosest, function(x) {
    if (all(!is.na(x$duration))) {
      x$duration <- factor(x$duration, levels = c("event", "hourly", "daily"))
      x[which.min(as.integer(x$duration)), ]
    } else {
      x[1, ]
    }
  }))

  # Prepare data frame for merging
  df$rowIndex <- seq_len(nrow(df))
  relevantCols <- c(requiredCols, "rowIndex", variable)
  dfMerged <- merge(df[relevantCols], cdecClosestDF, by = "rowIndex", all = TRUE)

  # Pull and process CDEC data
  dfSplitDuration <- split(dfMerged, list(dfMerged$duration, dfMerged$sensorNumber))

  pulledData <- do.call(rbind, lapply(dfSplitDuration, function(x) {
    if (nrow(x) == 0) return(NULL)

    dateRange <- range(as.Date(x$time))
    cdecData <- pullCDEC(
      station = unique(x$cdecStation),
      sensor = unique(x$sensorNumber),
      duration = as.character(unique(x$duration)),
      dateStart = dateRange[1],
      dateEnd = dateRange[2] + 1
    )

    if (is.null(cdecData) || nrow(cdecData) == 0) return(x)

    merged <- merge(x, cdecData,
                    by.x = "cdecStation",
                    by.y = "stationId",
                    all.x = TRUE)

    merged$timeDifference <- abs(difftime(merged$time,
                                          merged$dateTime,
                                          units = "mins"))

    do.call(rbind, by(merged,
                      list(merged$station, merged$time),
                      function(subset) {
                        validRows <- !is.na(subset$value)
                        if (!any(validRows)) return(subset[1, ])
                        subset <- subset[validRows, ]
                        subset[which.min(subset$timeDifference), ]
                      }))
  }))
  row.names(pulledData) <- NULL

  # Combine and format final output
  remainingData <- dfMerged[!dfMerged$cdecStation %in% pulledData$cdecStation, ]
  allCols <- union(names(pulledData), names(remainingData))

  remainingData[setdiff(allCols, names(remainingData))] <- NA
  pulledData[setdiff(allCols, names(pulledData))] <- NA

  combinedData <- rbind(remainingData, pulledData)

  # Select and rename final columns
  finalCols <- c("station", "lat", "lon", "time", "cdecStation",
                 grep(paste0("^", variable), names(combinedData), value = TRUE),
                 "value", "timeDifference", "distance", "sensorNumber.x",
                 "sensorDescription", "units", "duration.x", "dataAvailable")

  result <- combinedData[order(combinedData$rowIndex), finalCols]

  # Rename columns
  colsToRename <- c("value", "sensorNumber.x", "duration.x")
  newNames <- c(paste0(variable, "CDEC"), "sensorNumber", "duration")
  names(result)[match(colsToRename, names(result))] <- newNames

  # Return final result with original column order preserved
  result[, c(setdiff(names(df), "rowIndex"),
             setdiff(names(result), names(df)))]
}

# parPopCDEC <- function(df,
#                        cdec,
#                        metadata,
#                        variable = c("temp", "turbidity", "ec"),
#                        waterColumn = c("top", "lower")) {
#   library(parallel)
#   cl <- makeCluster(detectCores())
#   clusterEvalQ(cl, {library(dplyr); library(httr)})
#   clusterExport(cl, varlist = c("pullCDEC", "metadata"))
#
#   variableWanted <- ifelse(variable %in% "ec", "ELEC.* CONDUCT.* MICRO", variable)
#   waterColumnWanted <- ifelse(waterColumn %in% "bottom", "(lower|bottom)", waterColumn)
#
#   joinedDF <- left_join(df, cdec, by = "Station") %>%
#     data.frame() %>%
#     pivot_longer(c(first, second, third),
#                  names_to = "priority", values_to = "cdecGage")
#
#   filteredMetadata <- parLapply(cl, na.omit(unique(joinedDF$cdecGage)), function(x) {
#
#     dfFiltered <- metadata[[x]] %>%
#       filter(grepl(variableWanted, sensorDescription, ignore.case = T))
#
#     if (nrow(dfFiltered) > 1) {
#       if (waterColumnWanted == "lower") {
#         dfFilteredWaterColumn <- dfFiltered %>%
#           filter(grepl(waterColumnWanted, sensorDescription, ignore.case = T))
#         if (nrow(dfFilteredWaterColumn) != 0) {
#           dfFiltered <- dfFilteredWaterColumn
#         }
#       } else {
#         dfFilteredWaterColumn <- dfFiltered %>%
#           filter(!grepl("lower", sensorDescription, ignore.case = T))
#         if (nrow(dfFilteredWaterColumn) != 0) {
#           dfFiltered <- dfFilteredWaterColumn
#         }
#       }
#     }
#
#     duration <- pull(dfFiltered, duration) %>%
#       factor(levels = c("event", "hourly", "daily")) %>%
#       sort()
#
#     if (length(duration) == 0) {
#       message("Sensor ", dQuote(x), " does not have sensor data for ", dQuote(variable), ".")
#       return(data.frame())
#     }
#
#     duration <- duration[[1]]
#
#     dfFiltered %>%
#       filter(duration == !!duration) %>%
#       transmute(cdecGage = gage,
#                 SensorNumber = sensorNumber,
#                 Duration = duration)
#   }) %>%
#     bind_rows() %>%
#     right_join(joinedDF, by = "cdecGage") %>%
#     relocate(c(cdecGage, SensorNumber, Duration), .after = everything()) %>%
#     arrange(TowTime, priority) %>%
#     mutate(cdecGage = ifelse(is.na(SensorNumber) | is.na(Duration),
#                              NA, cdecGage))
#
#   joinedDFCDEC <- filteredMetadata %>%
#     filter(!is.na(cdecGage)) %>%
#     mutate(rowNumber = row_number())
#
#   splitGroups <- joinedDFCDEC %>%
#     mutate(group = paste(Station, SensorNumber, Duration, sep = "_")) %>%
#     group_by(group) %>%
#     split(f = .$group)
#
#   pulledData <- parLapply(cl, splitGroups, function(x) {
#
#     dates <- pull(x, which(sapply(x, function(x) inherits(x, "Date"))))
#
#     cdecStations <- unique(x$cdecGage)
#
#     df <- pullCDEC(station = cdecStations,
#                    sensor = unique(x$SensorNumber),
#                    duration = unique(x$Duration),
#                    dateStart = min(dates),
#                    dateEnd = max(dates) + 1) %>%
#       right_join(x, by = c("obsDate" = "SampleDate", "stationId" = "cdecGage"))
#
#     if (is.null(df)) return(data.frame(valueCDEC = NA,
#                                        closestTime = NA))
#
#     towTimeIndex <- sym(names(df)[which(sapply(df, function(x) inherits(x, "POSIXct")) & !names(df) %in% "dateTime")])
#
#     df <- df %>%
#       mutate(closestTime = abs(difftime(!!towTimeIndex, dateTime, units = "mins"))) %>%
#       group_by(rowNumber) %>%
#       slice(which.min(closestTime)) %>%
#       transmute(stationId,
#                 valueCDEC = value,
#                 closestTime) %>%
#       ungroup() %>%
#       right_join(x, by = "rowNumber") %>%
#       select(rowNumber, valueCDEC, closestTime)
#
#     if (nrow(df) != nrow(x)) warning("Could not pull CDEC data for some outlying datapoints.")
#     df
#   }) %>%
#     bind_rows() %>%
#     arrange(rowNumber) %>%
#     select(-rowNumber)
#
#   stopCluster(cl)
#
#   joinedDFCDEC %>%
#     select(-rowNumber) %>%
#     bind_cols(pulledData) %>%
#     bind_rows(joinedDF %>%
#                 filter(is.na(cdecGage))) %>%
#     pivot_wider(names_from = "priority",
#                 values_from = c("cdecGage", "SensorNumber", "Duration", "valueCDEC", "closestTime")) %>%
#     relocate(contains("valueCDEC"), .before = Mean)
# }
