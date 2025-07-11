#' Pulling CDEC gage data
#'
#' @description
#' This function will first pull the metadata of a CDEC gage. This allows the
#' function to direct the user accordingly if various arguments are missing.
#' Once all arguments are provided, the query is created and data downloaded.
#'
#'
#' @param station A character vector of station names. Can be multiple station.
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
#' @param verbose Logical. Should the function provide a narrative of its operations?
#' Defaults to `TRUE`.
#' @param maxAttempt Number of retries to attempt if a connection fails. Defaults to 3.
#' @param fallbackDuration Logical. Should the function try to use a coarser
#' duration if data cannot be found? Order is event > hourly > daily, in that order, never backwards.
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
pullCDEC <- function(station, sensor = NULL, duration = c("event", "hourly", "daily"),
                     dateStart, dateEnd = NULL, temperatureUnits = c("C", "F"),
                     coordinates, verbose = T, maxAttempt = 3, fallbackDuration = FALSE) {

  # --- Station or lat/lon ---
  if (!missing(coordinates) & !missing(station)) {
    warning("Both `station` and `coordinates` are provided. Ignoring `coordinates`.",
            call. = FALSE)
  }
  if (!missing(coordinates) & missing(station)) {
    if (length(coordinates) != 2)
      stop("`coordinates` should be a vector of two numbers, lat and lon.",
           call. = FALSE)
    # Assuming calcNearestCDEC is defined elsewhere or will be provided
    cdecClosest <- calcNearestCDEC(data.frame(lat = coordinates[[1]],
                                              lon = coordinates[[2]]))

    station <- unique(cdecClosest[["cdecStation"]])
  }

  # --- Metadata retrieval if missing ---
  if (is.null(sensor) | length(duration) > 1 | missing(dateStart)) {
    # Retrieve metadata logic
    webpage <- session(paste0("http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=",
                                     station))
    table <- read_html(webpage)
    table <- html_elements(table, "table")
    if (length(table) != 0) {
      session <- table[[which(grepl("Sensor Description",
                                    table))]]
    }
    else {
      stop("Check station name. No table was found.")
    }
    tableNames <- html_elements(session, "th")
    tableNames <- html_text(tableNames)
    tableNames <- gsub("\\s", "", tableNames)
    tableNames <- gsub("(^[A-Z])", "\\L\\1",
                       tableNames, perl = T)
    availableData <- html_table(session)
    if (nrow(availableData) == 0) {
      availableData <- lapply(tableNames, function(x) {
        df <- data.frame(name = NA)
        names(df) <- x
        df
      })
      names(availableData) <- tableNames
      availableData <- do.call(cbind, availableData)
    }
    else {
      availableData <- setNames(availableData, tableNames)
      availableData <- availableData[order(availableData[["sensorNumber"]]),
      ]
      availableData$duration <- gsub("\\(|\\)", "",
                                     availableData[["duration"]])
      availableData$gage <- station
    }

    if (isTRUE(verbose)) {
      print(availableData, n = Inf, width = Inf)
      message("Please provide sensor #, duration, and start and end dates.")
    }
    return(invisible(availableData))
  }
  # --- Arguments setup ---
  station <- unique(station)
  temperatureUnits <- match.arg(temperatureUnits)
  originalDuration <- match.arg(duration) # Store original duration for fallback logic
  durationCode <- switch(originalDuration,
                         "event" = "E",
                         "hourly" = "H",
                         "daily" = "D")

  dateStart <- parseDate(dateStart)
  dateEnd <- if (is.null(dateEnd)) Sys.Date() else parseDate(dateEnd)

  if (dateStart > dateEnd) {
    stop("`dateStart` cannot be after `dateEnd`.", call. = FALSE)
  }

  # --- Pagination check ---
  estimatedRows <- estimateCdecRows(length(station),
                                    duration,
                                    dateStart,
                                    dateEnd)
  if (estimatedRows > 2000000) {
    stop(sprintf(
      "This request is estimated to return ~%s rows, which exceeds the safe limit of %s.\n  Please use `batchCDEC()` instead for this request.",
      format(estimatedRows, big.mark = ","),
      format(2000000, big.mark = ",", scientific = FALSE)
    ), call. = FALSE)
  }
  # --- Data grab ---
  df <- fetchCDECData(station, sensor, durationCode, dateStart, dateEnd) # Internal function

  # --- Fallback logic, if applicable ---
  if (nrow(df) == 0) {
    if (fallbackDuration) {
      # Define the fallback chain
      durationOrder <- c("event", "hourly", "daily")
      # Find current duration's position
      currentPos <- match(originalDuration, durationOrder)
      # Try next durations in order
      for (nextPos in seq(currentPos + 1, length(durationOrder))) {
        nextDuration <- durationOrder[nextPos]
        if (isTRUE(verbose)) {
          message(sprintf("No data found for %s duration. Attempting fallback to duration %s.",
                          originalDuration, nextDuration))
        }
        nextDur <- switch(nextDuration,
                          "event" = "E",
                          "hourly" = "H",
                          "daily" = "D")
        # Recursive call with fallbackDuration=FALSE to avoid infinite recursion
        fallbackDf <- fetchCDECData(station, sensor, nextDur, dateStart, dateEnd)
        if (nrow(fallbackDf) > 0) {
          df <- fallbackDf
          message(sprintf("Successfully downloaded %s data.", nextDuration))
          break()
        }
      }
    }

    if (nrow(df) == 0) {
      # If no data found in any fallback duration, return empty data frame
      warning("No data available for station ", station, " as specified.", call. = F)
      return(data.frame())
    }
  }

  df$dateTime <- as.POSIXct(df[["dateTime"]], format = "%Y%m%d %H%M",
                            tz = "America/Los_Angeles")

  # # --- Data range validation and gap filling ---
  # actualMinDate <- min(df$dateTime, na.rm = TRUE)
  # actualMaxDate <- max(df$dateTime, na.rm = TRUE)
  #
  # # Check for missing start/end dates
  # if (actualMinDate > dateStart) {
  #   if (isTRUE(verbose)) {
  #     message(sprintf("Downloaded data starts on %s, but requested start was %s. Attempting to fill leading gap...", actualMinDate, dateStart))
  #   }
  #   missingLeadingData <- fetchCDECData(station, sensor, durationCode,
  #                                       dateStart, lubridate::ceiling_date(actualMinDate, unit = "day"))
  #   missingLeadingData$dateTime <- as.POSIXct(missingLeadingData[["dateTime"]], format = "%Y%m%d %H%M",
  #                                                tz = "America/Los_Angeles")
  #   # Filter for boundary data only
  #   missingLeadingData <- missingLeadingData[missingLeadingData$dateTime < actualMinDate, ]
  #   additionalDataPoints <- nrow(missingLeadingData)
  #   if (additionalDataPoints > 0) {
  #     cat("Found", additionalDataPoints, "additional data points.\n")
  #     df <- rbind(missingLeadingData, df)
  #     appendedData <- TRUE
  #   } else {
  #     cat("No additional data found.\n")
  #     appendedData <- FALSE
  #   }
  # }
  #
  # if (actualMaxDate < dateEnd) {
  #   if (isTRUE(verbose)) {
  #     message(sprintf("Downloaded data ends on %s, but requested end was %s. Attempting to fill trailing gap...", actualMaxDate, dateEnd))
  #   }
  #   missingTrailingData <- fetchCDECData(station, sensor, durationCode,
  #                                        lubridate::floor_date(actualMaxDate, unit = "day"),
  #                                        dateEnd)
  #   missingTrailingData$dateTime <- as.POSIXct(missingTrailingData[["dateTime"]], format = "%Y%m%d %H%M",
  #                                                 tz = "America/Los_Angeles")
  #   # Filter for boundary data only
  #   missingTrailingData <- missingTrailingData[missingTrailingData$dateTime > actualMaxDate, ]
  #   additionalDataPoints <- nrow(missingTrailingData)
  #   if (nrow(missingTrailingData) > 0) {
  #     if (verbose) message("Found", additionalDataPoints, "additional data points.\n")
  #     df <- rbind(df, missingTrailingData)
  #   } else {
  #     if (verbose) message("No additional data found.")
  #   }
  # }

  # --- Data cleaning ---
  df$value <- as.double(ifelse(df[["value"]] == "---",
                               NA, df[["value"]]))
  df$dataFlag <- as.character(ifelse(df[["dataFlag"]] == " ",
                                     NA, df[["dataFlag"]]))
  df$obsDate <- as.Date(df[["obsDate"]], format = "%Y%m%d %H%M")

  # Ensure df is ordered after potential rbinds
  df <- df[order(df$dateTime), ]

  # --- Unit Conversion ---
  if (any(unique(df[["units"]]) %in% "DEG F") &
      temperatureUnits == "C") {
    df$value <- ifelse(df[["units"]] == "DEG F",
                       (df[["value"]] - 32) * 5/9, df[["value"]])
    df$units <- ifelse(df[["units"]] == "DEG F",
                       "DEG C", df[["units"]])
  }
  if (any(unique(df[["units"]]) %in% "DEG C") &
      temperatureUnits == "F") {
    df$value <- ifelse(df[["units"]] == "DEG C",
                       (df[["value"]] * 9/5) + 32, df[["value"]])
    df$units <- ifelse(df[["units"]] == "DEG C",
                       "DEG F", df[["units"]])
  }
  df
}

#' Pulling CDEC gage metadata
#'
#' @param gage Name of the CDEC gage, a singular value
#' @param maxAttempt Number of times to retry a scrape. Defaults to 3.
#' @param timeout Seconds before a connection is terminated. Defaults to 60.
#' @param verbose Should the function annotate its progress? Defaults to TRUE.
#'
#' @description
#' A function to pull the metadata table associated with a CDEC gage.
#'
#' @return A list containing the location and sensor metadata of a CDEC station.
#' @export
#'
#' @importFrom httr timeout content
#' @importFrom rvest read_html html_element html_elements html_text html_table
#'
#' @examples
#' \dontrun{
#' pullMetadataCDEC("MAL")
#' }
pullMetadataCDEC <- function(gage, maxAttempt = 3, timeout = 60, verbose = TRUE) {

  # --- Network Request Block with Retry Logic ---
  url <- paste0("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", gage)
  # Retry logic now wrapped in internal function retryGet
  response <- retryGet(url, maxAttempt = maxAttempt, timeout(timeout))

  # --- HTML Parsing Block ---
  # If the code reaches here, 'response' is guaranteed to be a successful one.
  htmlContent <- content(response, as = "text", encoding = "UTF-8")
  page <- read_html(htmlContent)

  # Check for "Station Not Found" on the page content itself
  titleElement <- html_element(page, "h1")
  if (!is.na(titleElement) && grepl("Station Not Found", html_text(titleElement), ignore.case = TRUE)) {
    stop(sprintf("Gage '%s' not found on CDEC. The page exists but contains no station data.", gage), call. = FALSE)
  }

  # Target the metadata table using a robust selector
  locationMeatadata <- html_element(page, xpath = "//table[contains(., 'Latitude')]")
  sensorMetadata <- html_element(page, xpath = "//table[contains(., 'Sensor Description')]")

  locationDf <- html_table(locationMeatadata)
  sensorDf <- html_table(sensorMetadata)

  # --- Cleaning the Tables ---
  nColumns <- ncol(locationDf)

  # Location table first
  # Every odd will be assumed to be the key and even the values
  key <- unlist(locationDf[, seq(1, nColumns, by = 2)], use.names = FALSE)
  value <- unlist(locationDf[, seq(2, nColumns, by = 2)], use.names = FALSE)
  locationDf <- data.frame(key = key, value = value)

  # Sensor metadata
  tableNames <- html_elements(sensorMetadata, "th")
  tableNames <- html_text(tableNames)
  names(sensorDf) <- tableNames

  list(location = locationDf,
       sensor = sensorDf)
}

#' Pull CDEC gage lat/lon
#'
#' @param gage Name of the gage of interest, as a character.
#' @param maxAttempt Number of attempts to retry the same pull
#' @param timeout Max duration to wait for a download, in seconds
#'
#' @return A data frame containing the station name, lat, and lon.
#' @export
#'
#' @importFrom httr content http_status
#' @importFrom rvest html_element html_text
#'
#' @examples
#' \dontrun{
#' pullCoordinates("MAL")
#' }
pullCoordinates <- function(gage, maxAttempt = 3, timeout = 60) {

  response <- retryGet(paste0("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", gage),
                              maxAttempt = maxAttempt, timeout = timeout)

  htmlContent <- content(response, as = "text", encoding = "UTF-8")
  page <- read_html(htmlContent)
  # After the loop, check if we ultimately failed.
  if (is.null(response) || http_status(response)$category != "Success") {
    stop(sprintf("Failed to retrieve data for gage '%s' after %d attempts.", gage, maxAttempt), call. = FALSE)
  }

  dataString <- html_element(page, "table")
  dataString <- html_text(dataString)

  data.frame(station = regmatches(dataString, regexpr("(?<=Station ID)(.*)(?=Elevation)", dataString, perl = T)),
             latitude = regmatches(dataString, regexpr("(?<=Latitude)([\\d.-]+)", dataString, perl = T)),
             longitude = regmatches(dataString, regexpr("(?<=Longitude)([\\d.-]+)", dataString, perl = T)))
}

#' Find the Nth nearest CDEC station with specific data
#'
#' Identifies the n^{th} nearest CDEC station to one or more
#' input coordinates. It can filter station based on a specific sensor number
#' or by a general variable and water column combination. By default, all CDEC
#' station are searched.
#'
#' @param df A data.frame with at least 'lat' and 'lon' columns. All other columns
#' will be retained.
#' @param n The rank of the nearest station to find (e.g., n = 1 for the closest).
#' If n = "all", all applicable station and metadata will be returned in a nested format per point.
#' @param sensor An optional integer sensor number. If provided, this will override
#'   the 'variable' and 'waterColumn' arguments.
#' @param variable The type of sensor data required. One of "temp", "turbidity", "ec".
#'   Ignored if 'sensor' is provided.
#' @param waterColumn The position of the sensor in the water column. One of "top" or "bottom".
#'   Ignored if 'sensor' is provided.
#' @param method Determines the distance calculation function. `fast` will utilize the
#' Haversine method, while `accurate` the  Vincenty Ellipsoid method. For use within the Delta,
#' the Haversine method is sufficient and is much less computationally intensive.
#' Defaults to `fast`.
#' @param verbose Set to FALSE to disable specific messages. Defaults to TRUE.
#' @param cdecGPS Internal package data with station GPS locations. Can provide this
#' if the internal package data is not updated, although you will have to adhere to formatting.
#' @param cdecMetadata Internal package data with sensor metadata. Can provide this
#' if the internal package data is not updated, although you will have to adhere to formatting.
#'
#' @return A data.frame containing the input point identifiers,
#' the found CDEC station, the distance, and the sensor metadata.
#' @export
#'
#' @importFrom geosphere distm distVincentyEllipsoid distHaversine
#'
#' @examples
#' \dontrun{
#' df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)
#'
#' calcNearestCDEC(df)
#' }
calcNearestCDEC <- function(df, n = 1,
                            sensor = NULL,
                            variable = c("temp", "turbidity", "ec"),
                            waterColumn = c("top", "bottom"),
                            method = c("fast", "accurate"),
                            verbose = T,
                            cdecGPS = deltadata:::cdecStation,
                            cdecMetadata = deltadata:::cdecMetadata) {

  # --- Validation ---
  if (!all(c("lat", "lon") %in% names(df))) {
    stop("Input `df` must contain 'lat' and 'lon' columns.", call. = FALSE)
  }

  method <- match.arg(method)

  # --- Filter available CDEC station ---
  if (!is.null(sensor)) {
    # --- Path 1: if sensor is provided ---
    availableSensors <- cdecMetadata[cdecMetadata$sensorNumber == sensor, ]

    if (nrow(availableSensors) == 0) {
      stop(sprintf("The specified sensor '%s' was not found in the CDEC metadata.", sensor), call. = FALSE)
    }

  } else {
    # --- Path 2: no sensors provided ---
    if (verbose) {
      if (length(variable) > 1) {
        if (length(waterColumn) > 1) message("No variable or water column selected. Defaulting to `temp` and `top`.")
        else message("No variable selected. Defaulting to `temp`.")
      } else {
        if (length(waterColumn) > 1) message("No water column selected. Defaulting to `top`.")
      }
    }

    variable <- match.arg(variable)
    waterColumn <- match.arg(waterColumn)

    variableRegexMap <- list(
      temp      = "(temp).*(water)",
      turbidity = "turbidity",
      ec        = "elec.* conduct.* micro"
    )
    variablePattern <- variableRegexMap[[variable]]
    bottomPattern <- "(lower|bottom)"

    availableSensors <- cdecMetadata
    availableSensors <- availableSensors[grepl(variablePattern, availableSensors$sensorDescription, ignore.case = TRUE), ]

    if (waterColumn == "top") {
      availableSensors <- availableSensors[!grepl(bottomPattern, availableSensors$sensorDescription, ignore.case = TRUE), ]
    } else {
      availableSensors <- availableSensors[grepl(bottomPattern, availableSensors$sensorDescription, ignore.case = TRUE), ]
    }

    if (nrow(availableSensors) == 0) {
      stop("No CDEC station found with the specified sensor criteria.", call. = FALSE)
    }
  }

  # --- Post filter processing ---

  availableSensors$duration <- gsub("\\(|\\)", "",
                                    availableSensors[["duration"]])
  validstation <- unique(availableSensors$gage)
  cdecGpsFiltered <- cdecGPS[cdecGPS$station %in% validstation, ]

  # --- Calculate distance matrix ---

  distanceFunction <- if (method == "fast") {
    distHaversine
  } else {
    distVincentyEllipsoid
  }

  distanceMatrixMeter <- distm(
    df[, c("lon", "lat")],
    cdecGpsFiltered[, c("longitude", "latitude")],
    fun = distanceFunction
  )
  distanceMatrix <- distanceMatrixMeter / 1609.344

  # --- Filter station by n and format output ---
  # For n = "all", the output will be a nested data frame to keep a
  # consistent output format (a data frame with each point being a row)

  if (is.character(n) && n == "all") {
    # --- Path 1: n = "all" ---
    if (verbose) {
      message("n = 'all': Returning a data frame with a nested list-column 'cdecStation'.")
      message("Use `tidyr::unnest(yourObject, cdecstation)` to expand the results.")
    }

    # Use apply to process each input point (each row of the distance matrix)
    rankedResults <- apply(distanceMatrix, 1, function(dist) {

      # Create a data frame of all possible station and their distances
      results <- data.frame(
        cdecStation = cdecGpsFiltered$station,
        distance = dist
      )

      # Order the results by distance
      results <- results[order(results$distance), ]

      # Merge in the sensor metadata for complete information
      merge(results, availableSensors,
            by.x = "cdecStation", by.y = "gage",
            sort = F)
    })

    # Create the final data frame with the list-column
    resultDf <- df
    resultDf$cdecstation <- rankedResults

    return(resultDf)

  } else {
    # --- Path 2: n is numeric ---
    if(length(n) > 1) {
      n <- n[1]
      warning("n has length > 1. Using only the first element.\n")
    }

    indices <- apply(distanceMatrix, 1, function(row) order(row)[n])
    nthStation <- cdecGpsFiltered[indices, ]
    nthDistance <- distanceMatrix[cbind(1:nrow(df), indices)]

    resultDf <- cbind(
      df,
      cdecStation = nthStation$station,
      distance = nthDistance,
      rowIndex = 1:nrow(df)
    )

    mergedDf <- merge(
      resultDf,
      availableSensors,
      by.x = "cdecStation",
      by.y = "gage",
      all.x = TRUE
    )

    # Will return only 1 data row, prioritizing: event > hourly > daily
    # There's a decision between the different sensor types, but will deal with that as necessary
    mergedDf$duration <- factor(mergedDf$duration, levels = c("event", "hourly", "daily"))
    mergedDf <- mergedDf[order(mergedDf$duration), ]
    # Seems a bit loose but I think this works fine if the goal is to get the first row regardless of sensorNumber
    resultDf <- mergedDf[!duplicated(mergedDf$rowIndex), ]
    resultDf$rowIndex <- NULL

    return(resultDf)
  }
}

#' Find the n^{th} closest CDEC gage
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Identifies the n^{th} nearest CDEC gage to a lat/lon of interest. This function
#' requires metadata of all CDEC station of interest. By default, all CDEC
#' station are used.
#'
#' @note
#' This functionality has been incorporated into \code{\link{calcNearestCDEC}}.
#'
#' @param df A data frame that contains at least the lat/lon of station(s) of
#' interest, named as `lat` and `lon`.
#' @param n A number reflecting the desired relative distance of the CDEC gage from
#' the lat/lon of interest. n=1 means return the closest gage, n=2 means return the
#' second closest gage, etc. n should be an integer or should otherwise be convertible
#' to an integer.
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
#' @return A data frame of the metadata of the n^{th} closest CDEC station to your
#' point of interest that has data for the variable of interest.
#' @export
#'
#' @importFrom geosphere distm distVincentyEllipsoid
#'
#' @examples
#' \dontrun{
#' df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)
#'
#' calcNthNearestCDEC(df)
#' }
calcNthNearestCDEC <- function(df, n = 1,
                               cdecGPS = deltadata:::cdecStation,
                               cdecMetadata = deltadata:::cdecMetadata,
                               variable = c("temp", "turbidity", "ec"),
                               waterColumn = c("top", "bottom")) {

  lifecycle::deprecate_warn(
    when = "0.1.0",
    what = "calcNthNearestCDEC()",
    with = "calcNearestCDEC()",
    details = paste(
      "The new `calcNearestCDEC()` provides a more robust approach to this operation but may not be a direct replacement.",
      "Please review your code to ensure it is compatible with the new function as this function will be removed in the future."
    )
  )

  names(df) <- tolower(names(df))

  if (all(!c("station", "lat", "lon") %in% names(df))) stop("Your `df` must contain at least `station`, `lat, `lon`.", call. = F)

  if(length(n) > 1) {
    n <- n[1]
    warning("n has length > 1. Using only the first element.\n")
  }
  n <- as.integer(n)
  if(is.na(n)) {
    stop("Could not convert n to an integer.\n")
  }

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
  if(n > nrow(cdecGPSFiltered)) {
    stop("n is larger than the number of available station.\n")
  }

  lapply(1:nrow(df), function(x) {

    distanceMatrix <- distm(data.frame(longitude = df[["lon"]][[x]],
                                                  latitude = df[["lat"]][[x]]),
                                       data.frame(longitude = cdecGPSFiltered[["longitude"]],
                                                  latitude = cdecGPSFiltered[["latitude"]]),
                                       fun = distVincentyEllipsoid)

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

    metadata <- merge(distanceData[n, ], gageWaterColumn,
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
#' @param cdecClosest A data frame of the closest station per coordinate of interest.
#' This df should include three columns: `cdecStation` (the CDEC station name),
#' `sensorNumber` (sensor number that you're interested in), and `duration`
#' (the sampling interval of interest). If not provided, `calcNearestCDEC()` will
#' automatically populate the closest cdec station per sampling location from `df`.
#' @param variable Which water quality variable are you after. Supports only
#' water temperature (`temp`), turbidity (`turbidity`), or electroconductivity
#' (`ec`). Will default to `temp`.
#' @param waterColumn Where in the water column to look for sensor data, top
#' (`top`) or bottom (`bottom`)? Will default to `top`
#' @param ... Additional parameters to be passed onto `calcNearestCDEC()`.
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
                    waterColumn = c("top", "bottom"),
                    ...) {

  # --- Validation and preprocessing ---
  requiredCols <- c("station", "lat", "lon", "time")
  names(df) <- tolower(names(df))

  if (!all(requiredCols %in% names(df))) {
    stop(sprintf("Missing required columns: %s",
                 paste(setdiff(requiredCols, names(df)), collapse = ", ")),
         call. = FALSE)
  }

  if (nrow(df) == 0) return(data.frame())

  variable <- match.arg(variable)
  if (!variable %in% names(df)) {
    stop(sprintf("Variable '%s' not found in dataset", variable),
         call. = FALSE)
  }

  waterColumn <- match.arg(waterColumn)

  # The "time" column must be a time format in the form of %Y-%m-%d %h:%m:%s,
  # This is the same formatting as the CDEC data
  if (is.character(df$time)) df$time <- as.POSIXct(df$time,
                                                   tz = "America/Los_Angeles")
  if (all(is.na(df$time))) stop("Time column could not be parsed. Ensure the time is in `%Y-%m-%d %H:%M:%S` format", call. = F)

  # --- Get cdec data ---
  message("Step 1/2: Finding nearest CDEC stations with required data...")

  if (is.null(cdecClosest)) {
    cdecClosest <- calcNearestCDEC(df,
                                   variable = variable,
                                   waterColumn = waterColumn,
                                   ...)
  }

  # --- Pull the data, batch download ---
  dfSplitDuration <- split(cdecClosest, list(as.character(cdecClosest$duration),
                                             cdecClosest$sensorNumber),
                           drop = T)
  message("Step 2/2: Downloading and finding nearest CDEC value...")

  pulledData <- do.call(rbind, lapply(dfSplitDuration, function(durationSensorGroup) {
    if (nrow(durationSensorGroup) == 0) return(NULL)

    dateRange <- range(as.Date(durationSensorGroup$time))

    cdecData <- pullCDEC(
      station = unique(durationSensorGroup$cdecStation),
      sensor = unique(durationSensorGroup$sensorNumber),
      duration = as.character(unique(durationSensorGroup$duration)),
      dateStart = dateRange[1] - 1, # Add a 1-day buffers
      dateEnd = dateRange[2] + 1
    )

    if (is.null(cdecData) || nrow(cdecData) == 0) return(NULL)

    # Split the location of interset data into groups of its nearest cdec station
    cdecDataPerStation <- split(durationSensorGroup, durationSensorGroup$cdecStation)

    # To each sampling point, find closest time point from its closest cdec station
    results <- lapply(cdecDataPerStation, function(cdecDf) {

      stationId <- cdecDf$cdecStation[1]
      cdecStationDf <- cdecData[cdecData$stationId == stationId, ]

      # If there's no CDEC data for this station, return the original user data.
      if (nrow(cdecStationDf) == 0) {
        # Add NA columns to ensure rbind works later.
        cdecDf[[paste0(variable, "CDEC")]] <- NA_real_
        cdecDf$timeDifference <- NA_real_
        return(cdecDf)
      }

      # Use findInterval() to find location right before (index) the time of interest (sampling time)
      # Sort first as findInterval must have an increasing vector to compare to in 'vec'
      cdecStationDf <- cdecStationDf[order(cdecStationDf$dateTime), ]

      # Since sampling time might be in between the sampling interval of the CDEC gage,
      # need to check both sides of this index to see which is closer as findInterval returns floor
      lowerIndex <- findInterval(cdecDf$time, cdecStationDf$dateTime)
      # Time difference with the point before
      diffLowerIndex <- abs(difftime(cdecDf$time, cdecStationDf$dateTime[lowerIndex], units = "secs"))
      # Time difference with the point after
      upperIndex <- pmin(lowerIndex + 1, nrow(cdecStationDf)) # To handle edge case of end of dataset
      diffUpperIndex <- abs(difftime(cdecDf$time, cdecStationDf$dateTime[upperIndex], units = "secs"))

      # Choose closest and construct final data frame
      closestIndex <- ifelse(diffLowerIndex < diffUpperIndex, lowerIndex, upperIndex)

      result <- cdecDf
      result[[paste0(variable, "CDEC")]] <- cdecStationDf$value[closestIndex]
      result$timeDifference <- as.numeric(pmin(diffLowerIndex, diffUpperIndex, na.rm = TRUE), units = "mins")

      return(result)
    })

    do.call(rbind, results)

  }))
  # --- Clean up ---
  rownames(pulledData) <- NULL
  finalNameOrder <- append(setdiff(names(pulledData), "cdecStation"), "cdecStation",
                           after = ncol(pulledData) - 3)

  # Want cdec station name at the end of the data frame with the value and time difference
  pulledData[, finalNameOrder]

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
#     cdecstation <- unique(x$cdecGage)
#
#     df <- pullCDEC(station = cdecstation,
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

#' Batch download CDEC data
#'
#' @description
#' Implements a workflow to allow for robustly downloading a large amount of CDEC
#' data. The CDEC API currently has a limit of returning a maximum of 3-4 million
#' rows after which it will stop transferring data. This function estimates how
#' large a data pull will be and paginates the request into more manageable chunks.
#' Defaults to paginate every 1 million rows.
#'
#'
#' @param station A character vector of station names. Can be multiple station.
#' @param sensor A singular sensor value of interest.
#' @param duration The duration data, can be `event`, `hourly`, or `daily` and
#' depends on data availability for the gage of interest.
#' @param dateStart Beginning date for the period of interest.
#' @param dateEnd Ending date for the period of interest. Will default to today
#' if left as `NULL`
#' @param rowLimit Maximum number of rows to download at once. Maximum from the
#' server appears to be around 3-4 million rows. Defaults to 1 million. A larger
#' value can be used but depending on internet speed may bog down.
#' @param ... Any other arguments to be passed onto \code{\link{pullCDEC}}.
#'
#' @return A data frame of the request data pull.
#' @export
#'
#' @importFrom dplyr distinct bind_rows
#'
#' @examples
#' \dontrun{
#' # This theoretically can be ran via pullCDEC as well since it does not return
#' # an excess amount of rows
#' batchCDEC(
#' station = c("BDT","DLC","DLC","DSJ","DWS","FAL","FCT","FPT","FPT","FPT","FPX","GES","GLC","GLC",
#' "GLE","GSS","HLT","HLT","HOL","HWB","IST","IST","JTR","LIB","LIS","LIS","LPS","M13",
#' "MAB","MAL","MDM","MDM","MIR","MOK","MRU","MSD","MSD","NMR","NSL","NSL","OBI","OBI",
#' "ODM","OH1","OH1","OH4","OH4","OLD","OMR","ORI","ORM","ORQ","ORX","OSJ","OSJ","PDC",
#' "PRI","RRI","RYF","RYI","SDC","SDC","SDI","SGG","SJC","SJD","SJG","SJJ","SJL","SJL",
#' "SMR","SOI","SPE","SRV","SSS","SUT","SXS","TOE","TRN","TSL","UCS","ULC","VCU","WCI"),
#' sensor = 20, duration = "event", dateStart = Sys.Date() - 90
#' )
#' }
batchCDEC <- function(station, sensor, duration, dateStart, dateEnd = NULL,
                      rowLimit = 1000000, ...) {

  # --- Validation and preprocessing ---
  dateStart <- parseDate(dateStart)
  dateEnd <- if (is.null(dateEnd)) Sys.Date() else parseDate(dateEnd)
  station <- unique(station)

  if (is.na(dateStart) || is.na(dateEnd) || dateStart > dateEnd) {
    stop("Invalid `dateStart` or `dateEnd` provided.", call. = FALSE)
  }

  # --- Dynamic pagination logic ---
  # Estimate the total size of the request
  totalEstimatedRows <- estimateCdecRows(
    numberstation = length(station),
    duration = duration,
    startDate = dateStart,
    endDate = dateEnd
  )

  cat(sprintf("Total estimated rows for this request: %s\n", format(totalEstimatedRows, big.mark = ",")))

  # Determine if pagination is needed and calculate chunk size
  if (totalEstimatedRows > rowLimit) {
    # Calculate how many chunks are needed to stay under the threshold
    numChunks <- ceiling(totalEstimatedRows / rowLimit)
    totalDays <- as.numeric(difftime(dateEnd, dateStart, units = "days")) + 1

    # Days per chunk required to achieve this
    chunkSizeInDays <- ceiling(totalDays / numChunks)

    cat(sprintf("Estimate exceeds threshold. Splitting download into %d chunks of approximately %d days each.\n",
                numChunks, chunkSizeInDays))
  } else {
    chunkSizeInDays <- as.numeric(difftime(dateEnd, dateStart, units = "days")) + 1
    numChunks <- ceiling(totalEstimatedRows / rowLimit)
  }

  # --- Pagination Loop ---
  allDataChunks <- list()
  currentDateStart <- dateStart
  chunk <- 1

  while (currentDateStart <= dateEnd) {
    currentDateEnd <- min(currentDateStart + chunkSizeInDays, dateEnd + 1)

    cat(sprintf("--- Fetching data from %s to %s, Chunk %s/%s ---\n",
                currentDateStart, currentDateEnd, chunk, numChunks))

    chunkDf <- pullCDEC(
      station = station,
      sensor = sensor,
      duration = duration,
      dateStart = currentDateStart,
      dateEnd = currentDateEnd,
      ...
    )

    if (nrow(chunkDf) > 0) {
      allDataChunks[[length(allDataChunks) + 1]] <- chunkDf
    }

    currentDateStart <- currentDateEnd
    chunk <- chunk + 1
  }

  # --- Final combination and cleaning ---
  cat("--- All phases complete. Combining data. ---\n")
  finalDf <- bind_rows(allDataChunks)

  if (nrow(finalDf) > 0) {
    # Efficiently remove duplicates that may occur at chunk boundaries
    finalDf <- distinct(finalDf, "stationId", "sensorNumber", "duration",
                               "dateTime", .keep_all = T)
  }

  return(finalDf)
}

#' Parse a date in character format to date format.
#'
#' @param date
#'
#' @return A date value
#' @noRd
#'
#' @keywords internal
parseDate <- function(date) {
  # use the first digits to determine potential formats
  firstDigits <- sub("(\\d+).*", "\\1", date)

  formats <- if (nchar(firstDigits) < 3) {
    # If the first number is 1 or 2 digits, it must be a month.
    c("%m/%d/%Y", "%m-%d-%Y")
  } else {
    # Otherwise, it must be a 4-digit year.
    c("%Y/%m/%d", "%Y-%m-%d")
  }

  parsed <- tryCatch({
    as.Date(date, tryFormats = formats)
  }, error = function(cond) {
    stop("Cannot parse date as provided. Supply the date in a standard YYYY-MM-DD or MM-DD-YYYY format.",
         call. = FALSE)
  })

  return(parsed)
}

#' Calculate a coarse estimate of the number of rows a data call may produce
#'
#' @param numberstation Number of station to be pulled
#' @param duration Duration type, used to estimate number of rows per day.
#' can be "event", "hourly", or "daily"
#' @param startDate Start date
#' @param endDate End date
#'
#' @return Numeric. Number of rows
#' @noRd
#'
#' @keywords internal
estimateCdecRows <- function(numberstation, duration, startDate, endDate) {
  # Define the number of records per day for each duration
  recordsPerDay <- list(
    "event"  = 96, # 4 records/hour * 24 hours
    "hourly" = 24, # 1 record/hour * 24 hours
    "daily"  = 1
  )

  if (!duration %in% names(recordsPerDay)) {
    stop("Unsupported duration. Please use 'event', 'hourly', or 'daily'.", call. = FALSE)
  }

  # Ensure dates are in Date format
  startDate <- parseDate(startDate)
  endDate <- parseDate(endDate)

  # Calculate the total number of days (inclusive)
  totalDays <- as.numeric(difftime(endDate, startDate, units = "days")) + 1

  # Calculate the total estimated rows
  estimatedRows <- numberstation * recordsPerDay[[duration]] * totalDays

  return(ceiling(estimatedRows)) # Return a whole number
}

#' Executes data pull request to the API server.
#'
#' @param station A vector of station(s).
#' @param currentSensor Sensor value
#' @param currentDurationCode Duration code value
#' @param currentStart Start date
#' @param currentEnd End date
#' @param maxAttempt Number of attempts to retry the same pull
#' @param timeout Max duration to wait for a download, in seconds
#' @param verbose Provide commentary of function progress. Defaults to TRUE
#'
#' @return A data frame of the requested data.
#' @noRd
#'
#' @importFrom httr GET timeout user_agent http_status content
#' @importFrom utils read.csv
#'
#' @keywords internal
fetchCDECData <- function(station, currentSensor, currentDurationCode,
                          currentStart, currentEnd,
                          maxAttempt = 3, timeout = 300, verbose = TRUE) {

  urlLink <- paste0("https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=",
                    paste(station, collapse = ","),
                    "&SensorNums=", currentSensor,
                    "&dur_code=", currentDurationCode,
                    "&Start=", currentStart,
                    "&End=", currentEnd)

  if (isTRUE(verbose))
    cat("Reading from:", urlLink, "\n")

  # Initialize a variable to hold the final data frame
  df <- NULL

  response <- retryGet(urlLink, maxAttempt = maxAttempt,
                       timeout = timeout(seconds = timeout))

  # # --- Validate response ---
  # # Reserved for future edge cases. Currently, this doesn't work as the API returns text/plain and
  # # not exactly a csv file
  # isCsv <- grepl("text/csv", httr::headers(response)$`content-type`)
  # if (isCsv) csvContent <- httr::content(response, as = "text", encoding = "UTF-8")
  # else {
  #   stop(
  #     "Server returned a successful status, but the content was not valid CSV data. It may be an HTML error page.",
  #     call. = FALSE
  #   )
  # }

  csvContent <- content(response, as = "text", encoding = "UTF-8")
  # --- Read csv ---
  if (nchar(csvContent) > 0) {
    df <- read.csv(text = csvContent, check.names = FALSE)
  } else {
    # Success, but no data. Create an empty frame.
    df <- data.frame()
    if (isTRUE(verbose)) message("Request successful, but no data returned for this query.")
  }

  # Clean data frame if downloaded
  if (!is.null(df) && nrow(df) > 0) {
    # Clean names and convert
    names(df) <- gsub("((?<=[_\\s])+.)", "\\U\\1",
                      tolower(names(df)), perl = TRUE)
    names(df) <- gsub("_|\\s", "", names(df))
    df$dateTime <- as.POSIXct(df[["dateTime"]], format = "%Y%m%d %H%M",
                              tz = "America/Los_Angeles")
  }

  return(df)
}

#' GET a URL with retries
#'
#' Wraps `httr::GET` in a retry loop. Retries on network failures and 5xx server
#' errors but fails immediately on 4xx client errors (resource does not exist).
#'
#' @param url The URL to retrieve.
#' @param maxAttempt The number of times to try the request. Defaults to 3.
#' @param verbose A logical indicating whether to print status messages.
#' @param ... Additional arguments passed directly to `httr::GET()`, such as
#'   `timeout`, `user_agent`, or `query`.
#'
#' @return On success, an `httr` response object. On failure after all retries,
#'   returns `NULL`. A error after encountering a 4xx client error.
#'
#' @importFrom httr GET http_status
#'
#' @keywords internal
#' @noRd
retryGet <- function(url, maxAttempt = 3, verbose = TRUE, ...) {

  for (attempt in 1:maxAttempt) {

    response <- tryCatch({
      GET(url = url, ...)
    }, error = function(e) {
      if (verbose) message(sprintf("Network error on attempt %d: %s", attempt, e$message))
      return(NULL)
    })

    # If the request was successful, return the response object immediately
    if (!is.null(response) && http_status(response)$category == "Success") {
      return(response)
    }

    # If it was a client error (e.g., 4xx Not Found), stop
    # These are not retriable.
    if (!is.null(response) && http_status(response)$category == "Client error") {
      stop(sprintf(
        "Request failed with a client error (%s). Check inputs. URL: %s",
        http_status(response)$reason, url
      ), call. = FALSE)
    }

    # If we are here, server error (5xx) or a network error.
    # Retry
    if (!is.null(response)) {
      if (verbose) message(sprintf("Server error on attempt %d: %s.",
                                   attempt,http_status(response)$reason))
    }

    # Wait before the next attempt
    if (attempt < maxAttempt) {
      if (verbose) message("Retrying in 2 seconds...")
      Sys.sleep(2)
    }
  }

  # If here, all retries have failed
  stop(sprintf("Failed to retrieve data from %s after %d attempts.", url, maxAttempt), call. = FALSE)
  # return(NULL)
}
