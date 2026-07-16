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
#' @param ... Additional arguments to be passed to \code{calcNearestCDEC()}
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
#' pullCDEC("MAL", 25, "hourly", "06/13/1986", "06/14/1986")
#' # If coordinates are used instead, must specify the argument names.
#' pullCDEC(coordinates = c(38.04281, -121.9201), sensor = 25,
#' duration = "hourly", dateStart = "06/13/1986", dateEnd = "06/14/1986")
#' # Can specify multiple coordinates, just like you can with multiple station names
#' pullCDEC(coordinates =
#' data.frame(c(38.04281, 38.04281),
#' c(-121.9201, -121.9203)),
#' sensor = 25, duration = "hourly", dateStart = Sys.Date() - 5)
#' }
pullCDEC <- function(station, sensor = NULL, duration = c("event", "hourly", "daily"),
                     dateStart, dateEnd = NULL, temperatureUnits = c("C", "F"),
                     coordinates, verbose = T, maxAttempt = 3, fallbackDuration = FALSE,
                     ...) {

  # --- Station or lat/lon ---
  if (!missing(coordinates) & !missing(station)) {
    warning("Both `station` and `coordinates` are provided. Ignoring `coordinates`.",
            call. = FALSE)
  }
  if (!missing(coordinates) & missing(station)) {
    if (length(coordinates) != 2)
      stop("`coordinates` should be a vector of two numbers, lat and lon.",
           call. = FALSE)
    if (is.null(sensor) | length(duration) > 1 | missing(dateStart))
      stop("Metadata lookup not supported for coordinates. Use `calcNearestCDEC()`",
           call. = FALSE)
    # Assuming calcNearestCDEC is defined elsewhere or will be provided
    cdecClosest <- calcNearestCDEC(data.frame(lat = coordinates[[1]],
                                              lon = coordinates[[2]]),
                                   sensor = sensor,
                                   verbose = verbose,
                                   ...)

    station <- unique(cdecClosest[["cdecGage"]])
  }

  # --- Metadata retrieval if missing ---
  # There's pullMetadataCDEC, but this is faster, albiet perhaps more brittle.
  if (is.null(sensor) | length(duration) > 1 | missing(dateStart)) {
    if (length(station) > 1)
      stop("Metadata lookup not supported for multiple stations.",
           call. = FALSE)
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
      availableData$cdecGage <- station
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
  # For filtering at the end to ensure user only get the data they asked for
  originalEnd <- if (is.null(dateEnd)) Sys.Date() else parseDate(dateEnd)
  dateEnd <- originalEnd + 1

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
      warning("No data available for station ", paste(station, collapse = ", "), " as specified.", call. = F)
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
  # Final filtering to provide the range that the user wants
  df[df$obsDate <= originalEnd, ]
}

#' Pulling CDEC gage metadata
#'
#' @param cdecGage Name of the CDEC gage, a singular value
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
pullMetadataCDEC <- function(cdecGage, maxAttempt = 3, timeout = 60, verbose = TRUE) {

  # --- Network Request Block with Retry Logic ---
  url <- paste0("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", cdecGage)
  # Retry logic now wrapped in internal function retryGet
  response <- retryGet(url, maxAttempt = maxAttempt, timeout(timeout))

  # --- HTML Parsing Block ---
  # If the code reaches here, 'response' is guaranteed to be a successful one.
  htmlContent <- content(response, as = "text", encoding = "UTF-8")
  page <- read_html(htmlContent)

  # Check for "Station Not Found" on the page content itself
  titleElement <- html_element(page, "h1")
  if (!is.na(titleElement) && grepl("Station Not Found", html_text(titleElement), ignore.case = TRUE)) {
    stop(sprintf("Gage '%s' not found on CDEC. The page exists but contains no station data.", cdecGage), call. = FALSE)
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
#' @param cdecGage Name of the gage of interest, as a character.
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
pullCoordinates <- function(cdecGage, maxAttempt = 3, timeout = 60) {

  response <- retryGet(paste0("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", cdecGage),
                              maxAttempt = maxAttempt, timeout = timeout)

  htmlContent <- content(response, as = "text", encoding = "UTF-8")
  page <- read_html(htmlContent)
  # After the loop, check if we ultimately failed.
  if (is.null(response) || http_status(response)$category != "Success") {
    stop(sprintf("Failed to retrieve data for gage '%s' after %d attempts.", cdecGage, maxAttempt), call. = FALSE)
  }

  dataString <- html_element(page, "table")
  dataString <- html_text(dataString)

  data.frame(station = regmatches(dataString, regexpr("(?<=Station ID)(.*)(?=Elevation)", dataString, perl = T)),
             latitude = regmatches(dataString, regexpr("(?<=Latitude)([\\d.-]+)", dataString, perl = T)),
             longitude = regmatches(dataString, regexpr("(?<=Longitude)([\\d.-]+)", dataString, perl = T)))
}

#' Download a Delta DEM for use in hydrological distance calculations
#'
#' @description
#' Downloads the CNRA 10 m Delta DEM from the default URL or a user-supplied URL,
#' extracts the raster, optionally saves it to a user-specified directory, and
#' returns a \code{SpatRaster}. When \code{asMask = TRUE}, the raw elevation
#' raster is binarized at the WSE threshold to produce the water mask expected by
#' the \code{distMethod = "hydrological"} path in \code{\link{calcNearestCDEC}}.
#'
#' @param url URL to the DEM file. Accepts a direct \code{.tif} URL or a
#'   \code{.zip} archive containing a \code{.tif} or \code{.img}. Defaults to
#'   the CNRA 10 m Bay Delta DEM (2025-03-12 release).
#' @param destDir Character. Directory to save raster file for persistent storage
#'   If \code{NULL} (default), the file lives only in the session's temporary
#'   directory and is not available after R restarts.
#' @param asMask Logical. If \code{TRUE}, returns a binary water mask
#'   (NA = land, 1 = water) by applying the WSE threshold to the raw elevation.
#'   If \code{FALSE} (default), returns the raw elevation raster.
#' @param wse Water surface elevation threshold in metres. Cells above this value
#'   are classified as land. Only used when \code{asMask = TRUE}. Defaults to
#'   1.25 m.
#' @param timeout Numeric. Override the download timeout in seconds. If
#'   \code{NULL} (default), timeout is calculated dynamically from the file size.
#'
#' @return A \code{SpatRaster}. When \code{asMask = TRUE} and \code{destDir} is
#'   supplied, the binary mask is also written to \code{destDir} as a compressed
#'   \code{.tif} alongside the raw DEM.
#'
#' @export
#'
#' @importFrom terra rast writeRaster
#'
#' @examples
#' \dontrun{
#' # Raw DEM, kept in tempdir for the session
#' dem <- downloadDEM()
#'
#' # Binary water mask saved to a project directory, ready for calcNearestCDEC
#' waterMask <- downloadDEM(destDir = "data/dem", asMask = TRUE, wse = 1.25)
#'
#' nearestStation <- calcNearestCDEC(myPoints, sensor = 25,
#'                                   distMethod = "hydrological",
#'                                   dem = waterMask)
#' }
downloadDEM <- function(
    url = NULL,
    destDir = NULL,
    asMask = FALSE,
    wse = 1.25,
    timeout = NULL) {

  if (is.null(url)) {
    url <- "https://data.cnra.ca.gov/dataset/f902e012-7d8d-429c-8a1a-2bf5b4312532/resource/d10040a8-4880-4f0e-90e7-86f57556bd9d/download/dem_delta_10m_20250312.zip"
  }

  # Download and extract via the generalized getFile
  rawPath <- getFile(
    file = url,
    open = FALSE,
    timeout = timeout,
    targetExtension  = c("tif", "img", "vrt")
  )

  # Persist to tempdir instead of destDir to isolate raw file
  tempPath <- file.path(tempdir(), basename(rawPath))
  if (!file.exists(tempPath)) {
    message("Copying raw raster to temporary directory: ", tempPath)
    file.copy(rawPath, tempPath)
  }
  rawPath <- tempPath

  message("Loading raster: ", basename(rawPath))
  dem <- terra::rast(rawPath)

  if (!asMask) return(dem)

  # Derive binary water mask from the raw elevation
  message(sprintf("Applying WSE threshold (%.2f m) to derive binary water mask.", wse))
  waterMask <- dem
  waterMask[waterMask > wse] <- NA
  waterMask[!is.na(waterMask)] <- 1

  # Only save the resulting mask raster to the user-specified directory
  if (!is.null(destDir)) {
    if (!dir.exists(destDir)) {
      dir.create(destDir, recursive = TRUE)
      message("Created directory: ", destDir)
    }

    rawName <- tools::file_path_sans_ext(basename(rawPath))
    maskPath <- file.path(destDir, paste0(rawName, "_waterMask.tif"))
    message("Writing water mask to destination: ", maskPath)

    terra::writeRaster(waterMask, maskPath,
                       datatype = "INT1U",
                       gdal     = c("COMPRESS=DEFLATE", "PREDICTOR=2"),
                       overwrite = TRUE)
  }

  waterMask
}

#' Find the n-th closest CDEC gage
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Identifies the n-th nearest CDEC gage to a lat/lon of interest. This function
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
#' @return A data frame of the metadata of the n-th closest CDEC station to your
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
                               cdecGPS = cdecStation,
                               cdecMetadata = cdecMetadata,
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


  cdecGPSFiltered <- cdecGPS[cdecGPS[["station"]] %in% closestGages[["cdecGage"]], ]
  if(n > nrow(cdecGPSFiltered)) {
    stop("n is larger than the number of available station.\n")
  }

  lapply(1:nrow(df), function(x) {

    distanceMatrix <- distm(data.frame(longitude = df[["lon"]][[x]],
                                                  latitude = df[["lat"]][[x]]),
                                       data.frame(longitude = cdecGPSFiltered[["longitude"]],
                                                  latitude = cdecGPSFiltered[["latitude"]]),
                                       fun = distVincentyEllipsoid)

    distanceData <- data.frame(cdecGage = cdecGPSFiltered[["station"]],
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
                      by.x = "cdecGage", by.y = "cdecGage", all.x = T)

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
#' This df should include three columns: `cdecGage` (the CDEC station name),
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
#' @importFrom utils modifyList
#' @importFrom dplyr bind_rows
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
  # names(df) <- tolower(names(df))

  if (!all(requiredCols %in% names(df))) {
    stop(sprintf("Missing required columns: %s",
                 paste(setdiff(requiredCols, names(df)), collapse = ", ")),
         call. = FALSE)
  }

  if (nrow(df) == 0) return(data.frame())

  variable <- match.arg(variable)
  # if (!variable %in% names(df)) {
  #   stop(sprintf("Variable '%s' not found in dataset", variable),
  #        call. = FALSE)
  # }

  waterColumn <- match.arg(waterColumn)

  # The "time" column must be a time format in the form of %Y-%m-%d %h:%m:%s,
  # This is the same formatting as the CDEC data
  if (is.character(df$time)) df$time <- as.POSIXct(df$time,
                                                   tz = "America/Los_Angeles")
  if (all(is.na(df$time))) stop("Time column could not be parsed. Ensure the time is in `%Y-%m-%d %H:%M:%S` format", call. = F)

  # --- Get cdec data ---
  defaults <- list(
    method = "fast",
    verbose = TRUE
  )
  # Merge in user arguments if they're provided instead of the default
  userArguments <- list(...)
  finalArguments <- modifyList(defaults, userArguments)

  if (finalArguments$method == "accurate" && !identical(finalArguments$verbose, FALSE))
    message("Step 1/2: Finding nearest CDEC stations with required data...")

  if (is.null(cdecClosest)) {
    cdecClosest <- calcNearestCDEC(df,
                                   variable = variable,
                                   waterColumn = waterColumn,
                                   ...)
  }

  # --- PARAMETER SANITIZATION ---
  # Separate spatial/routing arguments from downstream downloader arguments.
  cdecParams <- c("distMethod", "waterRaster", "snapDist", "maxEuclideanDist",
                  "hydroCandidates", "gridDistMaxIter", "hydroOrientation")
  batchArgs <- userArguments[!names(userArguments) %in% cdecParams]

  # --- Pull the data, batch download ---
  dfSplitDuration <- split(cdecClosest, list(as.character(cdecClosest$duration),
                                             cdecClosest$sensorNumber),
                           drop = T)
  if (finalArguments$method == "accurate" && !identical(finalArguments$verbose, FALSE))
    message("Step 2/2: Downloading and finding nearest CDEC value...")

  pulledData <- bind_rows(lapply(dfSplitDuration, function(durationSensorGroup) {
    if (nrow(durationSensorGroup) == 0) return(NULL)

    dateRange <- range(as.Date(durationSensorGroup$time))

    stations <- unique(durationSensorGroup$cdecGage)
    durations <- as.character(unique(durationSensorGroup$duration))
    dateStarting <- dateRange[1] - 1
    dateEnding <- dateRange[2]

    estimatedRows <- estimateCdecRows(length(stations), durations, dateStarting, dateEnding + 1)
    if (estimatedRows < 2000000) {
      cdecData <- pullCDEC(
        station = unique(durationSensorGroup$cdecGage),
        sensor = unique(durationSensorGroup$sensorNumber),
        duration = as.character(unique(durationSensorGroup$duration)),
        dateStart = dateRange[1] - 1, # Add a 1-day buffers
        dateEnd = dateRange[2] # + 1 day buff now taken care of within pullCDEC
      )
    } else {
      batchCallArgs <- c(
        list(
          station = unique(durationSensorGroup$cdecGage),
          sensor = unique(durationSensorGroup$sensorNumber),
          duration = as.character(unique(durationSensorGroup$duration)),
          dateStart = dateRange[1] - 1,
          dateEnd = dateRange[2]
        ),
        batchArgs
      )
      cdecData <- do.call(batchCDEC, batchCallArgs)

    }

    if (is.null(cdecData) || nrow(cdecData) == 0) return(NULL)

    # Split the location of interset data into groups of its nearest cdec station
    cdecDataPerStation <- split(durationSensorGroup, durationSensorGroup$cdecGage)

    # To each sampling point, find closest time point from its closest cdec station
    results <- lapply(cdecDataPerStation, function(cdecDf) {

      cdecGage <- cdecDf$cdecGage[1]
      cdecStationDf <- cdecData[cdecData$stationId == cdecGage, ]

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

    bind_rows(results)
  }))
  # --- Clean up ---
  rownames(pulledData) <- NULL
  finalNameOrder <- append(setdiff(names(pulledData), "cdecGage"), "cdecGage",
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
#' @param cacheDir If specified, will save chunks to directory, checking to see
#' if the data already exists first.
#'
#' @return A data frame of the request data pull.
#' @export
#'
#' @importFrom dplyr distinct bind_rows
#' @importFrom rlang .data
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
                      rowLimit = 2000000, cacheDir = NULL, ...) {

  # --- Validation and preprocessing ---
  dateStart <- parseDate(dateStart)
  dateEnd <- if (is.null(dateEnd)) Sys.Date() else parseDate(dateEnd)
  station <- unique(station)

  if (is.na(dateStart) || is.na(dateEnd) || dateStart > dateEnd || is.infinite(dateStart) || is.infinite(dateEnd)) {
    stop("Invalid `dateStart` or `dateEnd` provided.", call. = FALSE)
  }

  # # --- Coordinate Resolution (Brings batchCDEC in parity with pullCDEC?) ---
  # if (!missing(coordinates) & missing(station)) {
  #   if (length(coordinates) != 2)
  #     stop("`coordinates` should be a vector of two numbers, lat and lon.",
  #          call. = FALSE)
  #
  #   cdecClosest <- calcNearestCDEC(
  #     df = data.frame(lat = coordinates[[1]], lon = coordinates[[2]]),
  #     sensor = sensor,
  #     ...
  #   )
  #   station <- unique(cdecClosest[["cdecGage"]])
  # }
  #
  # if (missing(station)) {
  #   stop("Must provide either `station` or `coordinates` to pull CDEC data.", call. = FALSE)
  # }
  #
  # station <- unique(station)

  # --- Cache directory setup ---
  useDiskCache <- !is.null(cacheDir)
  if (useDiskCache) {
    if (!dir.exists(cacheDir)) {
      dir.create(cacheDir, recursive = TRUE)
      message(sprintf("Created cache directory: %s", cacheDir))
    }
    stationsSorted <- sort(station)
    stationFingerprint <- sprintf("n%d_%s-%s",
                                  length(stationsSorted),
                                  stationsSorted[1],
                                  stationsSorted[length(stationsSorted)])
    chunkFile <- function(chunkStart, chunkEnd) {
      file.path(cacheDir,
                sprintf("cdec_s%s_%s_%s_%s_%s.rds",
                        sensor, duration, stationFingerprint,
                        chunkStart, chunkEnd))
    }
  }

  # --- Dynamic pagination logic ---
  # Estimate the total size of the request
  totalEstimatedRows <- estimateCdecRows(
    numberstation = length(station),
    duration = duration,
    startDate = dateStart,
    endDate = dateEnd + 1
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
    numChunks <- 1
  }

  # --- Pagination Loop ---
  allDataChunks <- list()
  failedChunks <- list()
  currentDateStart <- dateStart
  chunk <- 1

  while (currentDateStart <= dateEnd) {
    # -1 needed as pullCDEC defaults to adding 1 to ending date
    currentDateEnd <- min(currentDateStart + chunkSizeInDays - 1, dateEnd)

    cat(sprintf("--- Fetching data from %s to %s, Chunk %s/%s ---\n",
                currentDateStart, currentDateEnd + 1, chunk, numChunks))

    # --- Check cache before downloading ---
    if (useDiskCache) {
      cachedFile <- chunkFile(currentDateStart, currentDateEnd)
      if (file.exists(cachedFile)) {
        message(sprintf("Chunk %d: loading from cache (%s).",
                        chunk, basename(cachedFile)))
        chunkDf <- tryCatch(
          readRDS(cachedFile),
          error = function(e) {
            warning(sprintf(
              "Chunk %d: cache file is unreadable and will be re-downloaded.\n  Reason: %s",
              chunk, conditionMessage(e)
            ), call. = FALSE)
            NULL
          }
        )
        if (!is.null(chunkDf)) {
          allDataChunks[[length(allDataChunks) + 1]] <- chunkDf
          currentDateStart <- currentDateEnd + 1
          chunk <- chunk + 1
          next
        }
        # Falls through to a fresh download if the cached file was corrupt
      }
    }

    # --- Download chunk ---
    chunkDf <- tryCatch(
      pullCDEC(
        station = station,
        sensor = sensor,
        duration = duration,
        dateStart = currentDateStart,
        dateEnd = currentDateEnd,
        ...
      ),
      error = function(e) {
        warning(sprintf(
          "Chunk %d/%d (%s to %s) failed and will be skipped.\n  Reason: %s",
          chunk, numChunks, currentDateStart, currentDateEnd,
          conditionMessage(e)
        ), call. = FALSE)
        NULL
      }
    )

    if (!is.null(chunkDf) && nrow(chunkDf) > 0) {
      if (useDiskCache) {
        saveRDS(chunkDf, file = cachedFile)
        message(sprintf("Chunk %d: saved to cache (%s).", chunk, basename(cachedFile)))
      }
      allDataChunks[[length(allDataChunks) + 1]] <- chunkDf
    } else if (is.null(chunkDf)) {
      failedChunks[[length(failedChunks) + 1]] <- list(
        chunk = chunk,
        dateStart = currentDateStart,
        dateEnd = currentDateEnd
      )
    }

    # The + 1 here replicates the pullCDEC function defaulting to adding a day for its pull
    currentDateStart <- currentDateEnd + 1
    chunk <- chunk + 1
  }

  # --- Final combination and cleaning ---
  if (length(allDataChunks) == 0L) {
    stop("All chunks failed to download. No data to return.", call. = FALSE)
  }

  if (length(failedChunks) > 0L) {
    failedRanges <- vapply(failedChunks, function(x) {
      sprintf("  Chunk %d: %s to %s", x$chunk, x$dateStart, x$dateEnd)
    }, character(1))
    warning(sprintf(
      "%d of %d chunk(s) failed. The returned data covers only successful ranges.\n%s",
      length(failedChunks), numChunks, paste(failedRanges, collapse = "\n")
    ), call. = FALSE)
  }

  cat("--- All phases complete. Combining data. ---\n")
  finalDf <- bind_rows(allDataChunks)

  if (nrow(finalDf) > 0) {
    # Efficiently remove duplicates that may occur at chunk boundaries
    finalDf <- distinct(finalDf, .data$stationId, .data$sensorNumber, .data$duration, .data$dateTime,
                        .keep_all = T)
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
    df$sensorNumber <- suppressWarnings(as.integer(df$sensorNumber))
    df$value <- suppressWarnings(as.double(
      ifelse(trimws(df$value) %in% c("---", ""), NA, df$value)
    ))
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

#' Estimate row position of relevant data
#'
#' @param tableData data.frame
#' @param headerRow Row number of the header, if the header is within the data frame
#' @param maxSearchRows Max number of rows to search through. Defaults to 15 under
#' the assumption that the row with relevant data is at the top of the data frame.
#'
#' @returns An integer indicating start of the data row
#' @noRd
#'
#' @keywords internal
findDataStart <- function(tableData, headerRow = NULL, maxSearchRows = 15) {

  totalRows <- nrow(tableData)
  searchStart <- if (is.null(headerRow)) 1 else headerRow + 1
  searchEnd <- min(totalRows, searchStart + maxSearchRows - 1)

  if (searchStart > totalRows) {
    return(searchStart)
  }

  densityScores <- sapply(searchStart:searchEnd, function(i) {
    row <- as.character(tableData[i, ])
    nonNaRatio <- mean(!is.na(row) & row != "" & trimws(row) != "", na.rm = TRUE)
    contentVariety <- length(unique(nchar(row[!is.na(row)])))

    nonNaRatio * log(contentVariety + 1)
  })

  searchStart + which.max(densityScores) - 1
}

#' Scrape sensor list from CDEC webpage
#'
#' @param url Webpage to SENSLIST
#' @param naStrings Vector of potential NAs used in the table
#' @param naThreshold Threshold beyond which a column will be removed from the output
#' @param maxSearchRows Max number of rows to search through to find: the header row and
#' data start row
#'
#' @returns Data frame
#' @export
#'
#' @examples
#' \dontrun{
#' readCdecSensorList()
#' }
readCdecSensorList <- function(url = "https://cdec.water.ca.gov/reportapp/javareports?name=SensList",
                               naStrings = c("N/A", "XX", "xxx"),
                               naThreshold = 0.9,
                               maxSearchRows = 15) {

  if (!is.character(url) || length(url) != 1) {
    stop("url must be a single character string")
  }

  if (!is.character(naStrings)) {
    stop("naStrings must be a character vector")
  }

  if (!is.numeric(naThreshold) || length(naThreshold) != 1 ||
      naThreshold < 0 || naThreshold > 1) {
    stop("naThreshold must be a number between 0 and 1")
  }

  tryCatch({
    htmlContent <- rvest::read_html(url)
    tableElement <- rvest::html_element(htmlContent, "#SENSLIST")

    if (is.na(tableElement)) {
      stop("Could not find table element with ID 'SENSLIST' on the webpage")
    }

    rawTable <- rvest::html_table(tableElement,
                                  header = FALSE,
                                  na.strings = naStrings)

    # Convert any cell that becomes empty after removing ALL whitespace to NA
    processedTable <- as.data.frame(lapply(rawTable, function(col) {
      col <- as.character(col)
      cleanedCol <- gsub("\\s+", "", col)
      col[!is.na(col) & cleanedCol == ""] <- NA
      col
    }))

    if (nrow(processedTable) < 3) {
      stop("Table does not have enough rows for processing")
    }

    headerRow <- findDataStart(processedTable)
    dataStartRow <- findDataStart(processedTable, headerRow, maxSearchRows)

    dataRows <- processedTable[dataStartRow:nrow(processedTable), , drop = FALSE]
    naProportion <- colSums(is.na(dataRows)) / nrow(dataRows)

    validCols <- which(naProportion < naThreshold)

    if (length(validCols) == 0) {
      stop("No columns contain sufficient data")
    }

    selectedTable <- processedTable[, validCols, drop = FALSE]
    colnames(selectedTable) <- as.character(selectedTable[headerRow, ])
    cleanedTable <- selectedTable[dataStartRow:nrow(selectedTable), , drop = FALSE]

    rownames(cleanedTable) <- NULL

    return(cleanedTable)

  }, error = function(e) {
    stop("Failed to read or process data from URL: ", e$message)
  })
}

#' Build a binary water mask raster from a waterway shapefile
#'
#' Rasterizes a waterway polygon or line layer into the binary
#' \code{NA} = land / \code{1} = water format expected by
#' \code{calcNearestCDEC(distMethod = "hydrological")}, and validates that
#' the result is in a projected CRS.
#'
#' @param waterway A waterway polygon or line layer, supplied as a file path
#'   (readable by \code{sf::st_read}), an \code{sf}/\code{sfc} object, or a
#'   \code{terra::SpatVector}.
#' @param resolution Numeric. Grid cell size in the map units of the target
#'   CRS (metres, for a typical UTM projection).
#' @param crs Optional target CRS for the output raster (anything accepted
#'   by \code{sf::st_crs()}, e.g. \code{"EPSG:26910"} or a numeric EPSG
#'   code). If \code{NULL} (default), the CRS of \code{waterway} is used
#'   as-is, and must already be projected.
#' @param verbose Logical. Print progress and diagnostic messages. Defaults
#'   to \code{TRUE}.
#'
#' @details
#' The mask returned always follows the strict binary contract expected by
#' \code{calcNearestCDEC()}: \code{NA} for land, \code{1} for water,
#' regardless of how \code{waterway} is structured.
#'
#' Resolution is deliberately not defaulted. Too coarse a grid can erase
#' narrow channels or connections between waterways, producing spurious
#' "unreachable" stations or points several calls downstream in
#' \code{calcNearestCDEC()}; too fine a grid makes \code{gridDist()}
#' disproportionately expensive. Choose the coarsest resolution that still
#' preserves every channel width relevant to your points of interest.
#'
#' @return A binary \code{SpatRaster} (\code{NA} = land, \code{1} = water)
#'   in the target projected CRS, suitable for use as \code{waterRaster} in
#'   \code{calcNearestCDEC()}.
#'
#' @importFrom sf st_read st_as_sf st_transform st_crs st_is_longlat st_bbox
#' @importFrom terra vect rast rasterize ext
#' @export
buildWaterMask <- function(waterway, resolution, crs = NULL, verbose = TRUE) {

  if (missing(resolution) || !is.numeric(resolution) || length(resolution) != 1 ||
      resolution <= 0) {
    stop("`resolution` must be a single positive number, in the map units ",
         "of the target CRS.", call. = FALSE)
  }

  # --- Load waterway into sf ---
  waterwaySf <- if (is.character(waterway)) {
    if (!file.exists(waterway)) {
      stop(sprintf("`waterway` file not found: %s", waterway), call. = FALSE)
    }
    sf::st_read(waterway, quiet = !verbose)
  } else if (inherits(waterway, c("sf", "sfc"))) {
    sf::st_as_sf(waterway)
  } else if (inherits(waterway, "SpatVector")) {
    sf::st_as_sf(waterway)
  } else {
    stop("`waterway` must be a file path, an sf/sfc object, or a ",
         "terra::SpatVector.", call. = FALSE)
  }

  waterwayCrs <- sf::st_crs(waterwaySf)
  if (is.na(waterwayCrs)) {
    stop("`waterway` has no CRS assigned. Assign one with ",
         "`sf::st_set_crs()` before calling `buildWaterMask()`, or supply ",
         "`crs` explicitly if the geometry is already in the correct ",
         "projection.", call. = FALSE)
  }

  # --- Resolve and validate target CRS ---
  targetCrs <- if (!is.null(crs)) sf::st_crs(crs) else waterwayCrs

  if (is.na(targetCrs)) {
    stop("`crs` could not be resolved to a valid CRS.", call. = FALSE)
  }

  if (sf::st_is_longlat(targetCrs)) {
    stop(
      "The target CRS is unprojected (is longitude/latitude). Supply a projected `crs`.",
      call. = FALSE
    )
  }

  if (!isTRUE(waterwayCrs == targetCrs)) {
    if (verbose) message("Reprojecting waterway to target CRS...")
    waterwaySf <- sf::st_transform(waterwaySf, targetCrs)
  }

  # --- Combined extent ---
  waterwayBox <- sf::st_bbox(waterwaySf)
  templateExt <- terra::ext(waterwayBox)

  # --- Build template raster and rasterize ---
  template <- terra::rast(
    resolution = resolution,
    extent = templateExt,
    crs = targetCrs$wkt
  )

  waterwayVect <- terra::vect(waterwaySf)
  waterMask <- terra::rasterize(waterwayVect, template, field = 1,
                                background = NA_integer_)

  # Enforce the binary contract explicitly (rather than trusting
  # rasterize()'s field/background arguments alone) so a future change to
  # rasterize()'s defaults can't silently produce a non-binary mask.
  waterMask[!is.na(waterMask)] <- 1
  waterMask
}

#' Extracts raster cell of a point, snapping to water if needed.
#'
#' @param pt Point of interest, sampling point
#' @param costRast A binary cost raster
#' @param snapDist Buffer radius in map units for land-cell fallback.
#'
#' @noRd
#' @keywords internal
snapCellToWater <- function(pt, costRast, snapDist) {

  valCol <- names(costRast)[1]

  # Direct hit
  extracted <- terra::extract(costRast, pt, cells = TRUE, ID = FALSE)
  if (nrow(extracted) > 0 && !is.na(extracted[[valCol]][1])) {
    return(list(cell = extracted$cell[1], snapped = FALSE))
  }

  # Buffer search: terra returns cells in scan order, not proximity order,
  # so explicit distances are required to identify the true nearest water cell
  ptBuffer <- terra::buffer(pt, width = snapDist)
  buffered <- terra::extract(costRast, ptBuffer, cells = TRUE, xy = TRUE, ID = FALSE)
  buffered <- buffered[!is.na(buffered[[valCol]]), , drop = FALSE]

  if (nrow(buffered) > 0) {
    ptCoords <- terra::crds(pt)
    dists <- sqrt((buffered$x - ptCoords[1, 1])^2 +
                       (buffered$y - ptCoords[1, 2])^2)
    bestIdx <- which.min(dists)
    return(list(cell = buffered$cell[bestIdx], snapped = TRUE))
  }

  list(cell = NA_integer_, snapped = FALSE)
}

#' Extract a hydrological distance at a point, snapping to water if needed
#'
#' @param distRast SpatRaster: distance surface produced by gridDist.
#' @param pt SpatVector: a single point.
#' @param snapDist Buffer radius in map units for land-cell fallback.
#'
#' @return A list: `dist` (numeric or NA) and `snapped` (logical).
#' @noRd
#' @keywords internal
extractWithSnap <- function(distRast, pt, snapDist) {

  valCol <- names(distRast)[1]

  # Direct extraction
  val <- terra::extract(distRast, pt, ID = FALSE)[1, 1]
  if (!is.na(val)) return(list(dist = val, snapped = FALSE))

  # Buffer extraction: same scan-order issue as snapCellToWater;
  # use explicit distances to pick the nearest reachable cell
  ptBuffer <- terra::buffer(pt, width = snapDist)
  buffered <- terra::extract(distRast, ptBuffer, cells = TRUE, xy = TRUE, ID = FALSE)
  buffered <- buffered[!is.na(buffered[[valCol]]), , drop = FALSE]

  if (nrow(buffered) > 0) {
    ptCoords <- terra::crds(pt)
    dists <- sqrt((buffered$x - ptCoords[1, 1])^2 +
                       (buffered$y - ptCoords[1, 2])^2)
    bestIdx <- which.min(dists)
    return(list(dist = buffered[[valCol]][bestIdx], snapped = TRUE))
  }

  list(dist = NA_real_, snapped = FALSE)
}

#' Compute hydrological distances from one source to many targets
#'
#' Internal helper shared by every orientation of the hydrological search in
#' \code{calcNearestCDEC()} (station-centric or point-centric, ranked or
#' \code{n = "all"}): crops the cost surface to a local extent around the
#' source and its targets, snaps the source onto water, runs a single
#' \code{terra::gridDist()} call from it, and batch-extracts distances at
#' every target, falling back to a snap search for any target that lands
#' on a land cell.
#'
#' @param sourceVect SpatVector, single point/station: the gridDist() origin.
#' @param targetVect SpatVector: locations to extract distances at.
#' @param targetEuclidMeter Numeric vector, same length/order as
#'   \code{targetVect}: straight-line distance (meters) from source to each
#'   target, used only to size the crop buffer.
#' @param costSurf Full SpatRaster cost surface.
#' @param snapDist,gridDistMaxIter,verbose See \code{calcNearestCDEC()}.
#' @param sourceDesc Character, used as a prefix in progress/warning
#'   messages, e.g. \code{"Station 5/62: SBS"} or \code{"Point 1/1"}.
#'
#' @return Numeric vector, same length as \code{targetVect}: distance in
#'   meters, or \code{NA} where the source or that specific target could not
#'   be reached (matching the Inf/NA contract documented in
#'   \code{calcNearestCDEC()}: this function only ever returns \code{NA},
#'   never \code{Inf} as every value here reflects an evaluation that was
#'   actually attempted).
#' @noRd
#' @keywords internal
hydroDistFromSource <- function(sourceVect, targetVect, targetEuclidMeter,
                                costSurf, snapDist, gridDistMaxIter, verbose,
                                sourceDesc) {

  result <- rep(NA_real_, length(targetVect))

  allCoords <- rbind(terra::crds(sourceVect), terra::crds(targetVect))
  cropBuffer <- max(2000, max(targetEuclidMeter) * 0.5)
  cropExt <- terra::ext(
    min(allCoords[, 1]), max(allCoords[, 1]),
    min(allCoords[, 2]), max(allCoords[, 2])
  ) + cropBuffer

  localCost <- tryCatch(
    terra::crop(costSurf, cropExt),
    error = function(e) {
      warning(sprintf("%s -- could not crop raster (%s).",
                      sourceDesc, conditionMessage(e)), call. = FALSE)
      NULL
    }
  )
  if (is.null(localCost)) return(result)

  sourceSnap <- snapCellToWater(sourceVect, localCost, snapDist)
  if (is.na(sourceSnap$cell)) {
    warning(sprintf("%s -- could not be placed on water within %d m.",
                    sourceDesc, snapDist), call. = FALSE)
    return(result)
  }
  if (sourceSnap$snapped && verbose)
    message(sprintf("    %s snapped to nearest water cell.", sourceDesc))

  localCost[sourceSnap$cell] <- 2
  distRast <- tryCatch(
    withCallingHandlers(
      terra::gridDist(localCost, target = 2, maxiter = gridDistMaxIter),
      warning = function(w) {
        # Re-tag terra's generic "did not converge" warning with the
        # specific source it applies to, so it's traceable
        if (grepl("did not converge", conditionMessage(w), fixed = TRUE)) {
          warning(sprintf(
            paste0(
              "%s -- gridDist() did not converge within maxiter = %d. ",
              "Some distances from this source may be inaccurate or ",
              "missing. Consider increasing `gridDistMaxIter`."
            ),
            sourceDesc, gridDistMaxIter
          ), call. = FALSE)
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(e) {
      warning(sprintf("%s -- gridDist failed (%s).",
                      sourceDesc, conditionMessage(e)), call. = FALSE)
      NULL
    }
  )
  if (is.null(distRast)) return(result)

  batchVals <- terra::extract(distRast, targetVect, ID = FALSE)[, 1]

  for (idx in seq_along(targetVect)) {
    val <- batchVals[idx]
    if (!is.na(val)) {
      result[idx] <- val
    } else {
      # Direct extraction landed on a land cell; fall back to a snap search
      # around this specific target
      snapped <- extractWithSnap(distRast, targetVect[idx, ], snapDist)
      result[idx] <- snapped$dist
    }
  }

  result
}

#' Find the Nth nearest CDEC station with specific data
#'
#' For each input point, finds the Nth-nearest CDEC (California Data
#' Exchange Center) monitoring station reporting a given sensor/variable,
#' using either straight-line (Euclidean) distance or distance measured
#' through the connected water network (hydrological).
#'
#' @param df A data frame of query points with \code{lat} and
#'   \code{lon} columns, in decimal degrees (WGS84).
#' @param n Integer, or the string \code{"all"}. Which rank of nearest
#'   station to return for each point (\code{n = 1} is closest,
#'   \code{n = 2} second-closest, etc.). If \code{"all"}, every candidate
#'   station is evaluated and returned, ranked, in a nested list-column.
#' @param sensor Optional CDEC sensor number. If supplied, overrides
#'   \code{variable}/\code{waterColumn} filtering and restricts candidate
#'   stations to those reporting this exact sensor.
#' @param variable One of \code{"temp"}, \code{"turbidity"}, or
#'   \code{"ec"} (electrical conductivity). Ignored if \code{sensor} is
#'   supplied. Defaults to \code{"temp"} with a message if left
#'   unspecified.
#' @param waterColumn One of \code{"top"} or \code{"bottom"}, selecting
#'   surface vs. bottom-of-water-column sensors. Ignored if \code{sensor}
#'   is supplied. Defaults to \code{"top"} with a message if left
#'   unspecified.
#' @param method Distance algorithm for the Euclidean calculation:
#'   \code{"fast"} (Haversine, default) or \code{"accurate"} (Vincenty
#'   ellipsoid, slower but more precise). Also determines the lower-bound
#'   distances used for candidate selection and pruning when
#'   \code{distMethod = "hydrological"}.
#' @param distMethod Distance method: \code{"euclidean"} (default) or
#'   \code{"hydrological"}, which routes through connected water cells.
#' @param waterRaster A binary \code{SpatRaster} where \code{NA} = land and
#'   any non-\code{NA} value = navigable water, in a projected CRS. Can be
#'   built with \code{buildWaterMask()}. If \code{NULL}, the bundled Delta
#'   water mask is used. The bundled mask is developed from DWR's 10x10
#'   Lidar DEM of the Bay Delta, published March 2025, using wse = 1.25 m
#'   to specify the land/water boundary. Only relevant when
#'   \code{distMethod = "hydrological"}.
#' @param snapDist Maximum search radius in metres when a point or station
#'   falls on a land cell. Defaults to 200 m. Should generally be at least
#'   1.5-2x \code{waterRaster}'s resolution
#' @param maxEuclideanDist Numeric, in miles. Global pre-filter: point-
#'   cdec station pairs farther apart than this (by straight-line distance) are
#'   excluded before any candidate selection. Defaults to \code{Inf} (no
#'   filtering).
#' @param hydroCandidates Number of Euclidean-nearest stations per input
#'   point to evaluate hydrologically. Automatically expanded to
#'   \code{n + 2} if \code{n >= hydroCandidates}. Ignored when
#'   \code{n = "all"}.
#' @param gridDistMaxIter Passed to \code{terra::gridDist()}'s
#'   \code{maxiter} argument. If a station's cost-distance surface fails
#'   to converge within this many iterations (more likely for large or
#'   geometrically complex cropped extents), some distances for that
#'   station may come back \code{NA} even where a valid, longer water path
#'   exists or a value is returned that is incorrect. A per-station warning
#'   identifies when this happens; increase this value if you see it often.
#'   Defaults to \code{50}, matching \code{terra::gridDist()}'s own default.
#' @param hydroOrientation Where to focus distance calculation from, either from
#' the point of interest or from the cdec station. Defaults to \code{auto} (choose
#' whichever has lower number of points) but can be overriden to use \code{point}
#' or \code{station}. Override exists because \code{auto} uses a simple
#' heuristic that cannot guarantee an optimal choice.
#' @param verbose Logical. Print progress and diagnostic messages.
#' @param cdecGPS Optional override for the internal CDEC station GPS
#'   lookup table (defaults to \code{deltadata::cdecStation}).
#' @param cdecMetadata Optional override for the internal CDEC sensor
#'   metadata lookup table (defaults to \code{deltadata::cdecMetadata}).
#'
#' @details
#' # Hydrological candidate approximation
#' When \code{distMethod = "hydrological"} and \code{n != "all"}, only the
#' \code{hydroCandidates} Euclidean-nearest stations to each point are ever
#' evaluated hydrologically (expanded to \code{n + 2} if needed). This
#' keeps the search tractable, but it means the station returned is only
#' guaranteed to be the true nth-nearest station \emph{among that
#' Euclidean-nearest candidate set}, not necessarily the true global
#' nth-nearest station by water. If you need an exhaustive, exact
#' answer, use \code{n = "all"}, which evaluates every reachable station
#' and is unaffected by this approximation, at a proportionally higher
#' runtime cost.
#'
#' # Local raster cropping
#' For performance, every station's cost-distance surface
#' (\code{terra::gridDist()}) is computed on a raster cropped to a padded
#' bounding box around the station and its currently nominated points
#' (all input points, when \code{n = "all"}), rather than on the full
#' \code{waterRaster}. The padding is adaptive
#' (\code{max(2000, 0.5 * euclidean distance)}) but not exhaustive: if the
#' true water path between a station and a point requires a long detour
#' beyond this padded extent (e.g. around a peninsula or island), that
#' path will not be found, and the pair will be reported as unreachable
#' even though a valid, longer route exists. This trade-off is deliberate
#' for performance and is not currently configurable.
#'
#' # Geographic scope
#' Candidate selection and branch-and-bound pruning both rely on the fact
#' that Euclidean (straight-line, geodesic) distance is always a valid
#' lower bound on hydrological distance.
#'
#' @return
#' If \code{n} is a single integer: \code{df} with \code{cdecGage},
#' \code{distance} (miles), and merged station-metadata columns appended;
#' one row per input point.
#'
#' If \code{n = "all"}: \code{df} with a nested list-column \code{cdecGage},
#' one data frame per input point ranking every evaluated station. Use
#' \code{tidyr::unnest(result, cdecGage)} to expand.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Build a hydrological cost surface from the Delta waterway layer once,
#' # then reuse it across calls.
#' deltaWaterMask <- buildWaterMask(
#'   waterway = deltamapr::WW_Watershed,
#'   resolution = 30
#' )
#'
#' # Find the nearest station reporting sensor 20 (water temperature) to
#' # each unique SLS station location, using hydrological distance.
#' nearestStations <- LTMRdata::SLS %>%
#'   distinct(Station, Latitude, Longitude) %>%
#'   rename(lat = Latitude, lon = Longitude) %>%
#'   calcNearestCDEC(
#'     sensor = 20,
#'     distMethod = "hydrological",
#'     waterRaster = deltaWaterMask,
#'     snapDist = 50
#'   )
#' }
#'
#' @importFrom geosphere distm distVincentyEllipsoid distHaversine
#' @importFrom terra rast crop gridDist extract ext vect crs minmax crds
#'   is.lonlat res buffer
#' @importFrom sf st_as_sf st_transform
#' @export
calcNearestCDEC <- function(df, n = 1,
                            sensor = NULL,
                            variable = c("temp", "turbidity", "ec"),
                            waterColumn = c("top", "bottom"),
                            method = c("fast", "accurate"),
                            distMethod = c("euclidean", "hydrological"),
                            waterRaster = NULL,
                            snapDist = 200,
                            maxEuclideanDist = Inf,
                            hydroCandidates = 5,
                            gridDistMaxIter = 50,
                            hydroOrientation = c("auto", "point", "station"),
                            verbose = TRUE,
                            cdecGPS = NULL,
                            cdecMetadata = NULL) {

  cdecGPS <- if (is.null(cdecGPS)) get("cdecStation", envir = asNamespace("deltadata"))
  cdecMetadata <- if (is.null(cdecMetadata)) get("cdecMetadata", envir = asNamespace("deltadata"))

  # --- Validation ---
  if (!all(c("lat", "lon") %in% names(df))) {
    stop("Input `df` must contain 'lat' and 'lon' columns.", call. = FALSE)
  }

  if (!is.numeric(gridDistMaxIter) || length(gridDistMaxIter) != 1 ||
      gridDistMaxIter <= 0) {
    stop("`gridDistMaxIter` must be a single positive number.", call. = FALSE)
  }

  method <- match.arg(method)
  distMethod <- match.arg(distMethod)
  hydroOrientation <- match.arg(hydroOrientation)

  # Defined once, up front, since it's needed by both the hydrological
  # block and the final ranking block below (regardless of distMethod).
  isNAll <- is.character(n) && n == "all"

  # Initialize an empty vector to store removed stations
  removedStations <- character(0)

  # --- Sensor / variable filtering ---
  if (!is.null(sensor)) {
    availableSensors <- cdecMetadata[cdecMetadata$sensorNumber == sensor, ]
    if (nrow(availableSensors) == 0)
      stop(sprintf("Sensor '%s' was not found in the CDEC metadata.", sensor), call. = FALSE)
  } else {
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
      temp = "(temp).*(water)",
      turbidity = "turbidity",
      ec = "elec.* conduct.* micro"
    )
    bottomPattern <- "(lower|bottom)"

    availableSensors <- cdecMetadata
    availableSensors <- availableSensors[
      grepl(variableRegexMap[[variable]], availableSensors$sensorDescription,
            ignore.case = TRUE), ]

    if (waterColumn == "top") {
      availableSensors <- availableSensors[
        !grepl(bottomPattern, availableSensors$sensorDescription, ignore.case = TRUE), ]
    } else {
      availableSensors <- availableSensors[
        grepl(bottomPattern, availableSensors$sensorDescription, ignore.case = TRUE), ]
    }

    if (nrow(availableSensors) == 0)
      stop("No CDEC stations found with the specified sensor criteria.", call. = FALSE)
  }

  availableSensors$duration <- gsub("\\(|\\)", "", availableSensors[["duration"]])
  cdecGpsFiltered <- cdecGPS[cdecGPS$station %in% unique(availableSensors$gage), ]

  # --- Euclidean distance matrix ---
  distanceFunction <- if (method == "fast") distHaversine else distVincentyEllipsoid

  distanceMatrixMeter <- distm(
    df[, c("lon", "lat"), drop = FALSE],
    cdecGpsFiltered[, c("longitude", "latitude"), drop = FALSE],
    fun = distanceFunction
  )
  distanceMatrix <- distanceMatrixMeter / 1609.344

  # Global Euclidean threshold filter
  distanceMatrixMeterRanking <- distanceMatrixMeter
  if (is.finite(maxEuclideanDist)) {
    maxEuclideanDistMeter <- maxEuclideanDist * 1609.344
    distanceMatrixMeterRanking[distanceMatrixMeter > maxEuclideanDistMeter] <- Inf
    distanceMatrix[distanceMatrix > maxEuclideanDist] <- Inf
  }

  # --- Hydrological distance (station-centric) ---
  if (distMethod == "hydrological") {

    # Prepare cost surface
    if (is.null(waterRaster)) {
      costSurf <- terra::rast(system.file("extdata", "waterMask.tif",
                                          package = "deltadata"))
      if (verbose) message("Using bundled Delta water mask.")
    } else {
      costSurf <- waterRaster
      rasterMax <- terra::minmax(costSurf)[2, 1]
      if (rasterMax > 1) {
        warning(
          "The supplied raster has values > 1. A binary water mask is expected ",
          "(NA = land, any non-NA = water). Binarize first:\n",
          "  r[r > wse] <- NA\n  r[!is.na(r)] <- 1 or use `buildWaterMask()`.",
          call. = FALSE
        )
      }
    }

    # --- Validate the cost surface is in a projected CRS ---
    lonlat <- terra::is.lonlat(costSurf)
    if (isTRUE(lonlat)) {
      stop(
        "`waterRaster` must be in a projected CRS (e.g. a UTM zone, in ",
        "metres) for hydrological distance calculations to be valid.",
        call. = FALSE
      )
    } else if (is.na(lonlat)) {
      warning(
        "Could not determine whether `waterRaster` has a projected CRS. ",
        call. = FALSE
      )
    }

    # --- Warn if the raster is coarser than the snap search radius ---
    # A point genuinely adjacent to water can still fail to find a wet
    # cell within `snapDist` purely from grid quantization if the cell
    # size approaches or exceeds `snapDist`.
    cellRes <- terra::res(costSurf)
    if (any(cellRes > snapDist)) {
      warning(sprintf(
        paste0(
          "`waterRaster` resolution (%s m) is coarser than `snapDist` (%d m). ",
          "Consider a `snapDist` of at least 1.5-2x the raster resolution."
        ),
        paste(round(cellRes, 1), collapse = " x "), snapDist
      ), call. = FALSE)
    }

    demCrs <- terra::crs(costSurf)

    # Project points into DEM CRS
    # Can stay in terra, but terra::project() requires firewall interactions
    cdecVect <- terra::vect(sf::st_transform(
      sf::st_as_sf(cdecGpsFiltered, coords = c("longitude", "latitude"), crs = 4326),
      demCrs))
    inputVect <- terra::vect(sf::st_transform(
      sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326),
      demCrs))

    # --- Pre-filter stations that cannot reach water at all ---
    valColStation <- names(costSurf)[1]
    cdecVectBuffered <- terra::buffer(cdecVect, width = snapDist)
    reachableExtract <- terra::extract(costSurf, cdecVectBuffered, ID = TRUE)
    hasWater <- tapply(!is.na(reachableExtract[[valColStation]]), reachableExtract$ID, any)

    stationReachable <- rep(FALSE, nrow(cdecGpsFiltered))
    stationReachable[as.integer(names(hasWater))] <- hasWater

    # Store full list of removed stations before modifying cdecGpsFiltered
    removedStations <- cdecGpsFiltered$station[!stationReachable]

    if (verbose && length(removedStations) > 0) {
      maxPrint <- 5
      if (length(removedStations) <= maxPrint) {
        stationsString <- paste(removedStations, collapse = ", ")
      } else {
        stationsString <- sprintf(
          "%s ... (%d more; see 'unreachableStations' attribute for the complete list)",
          paste(removedStations[1:maxPrint], collapse = ", "),
          length(removedStations) - maxPrint
        )
      }
      message(sprintf(
        "%d of %d candidate station(s) have no water within %d m and will be removed from evaluation: %s",
        length(removedStations), length(stationReachable), snapDist, stationsString
      ))
    }

    # --- HARD FILTER: Remove unreachable stations from datasets ---
    cdecGpsFiltered <- cdecGpsFiltered[stationReachable, ]
    cdecVect <- cdecVect[stationReachable, ]
    distanceMatrix <- distanceMatrix[, stationReachable, drop = FALSE]
    distanceMatrixMeter <- distanceMatrixMeter[, stationReachable, drop = FALSE]
    distanceMatrixMeterRanking <- distanceMatrixMeterRanking[, stationReachable, drop = FALSE]

    # Guard against case where no viable stations remain
    if (nrow(cdecGpsFiltered) == 0) {
      stop("All candidate stations were removed because none were within the required `snapDist` of water.", call. = FALSE)
    }

    # Contract: Inf = candidate never evaluated for this point (pruned, or
    # never nominated); NA = evaluated but this station could not be
    # reached from this point (e.g. failed to snap, gridDist failed, or no
    # water within `snapDist` of the point).
    hydroDistMatrix <- matrix(Inf, nrow = nrow(df), ncol = nrow(cdecGpsFiltered))

    if (isNAll) {

      # --- n = "all": choose an evaluation orientation ---
      # A full ranking needs every station's real distance to every point,
      # no pruning
      # Fewer gridDist() calls (the "auto" signal below) is not the same
      # thing as less total work, though; every call's crop is bounded by
      # its *entire* target set's geographic footprint, not just its count.
      nPoints <- nrow(df)
      nStationsAll <- nrow(cdecGpsFiltered)

      pointCentric <- switch(hydroOrientation,
                             point = TRUE,
                             station = FALSE,
                             # "auto": fewer calls is a reasonable default signal, but it's only
                             # part of the real cost
                             auto = nPoints < nStationsAll
      )

      if (verbose) {
        message(sprintf(
          paste0(
            "n = 'all': %d point(s), %d station(s) -- using %s-centric ",
            "evaluation (%s)."
          ),
          nPoints, nStationsAll, if (pointCentric) "point" else "station",
          if (hydroOrientation == "auto") "auto-selected by count" else "set via `hydroOrientation`"
        ))
      }

      if (pointCentric) {
        for (i in seq_len(nPoints)) {
          progressLabel <- sprintf("Point %d/%d", i, nPoints)
          if (verbose) message(sprintf(
            "  %s (serving %d station(s))...", progressLabel, nStationsAll
          ))

          hydroDistMatrix[i, ] <- hydroDistFromSource(
            sourceVect = inputVect[i, ],
            targetVect = cdecVect,
            targetEuclidMeter = distanceMatrixMeter[i, ],
            costSurf = costSurf,
            snapDist = snapDist,
            gridDistMaxIter = gridDistMaxIter,
            verbose = verbose,
            sourceDesc = progressLabel
          )
        }
      } else {
        for (j in seq_len(nStationsAll)) {
          stationName <- cdecGpsFiltered$station[j]
          progressLabel <- sprintf("Station %d/%d: %s", j, nStationsAll, stationName)
          if (verbose) message(sprintf(
            "  %s (serving %d point(s))...", progressLabel, nPoints
          ))

          hydroDistMatrix[, j] <- hydroDistFromSource(
            sourceVect = cdecVect[j, ],
            targetVect = inputVect,
            targetEuclidMeter = distanceMatrixMeter[, j],
            costSurf = costSurf,
            snapDist = snapDist,
            gridDistMaxIter = gridDistMaxIter,
            verbose = verbose,
            sourceDesc = progressLabel
          )
        }
      }

    } else {

      # --- Ranked (n != "all"): station-centric, with candidate narrowing
      # and branch-and-bound pruning ---
      nVal <- as.integer(n)

      # Track best hydrological distances found so far for branch-and-bound pruning
      bestHydroList <- lapply(seq_len(nrow(df)), function(i) rep(Inf, nVal))

      nCandidates <- hydroCandidates
      if (nVal >= nCandidates) {
        nCandidates <- nVal + 2
        if (verbose) message(sprintf(
          "`hydroCandidates` expanded to %d to accommodate n = %d.", nCandidates, nVal
        ))
      }

      # Top nCandidates station indices per input point: nCandidates x nrow(df)
      pointCandidates <- apply(distanceMatrixMeterRanking, 1, function(row) {
        order(row)[seq_len(min(nCandidates, length(row)))]
      })
      if (!is.matrix(pointCandidates)) {
        pointCandidates <- matrix(pointCandidates, nrow = nCandidates)
      }

      # Transpose trick: order candidateStationIdx by natural rank priority,
      # so pruning bounds tighten early rather than only late in the loop
      candidateStationIdx <- unique(as.vector(t(pointCandidates)))

      # Invert nomination map
      nominatedByStation <- lapply(candidateStationIdx, function(j) {
        which(colSums(pointCandidates == j) > 0L)
      })

      nStations <- length(candidateStationIdx)

      for (s in seq_along(candidateStationIdx)) {

        j <- candidateStationIdx[s]
        nominatedPts <- nominatedByStation[[s]]
        stationName <- cdecGpsFiltered$station[j]

        # --- Dynamic branch-and-bound pruning ---
        activeNominatedPts <- c()
        for (i in nominatedPts) {
          euclDistMeter <- distanceMatrixMeter[i, j]
          # Prune if Euclidean distance exceeds current n-th best hydrological distance (converted to meters)
          nThBestHydroMeter <- bestHydroList[[i]][nVal] * 1609.344

          if (euclDistMeter < nThBestHydroMeter) {
            activeNominatedPts <- c(activeNominatedPts, i)
          }
        }

        # If all points can be pruned for this station, skip the computation completely
        if (length(activeNominatedPts) == 0L) {
          if (verbose) message(sprintf(
            "  Station %d/%d: %s -- Pruned (Euclidean distance exceeds current best hydro bounds). Skipping.",
            s, nStations, stationName
          ))
          # Left at Inf, not NA: these points were never evaluated against
          # this station (they were pruned, not found-unreachable), and may
          # still be reached via another candidate station.
          next
        }

        # Focus only on points that survived pruning
        nominatedPts <- activeNominatedPts

        progressLabel <- sprintf("Station %d/%d: %s", s, nStations, stationName)
        if (verbose) message(sprintf(
          "  %s (serving %d active input point(s))...",
          progressLabel, length(nominatedPts)
        ))

        distsMeter <- hydroDistFromSource(
          sourceVect = cdecVect[j, ],
          targetVect = inputVect[nominatedPts, ],
          targetEuclidMeter = distanceMatrixMeter[nominatedPts, j],
          costSurf = costSurf,
          snapDist = snapDist,
          gridDistMaxIter = gridDistMaxIter,
          verbose = verbose,
          sourceDesc = progressLabel
        )

        hydroDistMatrix[nominatedPts, j] <- distsMeter

        # Update pruning bounds with whatever came back reachable
        for (idx in seq_along(nominatedPts)) {
          val <- distsMeter[idx]
          if (!is.na(val)) {
            i <- nominatedPts[idx]
            valInMiles <- val / 1609.344
            bestHydroList[[i]] <- sort(c(bestHydroList[[i]], valInMiles))[1:nVal]
          }
        }
      }
    }

    # Convert metres to miles to match Euclidean distanceMatrix units
    distanceMatrix <- hydroDistMatrix / 1609.344
  }

  # --- n-based ranking and return ---
  if (isNAll) {
    if (verbose) {
      message("n = 'all': Returning a data frame with a nested list-column 'cdecGage'.")
      message("Use `tidyr::unnest(yourObject, cdecGage)` to expand the results.")
    }

    rankedResults <- apply(distanceMatrix, 1, function(dist) {
      results <- data.frame(cdecGage = cdecGpsFiltered$station, distance = dist)
      results <- results[order(results$distance), ]
      merge(results, availableSensors, by.x = "cdecGage", by.y = "gage", sort = FALSE)
    })

    resultDf <- df
    resultDf$cdecGage <- rankedResults

    # Attach unreachable stations as an attribute
    attr(resultDf, "unreachableStations") <- removedStations

    return(resultDf)
  }

  if (length(n) > 1) {
    n <- n[1]
    warning("n has length > 1. Using only the first element.\n")
  }

  indices <- apply(distanceMatrix, 1, function(row) order(row)[n])
  nthDistance <- distanceMatrix[cbind(seq_len(nrow(df)), indices)]
  unreachable <- !is.finite(nthDistance)

  nthStation <- cdecGpsFiltered[indices, ]
  nthStation$station[unreachable] <- NA_character_
  nthDistance[unreachable] <- NA_real_

  if (verbose && any(unreachable)) {
    message(sprintf(
      "%d input point(s) had no reachable station at rank n = %s; returning NA.",
      sum(unreachable), n
    ))
  }

  resultDf <- cbind(df,
                    cdecGage = nthStation$station,
                    distance = nthDistance,
                    rowIndex = seq_len(nrow(df)))

  mergedDf <- merge(resultDf, availableSensors,
                    by.x = "cdecGage", by.y = "gage", all.x = TRUE)

  mergedDf$duration <- factor(mergedDf$duration, levels = c("event", "hourly", "daily"))
  mergedDf <- mergedDf[order(mergedDf$rowIndex, mergedDf$duration,
                              -mergedDf$sensorNumber), ]
  resultDf <- mergedDf[!duplicated(mergedDf$rowIndex), ]
  resultDf$rowIndex <- NULL

  # Attach unreachable stations as an attribute
  attr(resultDf, "unreachableStations") <- removedStations

  resultDf
}
