#' Flag outlying values based on a standard deviation threshold
#'
#' @param x A vector of values of interest to check for outliers
#' @param y A vector of values for calculating the mean or median to compare potential outlying values to
#' This defaults to x.
#' @param measure Either "mean" or "median", the summary value to compare the sd. Defaults to mean.
#' @param sdThreshold A value indicating the standard deviation to use. Defaults to 2.
#' @param na.rm T/F. To ignore NAs or not
#'
#' @return A vector of values
#'
#' @importFrom stats median sd
#'
#' @noRd
#' @keywords internal
physicalOutliers <- function(x, y = x, measure = c("mean", "median"),
                             sdThreshold = 2, na.rm = TRUE) {

  measure <- match.arg(measure)
  center <- if (measure == "mean") mean(y, na.rm = na.rm)
  else median(y, na.rm = na.rm)

  # Calculate standard deviation
  sd <- sd(y, na.rm = na.rm)

  # Define outlier threshold
  threshold <- sdThreshold * sd

  # Flag outliers using logical indexing
  outlier <- abs(x - center) > threshold
  data.frame(thresholdValueMin = center - threshold,
             thresholdValueMax = center + threshold,
             outlier = outlier)
}

#' A crosswalk relating IEP Survey column names with one another
#'
#' @param survey Which survey: "ybfmp", "djfmp", "frp", "edsm", "sls", "20mm", "stn", "fmwt", "skt", "sms"
#' @param requiredNames Recommended IEP variable names, used to match to survey specific column names
#' @param xwalk A crosswalk table. Defaults to the official IEP column name crosswalk created by the DUWG
#'
#' @return A data frame that is a crosswalk of IEP recommended names and the name of the survey of interest
#'
#' @importFrom utils tail
#' @noRd
#' @keywords internal
populateCrosswalk <- function(survey, requiredNames, xwalk = deltadata::crosswalk) {
  if (!survey %in% names(xwalk)) {
    possibleNames <- tail(names(xwalk), -5)
    unitPositions <- grep("unit", possibleNames, ignore.case = TRUE)

    for (i in possibleNames[-unitPositions]) {
      cat(i, "\n")
    }
    stop("Survey ", shQuote(survey), " could not be found. Choose a choice from above.", call. = F)
  }

  matchedNames <- which(xwalk[["recommendedName"]] %in% requiredNames)
  df <- xwalk[matchedNames, c("recommendedName", survey)]
}

#' Move comment columns to the end
#'
#' @param data A data frame
#' @param commentNames Comment columns
#'
#' @return A data frame with shifted comments to the end of the data frame
#'
#' @noRd
#' @keywords internal
moveComments <- function(data, commentNames) {
  data[, c(setdiff(names(data), commentNames), commentNames)]
}

#' Convert name via the crosswalk
#'
#' @param outlierDF Outlier data frame created within the `qaqcData` function
#' @param survey Survey of interest, vector. See qaqcData()
#'
#' @return A renamed data frame
#'
#' @importFrom stats na.omit
#'
#' @noRd
#' @keywords internal
convertName <- function(outlierDF, survey) {
  applicableIndex <- which(names(outlierDF) %in% deltadata::crosswalk[["recommendedName"]])
  replaceIndex <- na.omit(match(names(outlierDF), deltadata::crosswalk[["recommendedName"]]))

  names(outlierDF)[applicableIndex] <- deltadata::crosswalk[[survey]][replaceIndex]
  outlierDF
}

#' Apply basic QAQC procedures to a survey dataset
#'
#' @param data A data frame with the required data, dependent on what QAQC procedures
#' you want to run. It is advised to provide the fully joined/merged dataset.
#' @param year A filtering year. This can support custom year labels if desired, e.g.,
#' your sampling season extends across two years.
#' @param survey Name of a qualifying IEP survey name. Currently explicit support
#' only for sls, 20mm, stn, fmwt, and bs.
#' @param convertNames T/F. Should the names be changed to their respective IEP recommended names
#' @param officialGPS Calculate outlying gps points if provided, defaulting to NULL.
#' A data frame containing the official GPS coordinates of the sampling stations.
#' Required to have at least three columns: `station`, `lat`, and `lon`. Coordinates
#' should be provided in degree decimals.
#' @param gpsDistance A value in miles indicating the threshold distance beyond which is an outlier.
#' Defaults to 0.5 miles
#' @param towSchedule Calculate outlying cable length values if provided.
#' A data frame containing the tow schedule. Several tow schedules
#' are provided in the package within the `towSchedule` list. See details.
#' @param meterSchedule Calculate outlying flow meter readings if provided.
#' A data frame containing the expected range for a meter reading
#' based on the duration of the tow. Several meter schedules are provided in the package
#' within the `meterSchedule` list. You can provide your own data frame following the format.
#' @param waterQualityVariables Calculate outlying water quality variables.
#' @param startingGPSFormat Either degrees, minutes, and seconds (dms) or degrees and decimal minutes (ddm).
#' Format of your GPS coordinates. This will be convert to decimal degrees
#' @param stdev Defaults to 2. The number of standard deviation away from the mean to flag as an outlying water quality value
#' @param waterQualityGroupings A list of grouping variables to iterate through the water quality check.
#' By default, calculates per station and per station and month.
#'
#' @details
#' For the `towSchedule` and `meterSchedule` arguments, you can use a schedules recorded
#' in the package (`towSchedule` or `meterSchedule`), currently
#' available only for center surveys, which were based on protocol documentation.
#' If not available, you can provide your own schedule. See the example on how to create
#' such a table.
#'
#' @return A list of objects containing identified outliers or rows with missing data points
#' @export
#'
#' @importFrom lubridate year month
#' @importFrom stats sd
#' @examples
#' \dontrun{
#' # You can create your own tow schedule with the following code:
#' aTowSchedule <- data.frame(
#' maxDepth = c(10, 15, 20, 25, 30, 35, Inf),
#' depth = cut(c(10, 15, 20, 25, 30, 35, Inf),
#' breaks = c(0, 10, 15, 20, 25, 30, 35, Inf), right = T, include.lowest = T),
#' cableLength = c(0, 50, 100, 125, 150, 175, 200)
#' )
#'
#' # An example using the CDFW 20 mm survey:
#' # Download the file and extract the required relationship and data tables:
#' # You will have to provide yourself with permissions to extract this table.
#'
#' # Grab relationship schema from the Access database for default joins
#' tmmRelationship <- bridgeAccess(
#' file = "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/20mm_New.zip",
#' tables = "MSysRelationships")[[1]]
#' # Grab relational tables of interest. The minimum should include station,
#' tow, and water sample data. Other columns are preserved but are not used
#' tmmTables <- bridgeAccess(file =
#' "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/20mm_New.zip",
#' tables = c("Station", "Survey", "Tow", "Gear"))
#' tmmTables$`20mmStations` <- bridgeAccess(file =
#' "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/20mm_New.zip",
#' tables = c("20mmStations"))[[1]]
#' officialGPS <- data.frame(
#' station = tmmTables$`20mmStations`$Station,
#' lat = decimalDegrees(paste(
#' tmmTables$`20mmStations`$LatD,
#' tmmTables$`20mmStations`$LatM,
#' tmmTables$`20mmStations`$LatS), type = "dms"),
#' lon = decimalDegrees(paste(
#' tmmTables$`20mmStations`$LonD,
#' tmmTables$`20mmStations`$LonM,
#' tmmTables$`20mmStations`$LonS
#' ), type = "dms", isLongitude = T)
#' )
#'
#' joinedTMM <- schemaJoin(tmmRelationship, tmmTables)
#' tmmQAQC <- qaqcData(data = joinedTMM, year = 2023, survey = "20mm",
#'  officialGPS = officialGPS, gpsDistance = 0.5, startingGPSFormat = "dms",
#'  towSchedule = towSchedule$`20mm`,
#'  meterSchedule = meterSchedule$`20mm`)
#' }
qaqcData <- function(data,
                     year,
                     survey = c("ybfmp", "djfmp", "frp", "edsm", "sls", "20mm",
                                "stn", "fmwt", "skt", "sms"),
                     convertNames = F,
                     officialGPS = NULL, gpsDistance = 0.5, startingGPSFormat = c("dms", "ddm"),
                     towSchedule = NULL,
                     meterSchedule = NULL,
                     waterQualityVariables = c("BottomDepth", "WaterTemperature",
                                               "WaterTemperatureTop", "WaterTemperatureBottom",
                                               "SpecificConductance", "SpecificConductanceTop",
                                               "SpecificConductanceBottom", "Secchi", "TurbidityNTU",
                                               "TurbidityTopNTU", "TurbidityBottomNTU",
                                               "Salinity", "SalinityTop", "SalinityBottom"),
                     stdev = 2,
                     waterQualityGroupings = list(
                       "StationCode",
                       c("StationCode", "Month")
                     )) {

  # --- Prepare data ---
  # Standardize names, should really only be a 2 column table
  # requiredNames will be a vector specified in the package itself, manually set
  # Another approach would be to just read in the column of "Recommended Name" from the excel sheet
  crosswalk <- populateCrosswalk(survey, requiredNames = crosswalk[["recommendedName"]])
  crosswalk[["dataName"]] <- names(data)[match(crosswalk[[survey]], names(data))]
  crosswalk[["check"]] <- crosswalk[[survey]] %in% names(data)

  incorrectNames <- na.omit(crosswalk[[survey]][!crosswalk[["check"]] & !is.na(crosswalk[["dataName"]])])
  if (length(incorrectNames) > 0) {
    warning("The following columns cannot be matched in the dataset: ",
            paste(incorrectNames, collapse = ", "), call. = F)
  }

  matchIndices <- match(names(data), crosswalk[[survey]])
  names(data)[!is.na(matchIndices)] <- crosswalk[["recommendedName"]][matchIndices[!is.na(matchIndices)]]

  # Certain column names may not be used by all surveys, if so, make dummy columns
  # Certain surveys do not have a "MethodCode" if they only have 1 gear type
  potentialColumns <- c("MethodCode")
  missingNames <- potentialColumns[!potentialColumns %in% names(data)]
  if (length(missingNames) > 0) {
    data[, potentialColumns] <- NA
  }

  # Calculate year if not calculated
  if (is.null(data$Year)) {
    data[["Year"]] <- lubridate::year(data[["SampleDate"]])
    # Also calculate month as well
    data[["Month"]] <- lubridate::month(data[["SampleDate"]])
  }
  dataYear <- data[data$Year == year, ]
  if (nrow(dataYear) == 0) {
    stop("Year ", year, " does not have any data.", call. = F)
  }

  # All outlier data frames will require three columns:
  requiredColumns <- c("SampleDate", "SurveyNumber", "StationCode")
  # Comments are also good to have to understand why certain outliers exist
  commentColumns <- grep("comments", names(dataYear), value = TRUE, ignore.case = T)

  # --- GPS outliers ---
  if (!is.null(officialGPS)) {

    # Check for missing columns and create them if needed in the officialGPS
    additionalGPSNames <- c("layer", "legend")
    missingGPSNames <- additionalGPSNames[!additionalGPSNames %in% names(officialGPS)]
    if (length(missingGPSNames) > 0) {
      officialGPS[, missingGPSNames] <- "Theoretical"
    }

    # Check input lat/lon format
    gpsColNames <- grep("latitude|longitude", names(dataYear), ignore.case = T, value = T)

    # Remove duplicates
    gpsDF <- unique(dataYear[, c(requiredColumns, gpsColNames, commentColumns)])

    # Split into start/end coordinates
    gpsStart <- data.frame(
      date = gpsDF[, "SampleDate"],
      station = gpsDF[, "StationCode"],
      layer = gpsDF[, "SurveyNumber"],
      legend = "start",
      gpsDF[, c(gpsColNames, commentColumns)]
    )

    gpsEnd <- data.frame(
      date = gpsDF[, "SampleDate"],
      station = gpsDF[, "StationCode"],
      layer = gpsDF[, "SurveyNumber"],
      legend = "end",
      gpsDF[, c(gpsColNames, commentColumns)]
    )

    startingGPSFormat <- match.arg(startingGPSFormat)
    latLonStartEnd <- c("LatitudeDegreeStart", "LatitudeMinuteStart", "LatitudeSecondStart",
                        "LongitudeDegreeStart", "LongitudeMinuteStart", "LongitudeSecondStart",
                        "LatitudeDegreeEnd", "LatitudeMinuteEnd", "LatitudeSecondEnd",
                        "LongitudeDegreeEnd", "LongitudeMinuteEnd", "LongitudeSecondEnd")
    # Convert to degrees; some surveys record in DMS
    if (all(latLonStartEnd %in% names(gpsDF))) {

      gpsColumnIndices <- which(names(gpsDF) %in% latLonStartEnd)
      gpsDF[, gpsColumnIndices] <- lapply(gpsDF[, gpsColumnIndices], as.numeric)

      gpsStart[["lat"]] <- suppressWarnings(decimalDegrees(paste(gpsDF[, "LatitudeDegreeStart"],
                                                                 gpsDF[, "LatitudeMinuteStart"],
                                                                 gpsDF[, "LatitudeSecondStart"]),
                                                           type = startingGPSFormat))
      gpsStart[["lon"]] <- suppressWarnings(decimalDegrees(paste(gpsDF[, "LongitudeDegreeStart"],
                                                gpsDF[, "LongitudeMinuteStart"],
                                                gpsDF[, "LongitudeSecondStart"]),
                                          type = startingGPSFormat, isLongitude = T))

      gpsEnd[["lat"]] = suppressWarnings(decimalDegrees(paste(gpsDF[, "LatitudeDegreeEnd"],
                                             gpsDF[, "LatitudeMinuteEnd"],
                                             gpsDF[, "LatitudeSecondEnd"]),
                                       type = startingGPSFormat))
      gpsEnd[["lon"]] = suppressWarnings(decimalDegrees(paste(gpsDF[, "LongitudeDegreeEnd"],
                                             gpsDF[, "LongitudeMinuteEnd"],
                                             gpsDF[, "LongitudeSecondEnd"]),
                                       type = startingGPSFormat, isLongitude = T))
    } else {
      if (all(c("LatitudeStart", "LongitudeStart") %in% names(gpsDF)) |
          all(c("Latitude", "Longitude") %in% names(gpsDF))) {
        # This still doesn't solve "Latitude" and "Longitude"

        gpsStart[, c("lat", "lon")] <- gpsDF[, c("LatitudeStart", "LongitudeStart")]
        gpsEnd[, c("lat", "lon")] <- gpsDF[, c("LatitudeEnd", "LongitudeEnd")]
      }
    }

    gpsActual <- rbind(gpsStart, gpsEnd)
    gpsNA <- split(gpsActual, is.na(gpsActual[["lat"]]) | is.na(gpsActual[["lon"]]))

    # Bind to official coordinates
    # Need an officialGPS data frame here...
    missingColumns <- setdiff(names(gpsNA$`FALSE`), names(officialGPS))
    officialGPS[missingColumns] <- NA
    gpsTotal <- rbind(gpsNA$`FALSE`, officialGPS)

    # Grab the outlying points
    outlierGPS <- gpsOutlier(gpsTotal, d = gpsDistance, returnAll = F)

    # Generate the plots
    gpsOutlierPlot <- plotGPS(outlierGPS, layerName = "Survey")

    # Remove the theoretical rows + Convert name back
    outlierGPSDF <- outlierGPS[which(outlierGPS[["legend"]] != "Theoretical"), ]
    outlierGPSDF <- data.frame(
      SampleDate = outlierGPSDF[["date"]],
      StationCode = outlierGPSDF[["station"]],
      SurveyNumber = outlierGPSDF[["layer"]],
      outlierGPSDF[, which(!names(outlierGPSDF) %in% c("date", "station", "layer",
                                                       commentColumns))],
      outlierGPSDF[, which(names(outlierGPSDF) %in% commentColumns)]
    )

    if (!convertNames) {
      outlierGPSDF <- convertName(outlierGPSDF, survey)
    }

    # GPS stations that did not have a theoretical coordinate?
    missingTheoreticalGPS <- merge(gpsActual, officialGPS, by = "station", all.x = T)
    missingGPS <- missingTheoreticalGPS[["station"]][is.na(missingTheoreticalGPS[["lat.y"]] |
                                                             is.na(missingTheoreticalGPS[["lon.y"]]))]

  } else {
    gpsOutlierPlot <- "officialGPS not provided"
    outlierGPSDF <- "officialGPS not provided"
    missingTheoreticalGPS <- "officialGPS not provided"
  }

  # --- Cable length outliers ---
  # Dependent on the tow schedule
  if (!is.null(towSchedule)) {
    towDF <- unique(dataYear[, c(requiredColumns, "BottomDepth", "CableLength",
                                 commentColumns)])

    # Apply depth bins
    depthCut <- cut(towDF[["BottomDepth"]], breaks = c(0, towSchedule$maxDepth), include.lowest = T, right = T)
    # Flag mismatched depth and cable length tows
    towOutlierDF <- outer(depthCut, levels(towSchedule$depth), `==`) &
      outer(towDF[["CableLength"]], towSchedule$cableLength, `!=`)

    cableLength <- towSchedule$cableLength[max.col(towOutlierDF, ties.method = "first")]
    outlierTowIndex <- rowSums(towOutlierDF) > 0

    outlierTowDF <- towDF[outlierTowIndex, ]
    outlierTowDF$theoreticalCableLength <- cableLength[outlierTowIndex]
    outlierTowDF$cableLengthOutlier <- T
    outlierTowDF <- moveComments(outlierTowDF, commentColumns)

    if (!convertNames) {
      outlierTowDF <- convertName(outlierTowDF, survey)
    }

    # cable length values not matched to towSchedule
    missingCableLength <- towDF[!towDF[["CableLength"]] %in% towSchedule$cableLength, ]
  } else {
    outlierTowDF <- "towSchedule not provided"
    missingCableLength <- "towSchedule not provided"
  }

  # --- Meter difference outliers ---
  if (!is.null(meterSchedule)) {
    # Duration, GearCode, MeterCheck
    meterDF <- unique(dataYear[, c(requiredColumns, c("Duration", "MethodCode", "FlowMeterDifference"),
                                   commentColumns)])

    # Flag meter difference based on tow duration
    # Sometimes, the gear column isn't used if there's only 1 gear type
    compareNA <- function(x, y) {
      ifelse(is.na(x) & is.na(y), TRUE, x == y)
    }

    meterOutlier <- with(meterSchedule, {
      outer(meterDF$Duration, duration, `==`) &
        outer(meterDF$MethodCode, gear, compareNA) &
        (outer(meterDF$FlowMeterDifference, meterMin, `<`) |
           outer(meterDF$FlowMeterDifference, meterMax, `>`))
    })

    meterRangeNamed <- paste0("(", meterSchedule[["meterMin"]], ", ",
                              meterSchedule[["meterMax"]], ")")

    meterRange <- meterRangeNamed[max.col(meterOutlier, ties.method = "first")]
    outlierMeterIndex <- rowSums(meterOutlier) > 0
    meterReadingTheoretical <- meterRange[outlierMeterIndex]

    outlierMeterDF <- meterDF[outlierMeterIndex, ]
    outlierMeterDF[["meterReadingTheoretical"]] <- meterReadingTheoretical
    outlierMeterDF[["meterReadingOutlier"]] <- T
    outlierMeterDF <- moveComments(outlierMeterDF, commentColumns)

    if (!convertNames) {
      outlierMeterDF <- convertName(outlierMeterDF, survey)
    }

    # Missing methodCode?
    missingMeterGearCode <- meterDF[!meterDF[["MethodCode"]] %in% unique(meterSchedule[["gear"]]), ]

    # --- Duration outliers ---

    durationDF <- unique(dataYear[, c(requiredColumns, "Duration", commentColumns)])
    # Flag any duration not specified in the meterSchedule
    outlierDurationDF <- durationDF[which(!durationDF[["Duration"]] %in% meterSchedule[["duration"]]), ]

    if (nrow(outlierDurationDF) > 0) {
      outlierDurationDF[["theoreticalDuration"]] <- paste(meterSchedule[["duration"]], collapse = ", ")
      outlierDurationDF[["durationOutlier"]] <- T
    } else {
      outlierDurationDF[["theoreticalDuration"]] <- character(0)
      outlierDurationDF[["durationOutlier"]] <- logical(0)
    }

    outlierDurationDF <- moveComments(outlierDurationDF, commentColumns)

    if (!convertNames) {
      outlierDurationDF <- convertName(outlierDurationDF, survey)
    }
  } else {
    outlierMeterDF <- "meterSchedule not provided"
    missingMeterGearCode <- "meterSchedule not provided"
    outlierDurationDF <- "meterSchedule not provided"
  }

  # --- Water quality outliers ---
  # Find outlying water quality parameters; +- 2 sd of the mean
  # Default to BottomDepth, Temp, TopEC, BottomEC, Secchi, Turbidity (will likely have to add more)
  waterQualityIndex <- which(crosswalk[["recommendedName"]] %in% waterQualityVariables &
                               !is.na(crosswalk[[survey]]))

  waterQualityVariables <- crosswalk[waterQualityIndex, "recommendedName"]
  waterQualityDF <- unique(dataYear[, c(requiredColumns, "StartTime",
                                        setdiff(unique(unlist(waterQualityGroupings)),
                                                requiredColumns),
                                        waterQualityVariables, commentColumns)])
  # Need the full database to calculate mean/medians from
  waterQualityTotal <- unique(data[, c(requiredColumns, setdiff(unique(unlist(waterQualityGroupings)),
                                                                requiredColumns),
                                       waterQualityVariables)])

  # Which variables are duplicated, e.g., across tows recorded only once
  uniqueCounts <- aggregate(waterQualityDF[, waterQualityVariables],
                            by = list(date = waterQualityDF[, "SampleDate"],
                                      stationCode = waterQualityDF[, "StationCode"]),
                            FUN = function(x) length(unique(x)))

  oncePerSampleVariables <- names(which(apply(uniqueCounts[, -c(1, 2)], 2, function(x) all(x == 1))))

  # Return data frame per variable per grouping
  outlierWaterQuality <- lapply(waterQualityGroupings, function(groupings) {
    outlierDF <- lapply(waterQualityVariables, function(variable) {

      # Calculate group-wise means and standard deviations
      groupMeans <- tapply(waterQualityTotal[[variable]], waterQualityTotal[groupings], mean, na.rm = TRUE)
      groupSD <- tapply(waterQualityTotal[[variable]], waterQualityTotal[groupings], sd, na.rm = TRUE)

      groupColumns <- expand.grid(dimnames(groupMeans)) # Get all group combinations
      thresholds <- data.frame(
        groupColumns,
        mean = as.vector(groupMeans),
        sd = as.vector(groupSD),
        thresholdValueMin = as.vector(groupMeans) - stdev * as.vector(groupSD),
        thresholdValueMax = as.vector(groupMeans) + stdev * as.vector(groupSD)
      )

      if (variable %in% oncePerSampleVariables) {
        data <- unique(waterQualityDF[, c(requiredColumns, "StartTime",
                                          setdiff(groupings, requiredColumns),
                                          variable, commentColumns)])
      } else {
        data <- waterQualityDF[, c(requiredColumns, "StartTime",
                                   setdiff(groupings, requiredColumns),
                                          variable, commentColumns)]
      }

      # Merge with the subset data and flag outliers
      df <- merge(data,
                  thresholds,
                  by = groupings)
      df[["outlier"]] <- abs(df[[variable]] - df[["mean"]]) > (stdev * df[["sd"]])

      # Filter and process
      dfMissing <- df[is.na(df[[variable]]), ]
      df <- df[which(df[["outlier"]]), ]

      if (!convertNames) {
        df <- convertName(df, survey)
        dfMissing <- convertName(dfMissing, survey)
      }

      list(df = moveComments(df, commentColumns),
           dfMissing = moveComments(dfMissing, commentColumns))
    })
    names(outlierDF) <- waterQualityVariables

    df <- lapply(outlierDF, "[[", "df")
    missingDF <- lapply(outlierDF, "[[", "dfMissing")
    list(df = df,
         missingDF = missingDF)
  })
  names(outlierWaterQuality) <- sapply(waterQualityGroupings, paste, collapse = "_")

  return(list(
    gpsPlot = gpsOutlierPlot,
    outlierGPS = outlierGPSDF,
    outlierCableLength = outlierTowDF,
    outlierMeterCount = outlierMeterDF,
    outlierDuration = outlierDurationDF,
    waterQuality = lapply(outlierWaterQuality, "[[", "df"),
    missingData = list(
      missingGPS = missingGPS,
      missingCableLength = missingCableLength,
      missingMeterGearCode = missingMeterGearCode,
      missingWaterQuality = outlierWaterQuality[[1]]$missingDF
    )
  ))
}
