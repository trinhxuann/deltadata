#' Plot station GPS coordinates with layer control.
#'
#' @description
#' This is a leaflet wrapper. The input data frame requires very specific columns that must be included. The format caters to IEP surveys but should be generalizable to other datasets.
#'
#' @param df A dataframe with 6 required columns: date, station, legend, layer,
#' lat, and lon. See the "details" section for additional information.
#' @param layerName Character vector used to label the layer element within the
#' pop up at each plotted point.
#' @param dateName Character vector used to label the date element within the
#' pop up at each plotted point.
#' @param height Height of the leaflet map.
#' @param ... Optional. Currently only used to determine the `provider` argument
#' within the addProviderTiles.
#'
#' @details
#' The input data frame must have at least six specific columns.
#'
#' `date` represents when the samples were taken and is used to label each data point in the pop-up label. This does not technically need to be a date and can be any date-related label that makes sense for your project, e.g., month or year.
#'
#' `station` represents the name of the sampling points and is used to label each data point on the map.
#'
#' `legend` contains the legend labels and marks the colors of the data points on the map. This is useful if there are different instances of the same sampling point, e.g., start and end GPS coordinates.
#'
#' `layer` determines the layers that are depicted in the layer control of the plot. Use this to filter data points on the map, e.g., per survey number.
#'
#' `lat` contains the latitude coordinates of your sampling point
#'
#' `lon` contains the longitude coordinates of your sampling point
#'
#' @return A leaflet plot.
#' @export
#'
#' @importFrom leaflet leaflet colorFactor providers addProviderTiles
#' addCircleMarkers labelOptions addLegend addLayersControl layersControlOptions
#'
#' @examples
#' \donttest{
#' df <- data.frame(date = 2023,
#' station = c(508, 513, 520, 801),
#' legend = c("Theoretical"),
#' layer = c("Theoretical"),
#' lat = c(38.04717, 38.05886, 38.03217, 38.04369),
#' lon = c(-121.9172, -121.8677, -121.8631, -121.8440))
#'
#' plotGPS(df, layerName = "Survey", dateName = "Year")
#' }
plotGPS <- function(df, layerName = "Layer", dateName = "Date", height = 1200, ...) {

  names(df) <- tolower(names(df))

  if (sum(names(df) %in% c("date", "station", "legend", "layer", "lat", "lon")) != 6) {
    stop("Six required columns: date, station, legend, layer, lat, and lon.",
         call. = F)
  }

  pal <- leaflet::colorFactor("viridis", domain = c(df$legend), reverse = T)

  dfSplit <- split(df, df$layer)

  l <- leaflet::leaflet(width = "100%", height = as.character(height))

  additionalArgs <- list(...)

  if (any(names(additionalArgs) %in% "provider")) {
    provider <- additionalArgs$provider
  } else {
    provider <- leaflet::providers$CartoDB.PositronNoLabels
  }

  l <- leaflet::addProviderTiles(l, provider = provider)

  # Loop through the names of dfSplit and add circle markers for each dataframe
  for (layer in names(dfSplit)) {

    # Add circle markers
    l <- leaflet::addCircleMarkers(
      map = l,
      data = dfSplit[[layer]],
      lng = ~lon, lat = ~lat,
      label = ~as.character(station),
      color = ~pal(legend),
      group = layer,
      radius = 7,
      stroke = FALSE, fillOpacity = 0.8,
      labelOptions = leaflet::labelOptions(noHide = TRUE,
                                           offset = c(18, 0),
                                           textOnly = TRUE,
                                           textsize = "12px",
                                           direction = "center"),
      popup = ~paste(layerName, layer, paste0("<br>", dateName), date)
    )
  }

  l <- leaflet::addLegend(map = l, pal = pal, values = unique(df$legend), opacity = 1)

  leaflet::addLayersControl(map = l,
                            overlayGroups = names(dfSplit),
                            options = leaflet::layersControlOptions(collapsed = FALSE))
}

#' Isolate outlying stations
#'
#' @description
#' Identify stations that are more than `d` distance in miles, defaulting to
#' 0.5, away from the theoretical coordinate.
#'
#' @param df A dataframe with 6 required columns: date, station, legend, layer,
#' lat, and lon. See \code{\link{plotGPS}} for details.
#' @param d Miles threshold to call a coordinate outlying. This distance is
#' measured as the crow flies.
#' @param returnAll Logical, to return all rows or not. Defaults to F, returning
#' only the outlying points
#'
#' @return A data frame with all outlying coordinates.
#' @export
#'
#' @importFrom geosphere distVincentyEllipsoid
#'
#' @examples
#' \donttest{
#' df <- data.frame(date = 2023,
#' station = c(508, 513, 520, 801, 801),
#' legend = c(rep("Theoretical", 4), "StartTow"),
#' layer = c(rep("Theoretical", 4), "13"),
#' lat = c(38.04717, 38.05886, 38.03217, 38.04369, 38.05500),
#' lon = c(-121.9172, -121.8677, -121.8631, -121.8440, -121.8487))
#'
#' gpsOutlier(df, d = 0.5)
#' }
gpsOutlier <- function(df, d = 0.5, returnAll = F) {

  originalNames <- names(df)
  names(df) <- tolower(names(df))

  if (sum(grepl("date|station|legend|layer|lat|lon", names(df))) != 6) {
    stop("Four required columns: date, station, legend, layer, lat, and lon. `legend` must have a `Theoretical` label.",
         call. = F)
  }

  if (all(!unique(df[["legend"]]) %in% "Theoretical"))
    stop("A ", shQuote("Theoretical"), " group in the `legend` column must be present.")

  theoretical <- df[df[["legend"]] == "Theoretical", ]

  # Finish translating this over to base R.
  outlierDF <- lapply(unique(df$station), function(x) {

    tows <- df[(df[["legend"]] != "Theoretical" & df[["station"]] == x), ]
    theoretical <- df[(df[["legend"]] == "Theoretical" & df[["station"]] == x), ]

    if (nrow(theoretical) > 0 & nrow(tows) > 0) {
      df <- tows
      # Add new columns with values from 'theoretical'
      df$lonTheoretical <- theoretical$lon
      df$latTheoretical <- theoretical$lat
      df$distance <- geosphere::distVincentyEllipsoid(cbind(df$lon, df$lat),
                                                      cbind(df$lonTheoretical, df$latTheoretical))
      df$distance <- df$distance/1609.34
      # Add outlier column based on condition
      df$outlier <- ifelse(df$distance > d, TRUE, FALSE)
      if (!returnAll) {
        df <- df[which((df[["outlier"]] == TRUE)), ]
      }
    } else {
      if (nrow(theoretical) == 0 & nrow(tows) > 0) {
        df <- tows
        df[, c("lonTheoretical", "latTheoretical", "distance", "outlier")] <- NA
      } else df <- NULL
    }
    df
  })

  outlierDF <- do.call(rbind, outlierDF)
  if (is.null(outlierDF)) stop("No station matched the theoreticals.")

  finTheoreticalStations <- df[(df[["legend"]] == "Theoretical" & df[["station"]] %in% unique(outlierDF[["station"]])), ]
  finTheoreticalStations$lonTheoretical <- NA
  finTheoreticalStations$latTheoretical <- NA
  finTheoreticalStations$distance <- NA
  finTheoreticalStations$outlier <- NA

  fin <- rbind(outlierDF, finTheoreticalStations)

  names(fin)[which(!names(fin) %in% c("lonTheoretical", "latTheoretical", "distance", "outlier"))] <-
    originalNames

  fin[order(fin[["distance"]], decreasing = T), ]
}

#' Convert GPS Coordinates to Decimal Degrees
#'
#' @param degrees A numeric vector
#' @param minutes A numeric vector
#' @param seconds A numeric vector
#'
#' @return A numeric vector in decimal degrees
#' @export
#'
#' @examples
#' gpsDF <- data.frame(LatD = c(rep(38, 7)),
#' LatM = c(2, 3, 3, 4, 4, 3, 5),
#' LatS = c(34.4, 37.1, 49, 35, 16, 39.9, 57.2))
#'
#' decimalDegrees(degrees = gpsDF$LatD,
#' minutes = gpsDF$LatM,
#' Seconds = gpsDF$LatS)
decimalDegrees <- function(degrees, minutes, seconds) {
  degrees + minutes/60 + seconds/3600
}

#' Seperate lat/lon
#'
#' @param x A vector of latitude or longitude in the format of degrees, minutes, and seconds
#'
#' @return A data frame of the coordinates in DMS
#'
#' @keywords internal
splitCoordinates <- function(x) {
  splitText <- strsplit(x, " ")[[1]]

  degreesMinutesSeconds <- lapply(splitText, as.numeric)
  data.frame(degrees = degreesMinutesSeconds[[1]],
             minutes = degreesMinutesSeconds[[2]],
             seconds = degreesMinutesSeconds[[3]])
}
