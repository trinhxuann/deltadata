#' A leaflet wrapper to plot station GPS coordinates with layer control.
#'
#' @param df A dataframe with 6 required columns: date, station, legend, layer, lat, and lon.
#' @param layerName Character vector used to label the layer element within the pop up at each plotted point.
#' @param dateName Character vector used to label the date element within the pop up at each plotted point.
#' @param ... Optional. Currently only used to determine the `provider` argument within the addProviderTiles.
#'
#' @return A leaflet plot.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(date = 2023,
#' station = c(508, 513, 520, 801),
#' legend = c("Theoretical"),
#' layer = c("Theoretical"),
#' lat = c(38.04717, 38.05886, 38.03217, 38.04369),
#' lon = c(-121.9172, -121.8677, -121.8631, -121.8440))
#'
#' plotGPS(df, layerName = "Survey", dateName = "Year")
#' }
plotGPS <- function(df, layerName = "Layer", dateName = "Date", ...) {

  names(df) <- tolower(names(df))

  if (sum(names(df) %in% c("date", "station", "legend", "layer", "lat", "lon")) != 6) {
    stop("Six required columns: date, station, legend, layer, lat, and lon.",
         call. = F)
  }

  pal <- leaflet::colorFactor("viridis", domain = c(df$legend), reverse = T)

  dfSplit <- split(df, df$layer)

  l <- leaflet::leaflet(width = "100%", height = "1200")

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

#' Identify stations that are more than `d` distance in miles, defaulting to 0.5, away from the theoretical coordinate.
#'
#' @param df A dataframe with 6 required columns: date, station, legend, layer, lat, and lon. See `plotGPS()` for details.
#' @param d Miles threshold to call a coordinate outlying. This distance is measured as the crow flies.
#'
#' @return A data frame with all outlying coordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(date = 2023,
#' station = c(508, 513, 520, 801, 801),
#' legend = c(rep("Theoretical", 4), "StartTow"),
#' layer = c(rep("Theoretical", 4), "13"),
#' lat = c(38.04717, 38.05886, 38.03217, 38.04369, 38.05500),
#' lon = c(-121.9172, -121.8677, -121.8631, -121.8440, -121.8487))
#'
#' gpsOutlier(df, d = 0.5)
#' }
gpsOutlier <- function(df, d = 0.5) {

  originalNames <- names(df)
  names(df) <- tolower(names(df))

  if (sum(grepl("date|station|legend|layer|lat|lon", names(df))) != 6) {
    stop("Four required columns: date, station, legend, layer, lat, and lon. `legend` must have a `Theoretical` label.",
         call. = F)
  }

  theoretical <- subset(df, legend == "Theoretical")

  # Finish translating this over to base R.
  outlierDF <- lapply(unique(df$station), function(x) {

    tows <- subset(df, legend != "Theoretical" & station == x)

    theoretical <- subset(df, legend == "Theoretical" & station == x)

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
      df <- subset(df, outlier == TRUE)

    } else df <- NULL

    df
  })

  outlierDF <- do.call(rbind, outlierDF)

  finTheoreticalStations <- subset(df, legend == "Theoretical" & station %in% unique(outlierDF[["station"]]))
  finTheoreticalStations$lonTheoretical <- NA
  finTheoreticalStations$latTheoretical <- NA
  finTheoreticalStations$distance <- NA
  finTheoreticalStations$outlier <- NA

  fin <- rbind(outlierDF, finTheoreticalStations)

  names(fin)[which(!names(fin) %in% c("lonTheoretical", "latTheoretical", "distance", "outlier"))] <-
    originalNames

  fin[order(fin[["distance"]], decreasing = T), ]
}
