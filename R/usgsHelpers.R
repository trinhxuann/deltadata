#' Locate and explore USGS NWIS monitoring stations within a spatial area
#'
#' @description
#' Identifies USGS National Water Information System (NWIS) monitoring stations
#' situated within a user-defined spatial area (polygon, bounding box, or coordinates).
#' Can filter stations reporting specific 5-digit parameter codes (e.g. 00010 for temperature)
#' or common variable aliases. By default, returns a character vector of USGS site numbers;
#' setting \code{map = TRUE} renders an interactive Leaflet map detailing station metadata
#' and available period of record in popups.
#'
#' **Developed with Gemini 3.8 Flash**
#'
#' @param area Spatial boundary of interest. Accepts an \code{sf} or \code{sfc}
#'   polygon/multipolygon, an \code{sf::st_bbox}, a \code{terra::SpatVector},
#'   or a numeric vector of 4 coordinates formatted as \code{c(xmin, ymin, xmax, ymax)}.
#'   If omitted, prints usage guidelines and returns \code{NULL} invisibly.
#' @param parameterCd Optional 5-digit USGS parameter code (character or numeric,
#'   e.g. \code{"00010"}, \code{"00060"}), or \code{"all"} to return all stations
#'   regardless of parameter. Takes precedence over \code{variable}.
#' @param variable Common parameter alias: \code{"temp"}, \code{"flow"} (or \code{"discharge"}),
#'   \code{"stage"} (or \code{"height"}), \code{"ec"} (or \code{"cond"}), \code{"turbidity"},
#'   or \code{"do"}. Ignored if \code{parameterCd} is supplied.
#' @param map Logical. If \code{FALSE} (default), returns a character vector of
#'   USGS site numbers. If \code{TRUE}, returns an interactive \code{leaflet} map.
#' @param addStations Character vector of additional USGS site number(s) to
#'   plot on the map for spatial comparison (e.g. \code{"11455420"}).
#'   Only applicable when \code{map = TRUE}.
#' @param add Alias for \code{addStations}.
#' @param verbose Logical. Print informative guidance and summaries. Defaults to \code{TRUE}.
#' @param timeout Numeric. Maximum seconds to wait for USGS API responses. Defaults to 60.
#'
#' @return
#' \itemize{
#'   \item If \code{map = FALSE}: A character vector of matching USGS site numbers.
#'   \item If \code{map = TRUE}: An interactive \code{leaflet} map widget.
#'   \item If arguments are missing: A parameter summary catalog returned invisibly
#'     with console guidance.
#' }
#'
#' @export
#'
#' @importFrom sf st_as_sf st_as_sfc st_bbox st_crs st_transform st_intersects st_union st_geometry
#' @importFrom leaflet leaflet addTiles addProviderTiles addPolygons addCircleMarkers addLayersControl addLegend layersControlOptions providers
#' @importFrom httr GET http_status content timeout
#' @importFrom utils read.delim head
#'
#' @examples
#' \dontrun{
#' # 1. Self-directing check (omitting area shows guidance):
#' findUSGS()
#'
#' # 2. Using an EDSM Subregion and checking what parameters exist:
#' deltamapr::R_EDSM_Subregions_Mahardja_FLOAT %>%
#'   dplyr::filter(SubRegion == "Upper Sacramento River Ship Channel") %>%
#'   findUSGS()
#'
#' # 3. Query water temperature (parameterCd = "00010" or variable = "temp"):
#' deltamapr::R_EDSM_Subregions_Mahardja_FLOAT %>%
#'   dplyr::filter(SubRegion == "Upper Sacramento River Ship Channel") %>%
#'   findUSGS(variable = "temp")
#'
#' # 4. Interactive map with a comparison station (Freeport: 11455420):
#' deltamapr::R_EDSM_Subregions_Mahardja_FLOAT %>%
#'   dplyr::filter(SubRegion == "Upper Sacramento River Ship Channel") %>%
#'   findUSGS(variable = "temp", map = TRUE, add = "11455420")
#' }
findUSGS <- function(area = NULL,
                     parameterCd = NULL,
                     variable = NULL,
                     map = FALSE,
                     addStations = NULL,
                     add = NULL,
                     verbose = TRUE,
                     timeout = 60) {

  # Resolve alias for addStations
  if (is.null(addStations) && !is.null(add)) {
    addStations <- add
  }

  # Lookup of common Bay-Delta USGS parameter codes
  commonParmMap <- c(
    "00010" = "Water Temperature (\u00b0C)",
    "00060" = "Discharge / Flow (cfs)",
    "00065" = "Gage Height / Stage (ft)",
    "00095" = "Specific Conductance (\u00b5S/cm)",
    "63680" = "Turbidity (FNU)",
    "00300" = "Dissolved Oxygen (mg/L)",
    "00400" = "pH"
  )

  # -------------------------------------------------------------------------
  # 1. Self-Directing Guidance if `area` is Missing
  # -------------------------------------------------------------------------
  if (missing(area) || is.null(area)) {
    if (isTRUE(verbose)) {
      message("Please provide a spatial `area` to query USGS NWIS stations.")
      message("Supported formats:")
      message("  1. An `sf` or `sfc` polygon/multipolygon (e.g. deltamapr::R_EDSM_Subregions_Mahardja_FLOAT)")
      message("  2. An `sf::st_bbox` object")
      message("  3. A `terra::SpatVector` object")
      message("  4. A numeric vector of 4 coordinates: c(xmin, ymin, xmax, ymax)")
      message("\nStandard USGS Parameter Codes:")
      for (cd in names(commonParmMap)) {
        message(sprintf("  - %s: %s", cd, commonParmMap[[cd]]))
      }
    }
    return(invisible(NULL))
  }

  # -------------------------------------------------------------------------
  # 2. Standardize Input `area` into WGS84 `sfc`
  # -------------------------------------------------------------------------
  areaSf <- tryCatch({
    if (inherits(area, "SpatVector")) {
      sf::st_as_sf(area)
    } else if (inherits(area, "bbox")) {
      sf::st_as_sfc(area)
    } else if (is.numeric(area) && length(area) == 4) {
      if (all(c("xmin", "ymin", "xmax", "ymax") %in% names(area))) {
        sf::st_as_sfc(sf::st_bbox(area, crs = sf::st_crs(4326)))
      } else if (all(c("lonMin", "latMin", "lonMax", "latMax") %in% names(area))) {
        sf::st_as_sfc(sf::st_bbox(c(
          xmin = area[["lonMin"]], ymin = area[["latMin"]],
          xmax = area[["lonMax"]], ymax = area[["latMax"]]
        ), crs = sf::st_crs(4326)))
      } else {
        # Default order: xmin, ymin, xmax, ymax
        sf::st_as_sfc(sf::st_bbox(c(
          xmin = area[1], ymin = area[2],
          xmax = area[3], ymax = area[4]
        ), crs = sf::st_crs(4326)))
      }
    } else if (inherits(area, c("sf", "sfc"))) {
      sf::st_geometry(area)
    } else {
      stop("`area` format is not recognized. Please provide an sf/sfc object, SpatVector, or bbox vector.", call. = FALSE)
    }
  }, error = function(e) {
    stop("Could not parse `area`: ", conditionMessage(e), call. = FALSE)
  })

  # Ensure area is projected to WGS84 (EPSG:4326)
  if (is.na(sf::st_crs(areaSf))) {
    warning("`area` has no CRS assigned. Assuming WGS84 (EPSG:4326).", call. = FALSE)
    areaSf <- sf::st_set_crs(areaSf, 4326)
  } else if (sf::st_crs(areaSf) != sf::st_crs(4326)) {
    areaSf <- sf::st_transform(areaSf, 4326)
  }

  # -------------------------------------------------------------------------
  # 3. Query USGS NWIS Site Web Service by Bounding Box
  # -------------------------------------------------------------------------
  bb <- sf::st_bbox(areaSf)
  # USGS NWIS expects: bBox=min_lon,min_lat,max_lon,max_lat (7 decimal places max)
  bboxParam <- sprintf("%.6f,%.6f,%.6f,%.6f", bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])

  usgsUrl <- "https://waterservices.usgs.gov/nwis/site/"
  queryParams <- list(
    format = "rdb",
    bBox = bboxParam,
    seriesCatalogOutput = "true",
    siteStatus = "all"
  )

  if (isTRUE(verbose)) {
    message("Querying USGS NWIS Site Web Service for area bounding box...")
  }

  usgsRaw <- fetchUsgsRdb(usgsUrl, queryParams, timeout = timeout)

  if (nrow(usgsRaw) == 0) {
    if (isTRUE(verbose)) message("No USGS stations found in the bounding box.")
    return(character(0))
  }

  # Clean and filter coordinates
  usgsClean <- usgsRaw[
    !is.na(usgsRaw$dec_lat_va) & !is.na(usgsRaw$dec_long_va) &
      nzchar(usgsRaw$dec_lat_va) & nzchar(usgsRaw$dec_long_va),
  ]

  if (nrow(usgsClean) == 0) {
    if (isTRUE(verbose)) message("No USGS stations with valid coordinates found.")
    return(character(0))
  }

  usgsClean$latitude <- as.numeric(usgsClean$dec_lat_va)
  usgsClean$longitude <- as.numeric(usgsClean$dec_long_va)

  # Filter coordinate sanity
  usgsClean <- usgsClean[
    abs(usgsClean$latitude) <= 90 & abs(usgsClean$longitude) <= 180,
  ]

  # -------------------------------------------------------------------------
  # 4. Spatial Intersect: Clip from BBox to Exact Input Area
  # -------------------------------------------------------------------------
  sitesUnique <- usgsClean[!duplicated(usgsClean$site_no), c("site_no", "station_nm", "latitude", "longitude")]
  sitesSf <- sf::st_as_sf(sitesUnique, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  unionArea <- sf::st_union(areaSf)
  insideIdx <- which(lengths(sf::st_intersects(sitesSf, unionArea)) > 0)

  if (length(insideIdx) == 0) {
    if (isTRUE(verbose)) message("No USGS stations found within the exact specified boundary.")
    return(character(0))
  }

  sitesInArea <- sitesSf[insideIdx, ]
  siteNumbersInArea <- sitesInArea$site_no
  areaCatalog <- usgsClean[usgsClean$site_no %in% siteNumbersInArea, ]

  # -------------------------------------------------------------------------
  # 5. Self-Directing Guidance if Parameter/Variable is Omitted
  # -------------------------------------------------------------------------
  isAllRequested <- (!is.null(parameterCd) && identical(parameterCd, "all")) ||
    (!is.null(variable) && identical(variable, "all"))

  if (is.null(parameterCd) && is.null(variable) && !isAllRequested) {
    # Summarize parameters present in the boundary
    parmSummaryList <- lapply(split(areaCatalog, areaCatalog$parm_cd), function(df) {
      cd <- df$parm_cd[1]
      if (is.na(cd) || !nzchar(cd)) return(NULL)
      desc <- if (cd %in% names(commonParmMap)) commonParmMap[[cd]] else "Other / Project Specific"
      data.frame(
        parameterCd = cd,
        description = desc,
        stationCount = length(unique(df$site_no)),
        beginDate = if (any(nzchar(df$begin_date))) min(df$begin_date[nzchar(df$begin_date)], na.rm = TRUE) else NA_character_,
        endDate = if (any(nzchar(df$end_date))) max(df$end_date[nzchar(df$end_date)], na.rm = TRUE) else NA_character_,
        stringsAsFactors = FALSE
      )
    })

    parmSummary <- do.call(rbind, parmSummaryList)
    if (!is.null(parmSummary) && nrow(parmSummary) > 0) {
      parmSummary <- parmSummary[order(-parmSummary$stationCount, parmSummary$parameterCd), ]
      rownames(parmSummary) <- NULL
    } else {
      parmSummary <- data.frame()
    }

    if (isTRUE(verbose)) {
      cat(sprintf("Found %d USGS station(s) within the specified area.\n", length(siteNumbersInArea)))
      cat("No `parameterCd` or `variable` specified. Showing available parameters in this area:\n\n")
      print(parmSummary, row.names = FALSE)
      message("\nNext steps:")
      message("  - To filter: provide `parameterCd = '<5-digit code>'` (e.g. parameterCd = '00010')")
      message("  - Or provide: `variable = '<temp|flow|stage|ec|turbidity|do>'`")
      message("  - To return all stations in this area: specify `parameterCd = 'all'`")
      message("  - Set `map = TRUE` to view interactively.")
    }
    return(invisible(parmSummary))
  }

  # -------------------------------------------------------------------------
  # 6. Filter Stations by Parameter Code or Variable Alias
  # -------------------------------------------------------------------------
  if (isAllRequested) {
    matchedSites <- siteNumbersInArea
  } else {
    # Resolve target parameter code
    if (!is.null(parameterCd)) {
      targetCd <- sprintf("%05s", as.character(parameterCd))
    } else {
      varLookup <- list(
        temp = "00010",
        temperature = "00010",
        flow = "00060",
        discharge = "00060",
        stage = "00065",
        height = "00065",
        ec = "00095",
        cond = "00095",
        conductance = "00095",
        turbidity = "63680",
        do = "00300"
      )
      varKey <- tolower(trimws(variable))
      if (!varKey %in% names(varLookup)) {
        stop(sprintf("Unknown `variable` '%s'. Supported: %s, or supply 5-digit `parameterCd` directly.",
                     variable, paste(unique(names(varLookup)), collapse = ", ")), call. = FALSE)
      }
      targetCd <- varLookup[[varKey]]
    }

    matchedRows <- areaCatalog[areaCatalog$parm_cd %in% targetCd, ]
    matchedSites <- unique(matchedRows$site_no)
  }

  finalSitesSf <- sitesInArea[sitesInArea$site_no %in% matchedSites, ]

  # -------------------------------------------------------------------------
  # 7. Process Additional Comparative Station(s)
  # -------------------------------------------------------------------------
  addedSitesSf <- sitesInArea[0, ] # Empty template
  if (!is.null(addStations) && length(addStations) > 0) {
    addStations <- unique(as.character(addStations))

    if (isTRUE(verbose) && map) {
      message("Fetching metadata for added comparison stations...")
    }

    addQuery <- list(
      format = "rdb",
      sites = paste(addStations, collapse = ","),
      seriesCatalogOutput = "true",
      siteStatus = "all"
    )
    addRaw <- fetchUsgsRdb(usgsUrl, addQuery, timeout = timeout)

    if (nrow(addRaw) > 0) {
      addRaw$latitude <- as.numeric(addRaw$dec_lat_va)
      addRaw$longitude <- as.numeric(addRaw$dec_long_va)
      addClean <- addRaw[!is.na(addRaw$latitude) & !is.na(addRaw$longitude) &
                           abs(addRaw$latitude) <= 90 & abs(addRaw$longitude) <= 180, ]

      # Append to catalog so popups can format added stations
      areaCatalog <- rbind(areaCatalog, addClean)

      addUnique <- addClean[!duplicated(addClean$site_no), c("site_no", "station_nm", "latitude", "longitude")]
      if (nrow(addUnique) > 0) {
        addedSitesSf <- sf::st_as_sf(addUnique, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
        # Remove any overlap with area stations so added stations are styled distinctly
        finalSitesSf <- finalSitesSf[!finalSitesSf$site_no %in% addedSitesSf$site_no, ]
      }
    }

    missingAdd <- setdiff(addStations, unique(addRaw$site_no))
    if (length(missingAdd) > 0) {
      warning("The following `addStations` could not be found on USGS NWIS: ",
              paste(sQuote(missingAdd), collapse = ", "), call. = FALSE)
    }
  }

  # Guard check if both final area stations and added stations are empty
  if (nrow(finalSitesSf) == 0 && nrow(addedSitesSf) == 0) {
    if (isTRUE(verbose)) message("No matching USGS stations found within the specified area.")
    return(character(0))
  }

  if (isTRUE(verbose) && !map) {
    if (nrow(finalSitesSf) > 0) {
      message(sprintf("Found %d matching USGS station(s) in area: %s",
                      nrow(finalSitesSf),
                      paste(finalSitesSf$site_no, collapse = ", ")))
    } else {
      message("Found 0 matching USGS stations in area.")
    }
    if (!is.null(addStations)) {
      message("Note: `addStations` is only plotted when `map = TRUE`.")
    }
  }

  # -------------------------------------------------------------------------
  # 8. Return Site Numbers or Leaflet Map
  # -------------------------------------------------------------------------
  if (!isTRUE(map)) {
    return(finalSitesSf$site_no)
  }

  # Helper function to generate clean USGS popup HTML
  generateUsgsPopupHtml <- function(stnNo, stnName, isAdded = FALSE) {
    stnMeta <- areaCatalog[areaCatalog$site_no == stnNo, ]
    parms <- unique(stnMeta$parm_cd[nzchar(stnMeta$parm_cd)])

    if (length(parms) > 0) {
      parmLines <- vapply(parms, function(p) {
        desc <- if (p %in% names(commonParmMap)) commonParmMap[[p]] else "Other"
        pRows <- stnMeta[stnMeta$parm_cd == p, ]
        bDate <- min(pRows$begin_date[nzchar(pRows$begin_date)], na.rm = TRUE)
        eDate <- max(pRows$end_date[nzchar(pRows$end_date)], na.rm = TRUE)
        dateStr <- if (is.finite(suppressWarnings(as.Date(bDate)))) sprintf(" (<i>%s to %s</i>)", bDate, eDate) else ""
        sprintf("<li><b>#%s:</b> %s%s</li>", p, desc, dateStr)
      }, character(1))

      parmListHtml <- paste0("<ul style='max-height: 140px; overflow-y: auto; padding-left: 18px; margin: 4px 0;'>",
                             paste(parmLines, collapse = ""),
                             "</ul>")
    } else {
      parmListHtml <- "<em>No parameter series catalog recorded</em>"
    }

    headerColor <- if (isAdded) "#e6550d" else "#006d2c"
    subtitle <- if (isAdded) "<span style='color: #e6550d; font-weight: bold;'>[Added Comparison Station]</span><br>" else ""
    latVal <- stnMeta$latitude[1]
    lonVal <- stnMeta$longitude[1]

    sprintf(
      paste0(
        "<div style='font-family: sans-serif; font-size: 12px; line-height: 1.4;'>",
        "<h4 style='margin: 0 0 2px 0; color: %s;'>USGS %s</h4>",
        "<b>%s</b><br>",
        "%s",
        "<b>Lat:</b> %.4f | <b>Lon:</b> %.4f<br>",
        "<b>Available Parameters:</b>%s",
        "<a href='https://waterdata.usgs.gov/monitoring-location/%s/' target='_blank'>View on USGS Dashboard &rarr;</a>",
        "</div>"
      ),
      headerColor,
      stnNo,
      stnName,
      subtitle,
      latVal,
      lonVal,
      parmListHtml,
      stnNo
    )
  }

  # Build Leaflet Map
  m <- leaflet::leaflet()

  # Clean muted gray canvas (default, free, no API key)
  m <- leaflet::addProviderTiles(
    m,
    provider = leaflet::providers$Esri.WorldGrayCanvas,
    group = "Gray Canvas"
  )
  m <- leaflet::addTiles(
    m,
    group = "OpenStreetMap"
  )
  m <- leaflet::addProviderTiles(
    m,
    provider = leaflet::providers$Esri.WorldImagery,
    group = "Satellite"
  )

  # Add search boundary polygon
  m <- leaflet::addPolygons(
    m,
    data = areaSf,
    color = "#2b8cbe",
    weight = 2,
    fillOpacity = 0.08,
    group = "Search Boundary"
  )

  activeOverlays <- c("Search Boundary")

  # Add stations inside area (Forest Green)
  if (nrow(finalSitesSf) > 0) {
    areaPopups <- mapply(generateUsgsPopupHtml,
                         stnNo = finalSitesSf$site_no,
                         stnName = finalSitesSf$station_nm,
                         MoreArgs = list(isAdded = FALSE),
                         SIMPLIFY = TRUE, USE.NAMES = FALSE)

    labels <- sprintf("USGS %s: %s", finalSitesSf$site_no, finalSitesSf$station_nm)

    m <- leaflet::addCircleMarkers(
      m,
      data = finalSitesSf,
      lng = ~longitude,
      lat = ~latitude,
      radius = 6,
      color = "#00441b",
      weight = 1.5,
      fillColor = "#31a354",
      fillOpacity = 0.85,
      label = unname(labels),
      popup = unname(areaPopups),
      group = "USGS Stations (Area)"
    )
    activeOverlays <- c(activeOverlays, "USGS Stations (Area)")
  }

  # Add comparison stations (Orange/Vermilion)
  if (nrow(addedSitesSf) > 0) {
    addedPopups <- mapply(generateUsgsPopupHtml,
                          stnNo = addedSitesSf$site_no,
                          stnName = addedSitesSf$station_nm,
                          MoreArgs = list(isAdded = TRUE),
                          SIMPLIFY = TRUE, USE.NAMES = FALSE)

    labelsAdded <- sprintf("USGS %s: %s (Added)", addedSitesSf$site_no, addedSitesSf$station_nm)

    m <- leaflet::addCircleMarkers(
      m,
      data = addedSitesSf,
      lng = ~longitude,
      lat = ~latitude,
      radius = 7,
      color = "#a63603",
      weight = 2,
      fillColor = "#e6550d",
      fillOpacity = 0.90,
      label = unname(labelsAdded),
      popup = unname(addedPopups),
      group = "Added Stations"
    )
    activeOverlays <- c(activeOverlays, "Added Stations")
  }

  # Add legend if comparison stations are present
  if (nrow(finalSitesSf) > 0 && nrow(addedSitesSf) > 0) {
    m <- leaflet::addLegend(
      m,
      position = "bottomright",
      colors = c("#31a354", "#e6550d"),
      labels = c("USGS Sites in Area", "Added Station(s)"),
      opacity = 0.9,
      title = "Station Type"
    )
  }

  m <- leaflet::addLayersControl(
    m,
    baseGroups = c("Gray Canvas", "OpenStreetMap", "Satellite"),
    overlayGroups = activeOverlays,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

  m
}

#' Fetch and parse USGS RDB text output
#'
#' @description
#' **Developed with Gemini 3.8 Flash**
#'
#' @param url Base URL string
#' @param query List of query parameters
#' @param timeout Max seconds to wait
#'
#' @return A data.frame with character columns
#' @noRd
#' @keywords internal
fetchUsgsRdb <- function(url, query, timeout = 60) {
  resp <- tryCatch({
    httr::GET(url, query = query, httr::timeout(timeout))
  }, error = function(e) {
    stop("Failed to connect to USGS NWIS services: ", conditionMessage(e), call. = FALSE)
  })

  if (httr::http_status(resp)$category != "Success") {
    stop(sprintf("USGS NWIS request failed (%s): %s",
                 httr::http_status(resp)$reason, httr::content(resp, "text", encoding = "UTF-8")),
         call. = FALSE)
  }

  rawText <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (!nzchar(trimws(rawText))) return(data.frame())

  lines <- strsplit(rawText, "\r?\n")[[1]]
  # Drop comment lines
  lines <- lines[!grepl("^#", lines)]

  # RDB must have at least 2 non-comment lines (header and format definition line)
  if (length(lines) < 2) return(data.frame())

  header <- strsplit(lines[1], "\t")[[1]]

  # If only header and format line exist, return empty data.frame matching headers
  if (length(lines) == 2) {
    emptyDf <- as.data.frame(
      setNames(replicate(length(header), character(0), simplify = FALSE), header),
      stringsAsFactors = FALSE
    )
    return(emptyDf)
  }

  # Data rows start at line 3 (line 2 is column widths/formats like 5s 15s ...)
  dataLines <- lines[3:length(lines)]
  dataLines <- dataLines[nzchar(trimws(dataLines))]

  if (length(dataLines) == 0) {
    emptyDf <- as.data.frame(
      setNames(replicate(length(header), character(0), simplify = FALSE), header),
      stringsAsFactors = FALSE
    )
    return(emptyDf)
  }

  # Parse via read.delim
  df <- utils::read.delim(
    text = paste(c(lines[1], dataLines), collapse = "\n"),
    header = TRUE,
    sep = "\t",
    colClasses = "character",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  df
}

#' Pull USGS NWIS station data with automated caching
#'
#' @description
#' Retrieves instantaneous (sub-daily) or daily time-series data directly from
#' the USGS National Water Information System (NWIS) REST API using JSON payloads.
#' Supports multiple stations, multiple parameter codes, and multiple sampling
#' durations in a single call. If parameter or start date arguments are omitted,
#' the function inspects the data catalog and prints an aggregated parameter
#' coverage matrix to the console. Requests exceeding USGS server thresholds are
#' automatically paginated, paced, and optionally saved to a persistent disk
#' cache directory for instant reloading and crash recovery.
#'
#' **Developed with Gemini 3.8 Flash**
#'
#' @param site A character vector of one or more USGS site numbers (e.g. \code{"11455420"}).
#' @param parameterCd A vector of 5-digit USGS parameter codes (e.g. \code{"00060"}
#'   for discharge, \code{"00010"} for temperature). Takes precedence over \code{variable}.
#' @param duration Sampling duration/frequency: \code{"iv"} (or \code{"uv"}/\code{"event"}/\code{"instantaneous"})
#'   for sub-daily instantaneous data, and/or \code{"dv"} (or \code{"daily"}) for daily
#'   summary data. Can specify multiple (e.g. \code{c("dv", "iv")}) to retrieve both
#'   daily and instantaneous series in the same call. Defaults to \code{"iv"}.
#' @param service Alias for \code{duration} to maintain compatibility with USGS terminology.
#' @param dateStart Beginning date for the period of interest. Accepts standard date
#'   formats (\code{"YYYY-MM-DD"} or \code{"MM/DD/YYYY"}).
#' @param dateEnd Ending date for the period of interest. Defaults to current date (\code{Sys.Date()}).
#' @param variable Common parameter alias(es): \code{"temp"} (or \code{"temperature"}),
#'   \code{"flow"} (or \code{"discharge"}), \code{"stage"} (or \code{"height"}),
#'   \code{"ec"} (or \code{"cond"}), \code{"turbidity"}, or \code{"do"}. Can be multiple.
#'   Ignored if \code{parameterCd} is supplied.
#' @param temperatureUnits Either \code{"C"} (default) or \code{"F"} to convert
#'   water temperature. Only applicable to temperature data (\code{00010}).
#' @param statCd Statistic code for daily values (\code{"dv"}/\code{"daily"}).
#'   Defaults to \code{"00003"} (daily mean). Set to \code{NULL} to pull all available statistics.
#' @param allowSparse Logical. If \code{FALSE} (default), protects users by blocking
#'   simultaneous multi-site AND multi-parameter queries, which frequently result in
#'   unintended sparse/incomplete matrices. Set to \code{TRUE} to opt in to sparse downloads.
#' @param chunkDays Numeric. Maximum number of days to request per API call. If
#'   \code{NULL} (default), dynamically calculated based on the number of sites and
#'   parameters to respect USGS query limits. Set to \code{Inf} to disable pagination.
#' @param pause Numeric. Number of seconds to pause between pagination requests
#'   to prevent triggering server rate-limits. Defaults to 0.5 seconds.
#' @param cacheDir Optional character string specifying a folder path to save
#'   downloaded chunks as RDS files. If specified, previously cached chunks will be
#'   loaded from disk rather than re-downloaded, allowing instant resumes if interrupted.
#' @param maxAttempt Numeric. Number of retry attempts per chunk when encountering
#'   transient server errors. Defaults to 3.
#' @param verbose Logical. Should the function print status updates and metadata?
#'   Defaults to \code{TRUE}.
#' @param timeout Max duration in seconds to wait for download per attempt. Defaults to 120.
#'
#' @return A tidy \code{data.frame} of the requested USGS time-series data. If
#'   called without parameters or dates, returns an invisible metadata table of
#'   available parameters.
#'
#' @export
#'
#' @importFrom httr GET http_status content timeout
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.delim
#'
#' @examples
#' \dontrun{
#' # 1. Multi-site metadata discovery matrix:
#' meta <- pullUSGS(c("11455095", "11455136", "11455420"))
#'
#' # 2. Downloading multi-year data with disk caching:
#' usgsData <- pullUSGS(site = c("11455095", "11455136"),
#'                      variable = "flow", duration = "iv",
#'                      dateStart = "2015-01-01", dateEnd = "2023-01-01",
#'                      cacheDir = "data/usgs_cache")
#' }
pullUSGS <- function(site,
                     parameterCd = NULL,
                     duration = c("iv", "dv", "uv", "event", "daily", "instantaneous"),
                     service = NULL,
                     dateStart,
                     dateEnd = NULL,
                     variable = NULL,
                     temperatureUnits = c("C", "F"),
                     statCd = "00003",
                     allowSparse = FALSE,
                     chunkDays = NULL,
                     pause = 0.5,
                     cacheDir = NULL,
                     maxAttempt = 3,
                     verbose = TRUE,
                     timeout = 120) {

  commonParmMap <- c(
    "00010" = "Water Temperature",
    "00060" = "Discharge (Flow)",
    "00065" = "Gage Height (Stage)",
    "00095" = "Specific Conductance",
    "63680" = "Turbidity",
    "00300" = "Dissolved Oxygen",
    "00400" = "pH"
  )

  commonUnitsMap <- c(
    "00010" = "DEG C",
    "00060" = "cfs",
    "00065" = "ft",
    "00095" = "\u00b5S/cm",
    "63680" = "FNU",
    "00300" = "mg/L",
    "00400" = "std units"
  )

  site <- unique(as.character(site))

  # -------------------------------------------------------------------------
  # 1. Metadata Inspection / Self-Directing Guidance
  # -------------------------------------------------------------------------
  isMetadataQuery <- (is.null(parameterCd) && is.null(variable)) || missing(dateStart)

  if (isMetadataQuery) {
    siteUrl <- "https://waterservices.usgs.gov/nwis/site/"
    siteQuery <- list(
      format = "rdb",
      sites = paste(site, collapse = ","),
      seriesCatalogOutput = "true",
      siteStatus = "all"
    )

    if (isTRUE(verbose)) {
      message(sprintf("Retrieving metadata catalog for %d USGS site(s)...", length(site)))
    }

    catRaw <- fetchUsgsRdb(siteUrl, siteQuery, timeout = timeout)

    if (nrow(catRaw) == 0) {
      stop(sprintf("No metadata returned for requested site(s): %s", paste(site, collapse = ", ")), call. = FALSE)
    }

    catClean <- catRaw[nzchar(catRaw$parm_cd) & !is.na(catRaw$parm_cd), ]

    if (nrow(catClean) == 0) {
      stop(sprintf("No time-series parameters found for site(s): %s", paste(site, collapse = ", ")), call. = FALSE)
    }

    catClean$durClean <- vapply(tolower(catClean$data_type_cd), function(d) {
      switch(d,
             "uv" = "iv",
             "iv" = "iv",
             "dv" = "dv",
             "qw" = "qw",
             d)
    }, character(1))

    catalogGroups <- split(catClean, list(catClean$parm_cd, catClean$durClean), drop = TRUE)

    summaryRows <- vector("list", length(catalogGroups))
    siteVectors <- vector("list", length(catalogGroups))

    for (k in seq_along(catalogGroups)) {
      df <- catalogGroups[[k]]
      cd <- df$parm_cd[1]
      desc <- if (cd %in% names(commonParmMap)) commonParmMap[[cd]] else "Other / Project Parameter"
      stnsWithData <- sort(unique(df$site_no))
      nWithData <- length(stnsWithData)
      nTotal <- length(site)
      propVal <- round(nWithData / nTotal, 2)

      coverageStr <- if (nTotal == 1) {
        site
      } else if (nWithData == nTotal) {
        sprintf("All %d sites", nTotal)
      } else if (nWithData <= 3) {
        sprintf("%s (%d/%d)", paste(stnsWithData, collapse = ", "), nWithData, nTotal)
      } else {
        sprintf("%s, ... (%d/%d)", paste(stnsWithData[1:2], collapse = ", "), nWithData, nTotal)
      }

      summaryRows[[k]] <- data.frame(
        parameterCd = cd,
        duration = df$durClean[1],
        description = desc,
        proportion = propVal,
        siteCoverage = coverageStr,
        beginDate = min(df$begin_date[nzchar(df$begin_date)], na.rm = TRUE),
        endDate = max(df$end_date[nzchar(df$end_date)], na.rm = TRUE),
        totalRecords = sum(as.numeric(df$count_nu), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      siteVectors[[k]] <- stnsWithData
    }

    coverageMatrix <- do.call(rbind, summaryRows)
    orderIdx <- order(as.integer(coverageMatrix$parameterCd), coverageMatrix$duration)

    coverageMatrix <- coverageMatrix[orderIdx, ]
    siteVectors <- siteVectors[orderIdx]
    rownames(coverageMatrix) <- NULL

    coverageMatrix$sites <- siteVectors

    if (isTRUE(verbose)) {
      if (length(site) == 1) {
        cat(sprintf("\nAvailable time-series data for USGS site %s:\n\n", site))
      } else {
        cat(sprintf("\nAvailable parameter coverage across %d requested USGS sites:\n\n", length(site)))
      }

      printCols <- setdiff(names(coverageMatrix), "sites")
      print(coverageMatrix[, printCols], row.names = FALSE)
      message("\nNotes:")
      message("  - 'iv': Instantaneous/Unit Value continuous sensor data (~15-min)")
      message("  - 'dv': Daily summarized data")
      message("  - 'qw': Periodic field/lab grab samples (not a continuous sensor)")
      message("\nNext steps:")
      message("  - Filter stations: `meta %>% filter(duration == 'iv') %>% pull(sites) %>% unlist()`")
      message("  - Provide `parameterCd` (e.g. parameterCd = c('00010', '00060'))")
      message("  - Or provide `variable` (e.g. variable = c('temp', 'flow'))")
      message("  - Specify `duration` ('iv' and/or 'dv') and `dateStart`.")
    }
    return(invisible(coverageMatrix))
  }

  # -------------------------------------------------------------------------
  # 2. Resolve Parameters & Dates
  # -------------------------------------------------------------------------
  temperatureUnits <- match.arg(temperatureUnits)

  if (!is.null(parameterCd)) {
    targetCds <- unique(sprintf("%05s", as.character(parameterCd)))
  } else {
    varLookup <- list(
      temp = "00010",
      temperature = "00010",
      flow = "00060",
      discharge = "00060",
      stage = "00065",
      height = "00065",
      ec = "00095",
      cond = "00095",
      conductance = "00095",
      turbidity = "63680",
      do = "00300",
      ph = "00400"
    )
    varKeys <- tolower(trimws(variable))
    unmatchedVars <- setdiff(varKeys, names(varLookup))
    if (length(unmatchedVars) > 0) {
      stop(sprintf("Unknown `variable`: %s. Supported: %s, or supply 5-digit `parameterCd` directly.",
                   paste(unmatchedVars, collapse = ", "),
                   paste(unique(names(varLookup)), collapse = ", ")), call. = FALSE)
    }
    targetCds <- unique(unname(unlist(varLookup[varKeys])))
  }

  # -------------------------------------------------------------------------
  # 3. Guard: Disallow Multi-Site AND Multi-Parameter unless allowSparse = TRUE
  # -------------------------------------------------------------------------
  if (length(site) > 1 && length(targetCds) > 1 && !isTRUE(allowSparse)) {
    stop(sprintf(
      paste0(
        "`pullUSGS()` requested %d sites across %d parameters.\n",
        "  In environmental sensor networks, stations rarely collect identical suites of sensors,\n",
        "  which often results in sparse/incomplete data matrices.\n\n",
        "  To proceed anyway, set `allowSparse = TRUE`.\n",
        "  Alternatively, query 1 station across multiple parameters, or 1 parameter across multiple stations."
      ),
      length(site), length(targetCds)
    ), call. = FALSE)
  }

  # -------------------------------------------------------------------------
  # 4. Resolve Durations / Services
  # -------------------------------------------------------------------------
  if (!is.null(service)) {
    duration <- service
  }

  if (missing(duration) && is.null(service)) {
    duration <- "iv"
  }

  durNorm <- tolower(trimws(duration))
  if (any(durNorm %in% "hourly")) {
    stop("USGS NWIS does not provide an 'hourly' service directly. Please choose 'iv' (or 'event' for ~15-min data) or 'dv' (or 'daily' for daily summaries).", call. = FALSE)
  }

  targetServices <- unique(vapply(durNorm, function(d) {
    switch(d,
           "iv" = "iv",
           "uv" = "iv",
           "event" = "iv",
           "instantaneous" = "iv",
           "dv" = "dv",
           "daily" = "dv",
           stop(sprintf("Unrecognized duration '%s'. Must be 'iv' ('event'/'uv') or 'dv' ('daily').", d), call. = FALSE))
  }, character(1)))

  siteLabel <- if (length(site) == 1) {
    sprintf("site %s", site)
  } else if (length(site) <= 3) {
    sprintf("%d sites (%s)", length(site), paste(site, collapse = ", "))
  } else {
    sprintf("%d sites (%s, ...)", length(site), paste(site[1:3], collapse = ", "))
  }

  parmLabel <- if (length(targetCds) == 1) {
    if (targetCds %in% names(commonParmMap)) sprintf("%s (%s)", targetCds, commonParmMap[[targetCds]]) else targetCds
  } else if (length(targetCds) <= 3) {
    paste(vapply(targetCds, function(cd) {
      if (cd %in% names(commonParmMap)) sprintf("%s (%s)", cd, commonParmMap[[cd]]) else cd
    }, character(1)), collapse = ", ")
  } else {
    sprintf("%d parameters (%s, ...)", length(targetCds), paste(targetCds[1:2], collapse = ", "))
  }

  dateStart <- parseDate(dateStart)
  originalEnd <- if (is.null(dateEnd)) Sys.Date() else parseDate(dateEnd)

  if (dateStart > originalEnd) {
    stop("`dateStart` cannot be after `dateEnd`.", call. = FALSE)
  }

  # -------------------------------------------------------------------------
  # 5. Disk Cache Setup
  # -------------------------------------------------------------------------
  useDiskCache <- !is.null(cacheDir)
  if (useDiskCache) {
    if (!dir.exists(cacheDir)) {
      dir.create(cacheDir, recursive = TRUE)
      if (isTRUE(verbose)) message(sprintf("Created cache directory: %s", cacheDir))
    }
    sitesSorted <- sort(site)
    siteFingerprint <- if (length(sitesSorted) == 1) {
      sitesSorted
    } else {
      sprintf("n%d_%s-%s", length(sitesSorted), sitesSorted[1], sitesSorted[length(sitesSorted)])
    }
    parmsSorted <- sort(targetCds)
    parmFingerprint <- paste0("p", paste(parmsSorted, collapse = "-"))
  }

  # -------------------------------------------------------------------------
  # 6. Service Ingest Loop
  # -------------------------------------------------------------------------
  allServiceResults <- list()
  unresolvedChunks <- list()

  for (serviceCode in targetServices) {

    currentChunkDays <- chunkDays
    if (is.null(currentChunkDays)) {
      if (serviceCode == "iv") {
        recordsPerDayEst <- length(site) * length(targetCds) * 96
        rawDays <- floor(40000 / recordsPerDayEst)
        currentChunkDays <- max(15, min(120, rawDays))
      } else {
        currentChunkDays <- 3650 # ~10 years for daily data
      }
    }

    totalDays <- as.numeric(difftime(originalEnd, dateStart, units = "days")) + 1
    needsPagination <- totalDays > currentChunkDays

    if (needsPagination) {
      numChunks <- ceiling(totalDays / currentChunkDays)
      if (isTRUE(verbose)) {
        message(sprintf(
          "Requested range (%d days for %s, parameter %s) exceeds safe %s query limits.\nPaginating %s request into ~%d chunks (chunk size: %d days)...",
          as.integer(totalDays), siteLabel, parmLabel, toupper(serviceCode), toupper(serviceCode), numChunks, as.integer(currentChunkDays)
        ))
      }
    } else {
      numChunks <- 1
    }

    dataUrl <- sprintf("https://waterservices.usgs.gov/nwis/%s/", serviceCode)
    currentStart <- dateStart
    chunkIdx <- 1
    serviceChunks <- list()

    while (currentStart <= originalEnd) {
      currentEnd <- min(currentStart + currentChunkDays - 1, originalEnd)

      # --- Cache Check ---
      cachedPath <- NULL
      if (useDiskCache) {
        chunkFileName <- sprintf("usgs_%s_%s_%s_%s_%s.rds",
                                 serviceCode, parmFingerprint, siteFingerprint,
                                 currentStart, currentEnd)
        cachedPath <- file.path(cacheDir, chunkFileName)

        if (file.exists(cachedPath)) {
          cachedDf <- tryCatch(readRDS(cachedPath), error = function(e) NULL)
          if (!is.null(cachedDf) && is.data.frame(cachedDf)) {
            if (isTRUE(verbose)) {
              if (needsPagination) {
                message(sprintf("  Fetching %s chunk %d/%d: loaded from cache (%s)",
                                toupper(serviceCode), chunkIdx, numChunks, basename(cachedPath)))
              } else {
                message(sprintf("Loading USGS %s data from cache (%s)...",
                                toupper(serviceCode), basename(cachedPath)))
              }
            }
            if (nrow(cachedDf) > 0) {
              serviceChunks[[length(serviceChunks) + 1]] <- cachedDf
            }
            currentStart <- currentEnd + 1
            chunkIdx <- chunkIdx + 1
            next
          }
        }
      }

      # --- Download Chunk ---
      if (isTRUE(verbose)) {
        if (needsPagination) {
          message(sprintf("  Fetching %s chunk %d/%d: %s to %s...", toupper(serviceCode), chunkIdx, numChunks, currentStart, currentEnd))
        } else {
          message(sprintf("Downloading USGS %s data from %s to %s for %s (parameter %s)...",
                          toupper(serviceCode), currentStart, currentEnd, siteLabel, parmLabel))
        }
      }

      queryParams <- list(
        format = "json",
        sites = paste(site, collapse = ","),
        parameterCd = paste(targetCds, collapse = ","),
        startDT = as.character(currentStart),
        endDT = as.character(currentEnd),
        siteStatus = "all"
      )

      if (serviceCode == "dv" && !is.null(statCd) && nzchar(statCd)) {
        queryParams$statCd <- paste(statCd, collapse = ",")
      }

      chunkDf <- tryCatch({
        fetchUsgsJson(dataUrl, queryParams, timeout = timeout, maxAttempt = maxAttempt, verbose = verbose)
      }, error = function(e) {
        unresolvedChunks[[length(unresolvedChunks) + 1]] <<- list(
          service = serviceCode, idx = chunkIdx, start = currentStart, end = currentEnd, err = conditionMessage(e)
        )
        NULL
      })

      # CRITICAL GUARD: Only assign duration and append if chunk has rows > 0
      if (!is.null(chunkDf) && is.data.frame(chunkDf)) {
        if (nrow(chunkDf) > 0) {
          chunkDf$duration <- serviceCode
          serviceChunks[[length(serviceChunks) + 1]] <- chunkDf
        }

        # Cache on disk even if 0 rows (so empty date ranges aren't repeatedly queried)
        if (useDiskCache && !is.null(cachedPath)) {
          tryCatch({
            saveRDS(chunkDf, file = cachedPath)
            if (isTRUE(verbose)) {
              message(sprintf("    Saved %s chunk %d to cache (%s).", toupper(serviceCode), chunkIdx, basename(cachedPath)))
            }
          }, error = function(e) NULL)
        }
      }

      currentStart <- currentEnd + 1
      chunkIdx <- chunkIdx + 1

      if (needsPagination && currentStart <= originalEnd && pause > 0) {
        Sys.sleep(pause)
      }
    }

    if (length(serviceChunks) > 0) {
      allServiceResults[[serviceCode]] <- do.call(rbind, serviceChunks)
    }
  }

  # -------------------------------------------------------------------------
  # 7. Detailed Failure Reporting
  # -------------------------------------------------------------------------
  if (length(unresolvedChunks) > 0) {
    unresolvedMsg <- vapply(unresolvedChunks, function(x) {
      cleanErr <- gsub("\\n", " ", x$err)
      sprintf("  [%s] Chunk %d: %s to %s (%s)", toupper(x$service), x$idx, x$start, x$end, cleanErr)
    }, character(1))

    warning(sprintf(
      "%d chunk(s) could not be retrieved from USGS for %s and parameter %s:\n%s",
      length(unresolvedChunks), siteLabel, parmLabel, paste(unresolvedMsg, collapse = "\n")
    ), call. = FALSE)
  }

  tidyDf <- do.call(rbind, allServiceResults)

  if (is.null(tidyDf) || !is.data.frame(tidyDf) || nrow(tidyDf) == 0) {
    msg <- sprintf("No data returned for %s and parameter %s across duration(s): %s.",
                   siteLabel, parmLabel, paste(targetServices, collapse = ", "))
    warning(msg, call. = FALSE)
    return(data.frame())
  }

  # -------------------------------------------------------------------------
  # 8. Formatting, Deduplication, & Sorting
  # -------------------------------------------------------------------------
  tidyDf$obsDate <- as.Date(substr(tidyDf$dateTime, 1, 10))

  isIv <- tidyDf$duration == "iv"
  formattedDateTimes <- as.POSIXct(rep(NA, nrow(tidyDf)), tz = "America/Los_Angeles")

  if (any(isIv)) {
    cleanDt <- sub("T", " ", substr(tidyDf$dateTime[isIv], 1, 19))
    formattedDateTimes[isIv] <- as.POSIXct(cleanDt, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")
  }
  if (any(!isIv)) {
    formattedDateTimes[!isIv] <- as.POSIXct(tidyDf$obsDate[!isIv], tz = "America/Los_Angeles")
  }

  tidyDf$dateTime <- formattedDateTimes

  tidyDf$parameterName <- ifelse(tidyDf$parameterCd %in% names(commonParmMap),
                                 commonParmMap[tidyDf$parameterCd],
                                 "Unknown Parameter")

  tidyDf$units <- ifelse(tidyDf$parameterCd %in% names(commonUnitsMap),
                         commonUnitsMap[tidyDf$parameterCd],
                         NA_character_)

  # Temperature conversion (°C -> °F) strictly scoped to 00010
  if (temperatureUnits == "F") {
    isTemp <- tidyDf$parameterCd == "00010"
    tidyDf$value[isTemp] <- (tidyDf$value[isTemp] * 9/5) + 32
    tidyDf$units[isTemp] <- "DEG F"
  }

  # Safe subsetting using which()
  validRows <- which(!is.na(tidyDf$obsDate) & tidyDf$obsDate >= dateStart & tidyDf$obsDate <= originalEnd)
  tidyDf <- tidyDf[validRows, ]

  # Deduplicate overlap at chunk boundaries using exact timestamps
  tidyDf <- tidyDf[!duplicated(tidyDf[c("siteNo", "parameterCd", "duration", "dateTime")]), ]

  # Sort numerically by parameterCd (lower numbers first), siteNo, duration, dateTime
  tidyDf <- tidyDf[order(as.integer(tidyDf$parameterCd), tidyDf$siteNo, tidyDf$duration, tidyDf$dateTime), ]
  rownames(tidyDf) <- NULL

  # -------------------------------------------------------------------------
  # 9. QOL Check: Report Missing Site-Parameter Combinations
  # -------------------------------------------------------------------------
  expectedPairs <- expand.grid(site = site, parameterCd = targetCds, stringsAsFactors = FALSE)
  actualPairs <- unique(tidyDf[, c("siteNo", "parameterCd")])
  names(actualPairs) <- c("site", "parameterCd")

  missingPairs <- subset(expectedPairs, !paste(site, parameterCd) %in% paste(actualPairs$site, actualPairs$parameterCd))

  if (nrow(missingPairs) > 0) {
    missingList <- split(missingPairs$site, missingPairs$parameterCd)
    missingMsgs <- vapply(names(missingList), function(cd) {
      desc <- if (cd %in% names(commonParmMap)) commonParmMap[[cd]] else cd
      sprintf("  - Parameter %s (%s): missing at site(s) %s", cd, desc, paste(missingList[[cd]], collapse = ", "))
    }, character(1))

    warning(sprintf(
      "Some requested site-parameter combinations returned no data:\n%s",
      paste(missingMsgs, collapse = "\n")
    ), call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message(sprintf(
      "Download complete: %s record(s) retrieved across %d site(s) and %d parameter(s).",
      format(nrow(tidyDf), big.mark = ","),
      length(unique(tidyDf$siteNo)),
      length(unique(tidyDf$parameterCd))
    ))
  }

  finalCols <- c("agencyCd", "siteNo", "duration", "dateTime", "obsDate",
                 "parameterCd", "parameterName", "value", "units", "dataFlag", "tz")

  tidyDf[, finalCols]
}

#' Fetch and parse USGS JSON output with robust type-checking
#'
#' @param url Base URL string
#' @param query List of query parameters
#' @param timeout Max seconds to wait per attempt
#' @param maxAttempt Number of retries for transient server errors
#' @param verbose Print retry notices
#'
#' @return A tidy data.frame
#' @noRd
#' @keywords internal
fetchUsgsJson <- function(url, query, timeout = 120, maxAttempt = 3, verbose = TRUE) {
  lastErr <- NULL

  for (attempt in seq_len(maxAttempt)) {
    resp <- tryCatch({
      httr::GET(url, query = query, httr::timeout(timeout))
    }, error = function(e) {
      lastErr <<- conditionMessage(e)
      NULL
    })

    if (!is.null(resp)) {
      status <- httr::status_code(resp)

      # Success (2xx)
      if (status >= 200 && status < 300) {
        rawText <- httr::content(resp, as = "text", encoding = "UTF-8")
        if (!nzchar(trimws(rawText))) return(data.frame())

        jsonObj <- tryCatch({
          jsonlite::fromJSON(rawText, simplifyDataFrame = TRUE)
        }, error = function(e) {
          lastErr <<- conditionMessage(e)
          NULL
        })

        if (is.null(jsonObj) || is.null(jsonObj$value$timeSeries)) {
          return(data.frame())
        }

        tsList <- jsonObj$value$timeSeries
        nSeries <- length(tsList$sourceInfo$siteName)
        if (nSeries == 0) return(data.frame())

        seriesDfs <- lapply(seq_len(nSeries), function(i) {
          # Safe site number extraction
          siteCodeObj <- tsList$sourceInfo$siteCode[[i]]
          stnNo <- if (is.data.frame(siteCodeObj) && "value" %in% names(siteCodeObj)) {
            siteCodeObj$value[1]
          } else if (is.list(siteCodeObj) && "value" %in% names(siteCodeObj)) {
            siteCodeObj$value[[1]]
          } else {
            NA_character_
          }

          agency <- if (is.data.frame(siteCodeObj) && "agencyCode" %in% names(siteCodeObj)) {
            siteCodeObj$agencyCode[1]
          } else {
            "USGS"
          }

          # Safe timezone extraction
          tzStr <- tryCatch({
            tsList$sourceInfo$timeZoneInfo$defaultTimeZone$zoneAbbreviation[i]
          }, error = function(e) NA_character_)
          if (is.null(tzStr) || length(tzStr) == 0 || is.na(tzStr)) {
            tzStr <- NA_character_
          }

          # Safe parameter code extraction
          varObj <- tsList$variable$variableCode[[i]]
          pCd <- if (is.data.frame(varObj) && "value" %in% names(varObj)) {
            varObj$value[1]
          } else if (is.list(varObj) && "value" %in% names(varObj)) {
            varObj$value[[1]]
          } else {
            NA_character_
          }

          # Safe value container extraction
          valWrapper <- tryCatch(tsList$values[[i]]$value, error = function(e) NULL)
          if (is.null(valWrapper) || length(valWrapper) == 0) return(NULL)

          valDf <- tryCatch(valWrapper[[1]], error = function(e) NULL)

          # CRITICAL GUARD: Ensure valDf is genuinely a data.frame with rows
          if (is.null(valDf) || !is.data.frame(valDf) || nrow(valDf) == 0) {
            return(NULL)
          }

          if (!all(c("dateTime", "value") %in% names(valDf))) {
            return(NULL)
          }

          # Safe qualifiers extraction
          flags <- if ("qualifiers" %in% names(valDf)) {
            vapply(valDf$qualifiers, function(q) {
              if (is.null(q) || length(q) == 0 || all(is.na(q))) "" else paste(q, collapse = ",")
            }, character(1))
          } else {
            NA_character_
          }

          data.frame(
            agencyCd = agency,
            siteNo = stnNo,
            dateTime = valDf$dateTime,
            tz = tzStr,
            parameterCd = pCd,
            value = suppressWarnings(as.numeric(valDf$value)),
            dataFlag = flags,
            stringsAsFactors = FALSE
          )
        })

        combined <- do.call(rbind, seriesDfs[!vapply(seriesDfs, is.null, logical(1))])
        if (is.null(combined)) return(data.frame())
        return(combined)
      }

      # Client errors (4xx) - fail fast
      if (status >= 400 && status < 500) {
        stop(sprintf("USGS NWIS request failed (%s): %s",
                     httr::http_status(resp)$reason,
                     httr::content(resp, "text", encoding = "UTF-8")), call. = FALSE)
      }

      # Server errors (5xx) - retry with exponential backoff
      if (status >= 500 && status < 600) {
        lastErr <- sprintf("HTTP %d (%s)", status, httr::http_status(resp)$reason)
        if (attempt < maxAttempt) {
          backoff <- 2 ^ attempt
          if (isTRUE(verbose)) {
            message(sprintf("    Server returned %s. Waiting %d seconds before retry %d/%d...",
                            lastErr, backoff, attempt + 1, maxAttempt))
          }
          Sys.sleep(backoff)
        }
      }
    } else {
      if (attempt < maxAttempt) Sys.sleep(2)
    }
  }

  stop(sprintf("USGS NWIS request failed after %d attempts: %s", maxAttempt, lastErr), call. = FALSE)
}




