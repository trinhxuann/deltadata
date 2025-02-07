pull_USGS <- function(site, dateStart, dateEnd = NULL,
                      timing = c("daily", "hourly"),
                      sensor = NULL) {

  # Do you have the two required packages?
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("zoo package is required", call. = FALSE)
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("readr package is required", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required", call. = FALSE)
  }

  site <- substitute(site)
  # # The dateStart date has to dateStart 14 days before the specified dateStart date.
  # # This is to account for NA padding during the rolling mean calculations.
  # # The 13 is correct here.
  # startPad <- as.Date(dateStart) - lubridate::days(13)

  availableData <-
    paste0("https://nwis.waterdata.usgs.gov/nwis/uv/?site_no=", site) %>%
    xml2::read_html() %>%
    rvest::html_table(header = T) %>%
    .[[1]] %>%
    rename("blank" = 1,
           "Available Start" = 3,
           "Available End" = 4) %>%
    select(where(~!all(is.na(.))))

  if (is.null(sensor) | length(timing) > 1) {
    print(availableData)
    return(message("Please provide sensor (parameter) # and/or timing. For ", site, ", the above data is available.
  Timing variables are: `daily` or `hourly`.
  Provide `dateStart` and `dateEnd` in mdy or ymd format."))
  }

  # If no dateEnd date is given, use today
  if (is.null(dateEnd)) {
    dateEnd <- lubridate::today()
  }

  # For hourly data, timing = "uv"; for daily it's "dv"
  if (!timing %in% c("daily", "hourly")) {
    stop("Timing variable should either be daily or hourly.")
  }

  if (timing %in% "daily") {
    timing <- "dv"
    columnNames <- c("agency", "site", "date", "variable", "qaqc")
  } else {
    if (timing %in% "hourly") {
      timing <- "uv"
      columnNames <- c("agency", "site", "date", "time","TZ", "variable", "qaqc")
    }
  }

  gageURL <- paste0("https://nwis.waterdata.usgs.gov/nwis/",
                    timing,
                    "?cb_",
                    sensor,
                    "=on&format=rdb&site_no=",
                    site,
                    "&referred_module=sw&period=&begin_date=",
                    dateStart,
                    "&end_date=",
                    dateEnd)
  siteName <- paste0("https://nwis.waterdata.usgs.gov/nwis/uv/?site_no=", site) %>%
    xml2::read_html() %>%
    html_node("h2") %>%
    html_text()

  gageDF <- read_table2(gageURL,
                        col_names = columnNames) %>%
    # Will this be OK? Need to test for more stations
    filter(agency %in% "USGS") %>%
    mutate(siteNamed = siteName)

  # if (isTRUE(roll)) {
  #   gageDF <- gageDF %>%
  #     mutate(varRoll5 = zoo::rollmean(variable, 5, na.pad = T, align = "right"),
  #            varRoll14 = zoo::rollmean(variable, 14, na.pad = T, align = "right"))
  # }

  # gageDF <- gageDF %>%
  #   # Filter out the padded dates
  #   filter(date >= dateStart)
  #
  # Changing name to correct label
  names(gageDF)[str_which(names(gageDF), "variable")] <- str_subset(availableData$`Available Parameters`, as.character(sensor)) %>%
    str_extract(pattern = "[:alpha:].*")

  print(gageURL)
  return(gageDF)
}

pull_USGS(site = 11313405)
