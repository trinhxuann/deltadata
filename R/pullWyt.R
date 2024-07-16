#' Scrape water year type data from DWR
#'
#' @param url URL of the historical Water Supply Index (WSIHIST) website
#'
#' @return Returns a list of three tables, reconstructed wyt, eight river runoff, and official wyt classification.
#' \item{reconstructedWyt}{All information from the Sacramento and San Joaquin Valleys water year type indices table}
#' \item{eightRiver}{All information from the Eight River Runoff table}
#' \item{officialWyt}{Contains only the wyt index and classification for the Sacramento and San Joaquin River Valleys}
#'
#' @export
#'
#' @importFrom rvest session html_element html_text
#' @importFrom utils read.table
#'
#' @examples
#' \dontrun{
#' pullWyt()
#' }
pullWyt <- function(url = "https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST") {

  session <- rvest::session(url)

  tableElement <- rvest::html_element(session, xpath = '//*[@id="main-content"]/div/div/main/section/pre/text()')
  tableText <- rvest::html_text(tableElement)

  # Total WYT table
  wytTotal <- regmatches(tableText, regexpr("(?s)WY  Oct.*Eight River Runoff", tableText, perl = T))
  tableLinesTotal <- strsplit(wytTotal, "\r\n")[[1]]
  tableLinesTotal <- tableLinesTotal[!grepl("--", tableLinesTotal)]
  tableLinesTotal <- tableLinesTotal[1:(which(tableLinesTotal == "")[1] - 1)]
  wytTotalDF <- read.table(text = tableLinesTotal, header = TRUE,
                           fill = T, na.strings = c("NA", ""))
  # Fix first 5 years
  tableEarly <- wytTotalDF[wytTotalDF$WY %in% 1901:1905, ]
  tableLate <- wytTotalDF[wytTotalDF$WY > 1905, ]

  wytTotalDF <- merge(
    # Sac
    data.frame(
      waterYear = c(tableEarly$WY, tableLate$WY),
      sacOctMar = c(tableEarly$Oct.Mar.1, tableLate$Oct.Mar),
      sacAprJul = c(tableEarly$Apr.Jul.1, tableLate$Apr.Jul),
      sacWySum  = c(tableEarly$WYsum.1, tableLate$WYsum),
      sacIndex  = c(tableEarly$Index.1, tableLate$Index),
      sacYrType = c(tableEarly$Yr.type.1, tableLate$Yr.type)
    ),
    # SJR
    data.frame(
      waterYear = c(tableEarly$WY, tableLate$WY),
      sjrOctMar = c(tableEarly$Oct.Mar, tableLate$Oct.Mar.1),
      sjrAprJul = c(tableEarly$Apr.Jul, tableLate$Apr.Jul.1),
      sjrWySum  = c(tableEarly$WYsum, tableLate$WYsum.1),
      sjrIndex  = c(tableEarly$Index, tableLate$Index.1),
      sjrYrType = c(tableEarly$Yr.type, tableLate$Yr.type.1)
    ), by = "waterYear"
  )

  # Eight River Runoff
  eightRiver <- regmatches(tableText, regexpr("(?s)WY      Dec.*Official Year", tableText, perl = T))
  tableLinesEight <- strsplit(eightRiver, "\r\n")[[1]]
  tableLinesEight <- tableLinesEight[!grepl("--", tableLinesEight)]
  tableLinesEight <- tableLinesEight[1:(which(tableLinesEight == "")[1] - 1)]

  eightRiverDF <- read.table(text = tableLinesEight, header = TRUE,
                             fill = T, na.strings = c("NA", ""))
  names(eightRiverDF)[1] <- "waterYear"

  # Official wyt
  officialWyt <- regmatches(tableText, regexpr("(?s)Official Year Classifications.*Abbreviations:", tableText, perl = T))
  tableLinesOfficial <- strsplit(officialWyt, "\r\n")[[1]]
  tableLinesOfficial <- tableLinesOfficial[3:(which(tableLinesOfficial == "")[1] - 1)]

  officialWytDF <- read.table(text = tableLinesOfficial, header = TRUE,
                              fill = T, na.strings = c("NA", ""))
  names(officialWytDF) <- c("waterYear", "sacIndex", "sacWyt", "sjrIndex", "sjrWyt")

  return(list(
    reconstructedWyt = wytTotalDF,
    eightRiver = eightRiverDF,
    officialWyt = officialWytDF
  ))
}

#' Water year type of a date
#'
#' @param date A single or vector of dates, in date format
#' @param wyt A data frame of the official wyt from the \code{pullWyt} function
#' @param valley Which system, either "sac" or "sjr"
#' @param value What value to return, either "wyt" or "index"
#'
#' @return Returns water year type index or classification associated with the date(s)
#' @export
#'
#' @examples
#' \dontrun{
#' wytTable <- pullWyt()
#' randomDates <- sample(0:5000, 100, replace = TRUE) + as.Date("2010-01-01")
#' wytDate(randomDates, wyt = wytTable$officialWyt)
#' }
wytDate <- function(date, wyt,
                    valley = c("sac", "sjr"), value = c("wyt", "index")) {

  # Which valley and value are you interested in?
  valley <- match.arg(valley)
  value <- match.arg(value)

  # Identifying what you wanted
  column <- paste0(valley, ifelse(value == "index", "Index", "Wyt"))
  # Has to be a date in which water year has yet to be calculated
  waterYear <- as.numeric(format(date, "%Y")) + (as.numeric(format(date, "%m")) >= 10)

  # Vectorize into a vector
  lookup <- setNames(wyt[[column]], wyt$waterYear)
  lookup[as.character(waterYear)]
}
