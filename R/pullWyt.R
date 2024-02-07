#' Scrape water year type data from DWR
#'
#' @param url URL of the historical Water Supply Index (WSIHIST) website
#'
#' @return A list of three tables, reconstructed wyt, eight river runoff, and official wyt classification
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

  tableElement <- rvest::html_element(session, xpath = '//*[@id="main-content"]/div/div[2]/main/section/pre/text()')
  tableText <- rvest::html_text(tableElement)

  # Total WYT table
  wytTotal <- regmatches(tableText, regexpr("(?s)WY  Oct.*Eight River Runoff", tableText, perl = T))
  tableLinesTotal <- strsplit(wytTotal, "\r\n")[[1]]
  tableLinesTotal <- tableLinesTotal[!grepl("--", tableLinesTotal)]
  tableLinesTotal <- tableLinesTotal[1:(which(tableLinesTotal == "")[1] - 1)]
  wytTotalDF <- read.table(text = paste(tableLinesTotal, collapse = "\n"), header = TRUE,
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

  eightRiverDF <- read.table(text = paste(tableLinesEight, collapse = "\n"), header = TRUE,
                             fill = T, na.strings = c("NA", ""))
  names(eightRiverDF)[1] <- "waterYear"

  # Official wyt
  officialWyt <- regmatches(tableText, regexpr("(?s)Official Year Classifications.*Abbreviations:", tableText, perl = T))
  tableLinesOfficial <- strsplit(officialWyt, "\r\n")[[1]]
  tableLinesOfficial <- tableLinesOfficial[3:(which(tableLinesOfficial == "")[1] - 1)]

  officialWytDF <- read.table(text = paste(tableLinesOfficial, collapse = "\n"), header = TRUE,
                              fill = T, na.strings = c("NA", ""))
  names(officialWytDF) <- c("waterYear", "sacIndex", "sacWyt", "sjrIndex", "sjrWyt")

  return(list(
    reconstructedWyt = wytTotalDF,
    eightRiver = eightRiverDF,
    officialWyt = officialWytDF
  ))
}

