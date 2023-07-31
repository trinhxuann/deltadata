#' Table of file names and the associated url from an EDI webpage
#'
#' @param url URL of the EDI package.
#' @param version Optional. Numeric value of the version number of interest.
#' Defaults to `newest`, which will pull the newest version.
#'
#' @return A table of file names, hash values, and associated url of all files
#' available in the data package.
#'
#' @noRd
#' @keywords internal
tableNamesEDI <- function(url, version = "newest") {

  packageID <- regmatches(url, regexpr("(?<=edi\\.)\\d+", url, perl = TRUE))
  versionNumber <- regmatches(url, regexpr(paste0("(?<=edi\\.\\d{", nchar(packageID), "}\\.)\\d+"),
                                           url, perl = TRUE))

  currentNewestVersion <- max(read.table(paste0("https://pasta.lternet.edu/package/eml/edi/", packageID))[1])

  if (as.numeric(versionNumber) != currentNewestVersion & version == "newest") {
    warning("Your current version is ", versionNumber, " but the newest version available is ", currentNewestVersion,
            call. = F)
  }

  tableLinks <- read.table(paste0("https://pasta.lternet.edu/package/eml/edi/", packageID, "/", version), header = F)[[1]]
  tableLinks <- tableLinks[which(grepl("/data/", tableLinks))]

  what <- read.csv(tableLinks[[4]])

  tableNames <- lapply(tableLinks, function(x) {
    entityName <- readLines(gsub("data", "name", x), warn = F)

    data.frame(id = gsub(".*\\/", "", x),
               name = entityName,
               url = x)
  })

  do.call(rbind, tableNames)
}

#' Pull files from an EDI package
#'
#' @param url URL of the EDI package
#' @param files Vector of file names of interest. Should match exactly what
#' is on the website. Leave this blank to see the options.
#' @param version Version of interest for the package at hand. Defaults to
#' `newest`, which pulls data from the newest version.
#'
#' @return A list of the data files requested. If it is a CSV, this will be
#' read directly into R via the `read.csv()` function. If any other file types,
#' the file will be downloaded and the file path will be provided as an
#' output for that element.
#' @export
#'
#' @examples
#' \donttest {
#' getEDI("https://portal.edirepository.org/nis/mapbrowse?packageid=edi.534.7",
#' files = c("Catch.csv", "SLSTables", "SLS_Metadata"))
#' }
getEDI <- function(url, files, version = "newest") {

  tables <- tableNamesEDI(url, version = version)

  if (missing(files))  {
    cat("Specify files to download: \n")
    return(print(tables))
  }

  tables <- subset(tables, name %in% files)

  # For files that are csv, read them in directly
  # For files that are NOT csvs, download them to the temp dir and provide the users with the file path?
  downloadedFiles <- lapply(tables[["name"]], function(x) {
    url <- subset(tables, name == x)[["url"]]

    if (grepl("\\.csv$", x)) {
      read.csv(url)
    } else {
      filePath <- file.path(tempdir(), x)
      if (!file.exists(filePath)) {
        downloaded <- download.file(url, destfile = filePath, mode = "wb")
        if (downloaded == 0) {
          filePath
        } else {
          stop("File download failed for: ", x, call. = F)
        }
      } else {
        filePath
      }
    }
  })

  setNames(downloadedFiles, files)
}

