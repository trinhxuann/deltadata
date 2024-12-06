#' Parse the scope, identifier, and revision number from a valid EDI url
#'
#' @description
#' EDI urls come in two main forms when displaying the package ID. This function attempts to
#' parse those forms into a more consistent form.
#'
#' @param url A valid EDI url
#'
#' @return A 1x4 table of the scope, identifier, revision, and package ID of the EDI package
#'
#' @noRd
#' @keywords internal
parseEDI <- function(url) {

  # Check if it is the first form with scope, identifier, and revision
  if (grepl("scope=|identifier=|revision=", url)) {
    scope <- sub('.*scope=([^&]+).*', '\\1', url)
    identifier <- sub('.*identifier=([^&]+).*', '\\1', url)

    # Check if revision is present
    if (grepl("revision=", url)) {
      revision <- sub('.*revision=([^&]+).*', '\\1', url)
    } else {
      revision <- "1"
    }

    return(data.frame(scope = scope, identifier = identifier, revision = revision,
                      packageID = paste(scope, identifier, revision, sep = ".")))
  } else {
    # Extract values using the second form
    packageid <- sub('.*packageid=([^&]+).*', '\\1', url)
    parts <- strsplit(packageid, "\\.")[[1]]

    scope <- parts[1]
    identifier <- parts[2]
    revision <- parts[3]

    return(data.frame(scope = scope, identifier = identifier, revision = revision,
                      packageID = paste(scope, identifier, revision, sep = ".")))
  }
}

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
tableNamesEDI <- function(packageInfo, version = "newest") {

  currentNewestVersion <- max(
    suppressWarnings(utils::read.table(paste0("https://pasta.lternet.edu/package/eml/edi/", packageInfo$identifier))[1])
  )

  if (as.numeric(packageInfo$revision) != currentNewestVersion & version == "newest") {
    warning("Your current version is ", packageInfo$revision, " but the newest version available is ", currentNewestVersion, ". Pulling from the newest version, otherwise, specify the `version` argument.",
            call. = F)
  }

  tableLinks <- suppressWarnings(utils::read.table(paste0("https://pasta.lternet.edu/package/eml/edi/", packageInfo$identifier, "/", version), header = F)[[1]])
  tableLinks <- tableLinks[which(grepl("/data/", tableLinks))]

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
#' @description
#' This function works with the API provided by EDI to download the files of a package. The default behavior is to download the newest version of the data package, but this can be changed via the `version` argument. The function handles different file types differently, i.e., .csv files are read directly into R while all other file types are downloaded in the temporary folder and a file path to the file is returned. Bear in mind that the EDI servers are very slow and the execution of this code will be thus affected.
#'
#' @param url URL of the EDI package with the version number, i.e., the package URL you would access in the browser.
#' @param files Vector of file names of interest. Should match exactly what
#' is on the website. Leave this blank to see the options.
#' @param version Version of interest for the package at hand. Defaults to
#' `newest`, which pulls data from the newest version.
#'
#' @return A list of the data files requested. If it is a CSV, this will be
#' read directly into R via the `utils::read.csv()` function. If any other file types,
#' the file will be downloaded and the file path will be provided as an
#' output for that element.
#' @export
#'
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' getEDI("https://portal.edirepository.org/nis/mapbrowse?packageid=edi.534.8",
#' files = c("Catch.csv", "SLSTables.rds", "SLS_Metadata.pdf"))
#' }
getEDI <- function(url, files, version = "newest") {

  packageInfo <- parseEDI(url)

  tables <- tableNamesEDI(packageInfo, version = version)

  if (missing(files))  {
    cat("Specify files to download: \n")
    return(print(tables))
  }

  matchedTables <- files %in% tables$name

  if (!all(matchedTables)) {
    unmatchedNames <- files[!which(matchedTables)]
    print(tables)

    stop("The specified table(s) cannot be found in the EDI publication: ",
            paste(files[which(!matchedTables)], collapse = ", "),
            ". Please check your spelling.", call. = F)
  }

  tables <- tables[tables[["name"]] %in% files, ]

  # For files that are csv, read them in directly
  # For files that are NOT csvs, download them to the temp dir and provide the users with the file path?
  downloadedFiles <- lapply(tables[["name"]], function(x) {
    url <- (tables[tables[["name"]] == x, ])[["url"]]

    if (grepl("\\.csv$", x)) {
      utils::read.csv(url)
    } else {
      filePath <- file.path(tempdir(), x)
      if (!file.exists(filePath)) {
        downloaded <- utils::download.file(url, destfile = filePath, mode = "wb")
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

