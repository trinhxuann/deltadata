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
    revision <- as.numeric(parts[3])

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

  baseUrl <- sprintf("https://pasta.lternet.edu/package")
  versionUrl <- sprintf("%s/eml/%s/%s", baseUrl, packageInfo$scope, packageInfo$identifier)

  currentNewestVersion <- max(
    suppressWarnings(utils::read.table(versionUrl)[1])
  )

  if (packageInfo$revision != currentNewestVersion & version == "newest") {
    warning("Your current version is ", packageInfo$revision, " but the newest version available is ", currentNewestVersion, ". Pulling from the newest version, otherwise, specify the `version` argument.",
            call. = F)
    version <- currentNewestVersion
  } else version <- packageInfo$revision

  entityUrl <- sprintf("%s/name/eml/%s/%s/%s", baseUrl, packageInfo$scope, packageInfo$identifier, version)

  parseEntityDf <- function(url) {
    lines <- readLines(url)
    parts <- strsplit(lines, ",")
    data.frame(
      id = sapply(parts, `[`, 1),
      name = sapply(parts, function(x) paste(x[-1], collapse = ","))
    )
  }

  entities <- parseEntityDf(entityUrl)
  # Get extension info
  getFormatType <- function(id) {
    rmdUrl <- sprintf("%s/data/rmd/eml/%s/%s/%s/%s",
                      baseUrl, packageInfo$scope, packageInfo$identifier, version, id)
    doc <- tryCatch({
      suppressMessages(XML::xmlParse(httr::GET(rmdUrl), encoding = "UTF-8"))
    }, error = function(e) {
      cat("Retrying...\n")
      Sys.sleep(2)  # Wait 2 seconds
      suppressMessages(XML::xmlParse(httr::GET(rmdUrl), encoding = "UTF-8"))  # Retry once
    })
    format <- XML::xpathSApply(doc, "//dataFormat", XML::xmlValue)
    sub("^.*/|\\.", "", format)
  }

  entities$extension <- vapply(entities$id, getFormatType, character(1), USE.NAMES = F)

  entities$link <- sprintf("%s/data/eml/%s/%s/%s/%s",
                           baseUrl, packageInfo$scope, packageInfo$identifier, version, entities$id)

  return(entities)
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
#' @param quiet Defaults to FALSE. If TRUE, will not print the abbreviated table
#' in the console.
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
getEDI <- function(url, files, version = "newest", quiet = FALSE) {

  tables <- getMetadataEdi(url, version = version)$df

  if (missing(files))  {
    if (!isTRUE(quiet)) {
      cat("Specify files to download: \n")
      print(tables[c("name", "extension", "size", "description")])
    }
    return(tables)
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
  # For files that are rds, read them in directly as a list output
  # All other files are downloaded
  fileFate <- function(name, size, extension, link) {

    cat("Downloading", name, "; file size:", size, "\n")
    switch(extension,
           "csv" = utils::read.csv(link),
           "rds" = readRDS(url(link)),
           {
             filePath <- file.path(tempdir(), name)
             if (!file.exists(filePath)) {
               if (utils::download.file(link, filePath, mode = "wb") != 0) {
                 stop("Download failed for: ", name, call. = FALSE)
               }
             }
             filePath
           }
    )
  }

  mapply(fileFate,
         tables$name, tables$size, tables$extension, tables$link,
         SIMPLIFY = FALSE)
}

#' Grab metadata from an EDI package webpage
#'
#' @details
#' Only an opinionated set of metadata parameters will be returned by default,
#' read from the EML XML metadata file.
#'
#' @param url URL to the EDI data package landing page
#' @param version Defaults to pulling the newest version. Specify a number if
#' you are interested in a specific version
#' @param all Defaults to FALSE. If TRUE, will return the XML file of the metadata
#' itself. If FALSE, provides only an opinionated set of parameters
#'
#' @return One of the following depending on all:
#'  \item{list}{A list containing a data.frame of opinionated metadata, the
#'  package title, and the publication date of the package}
#'  \item{xml}{The EML XML file}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Data package of the CDFW IEP SLS Survey
#' getMetadataEdi("https://portal.edirepository.org/nis/mapbrowse?packageid=edi.534.9")
#' }
getMetadataEdi <- function(url, version = "newest", all = FALSE) {

  packageInfo <- parseEDI(url)

  baseUrl <- sprintf("https://pasta.lternet.edu/package")
  versionUrl <- sprintf("%s/eml/%s/%s", baseUrl, packageInfo$scope, packageInfo$identifier)

  currentNewestVersion <- max(
    suppressWarnings(utils::read.table(versionUrl)[1])
  )

  if (packageInfo$revision != currentNewestVersion & version == "newest") {
    version <- currentNewestVersion
  } else version <- packageInfo$revision

  fullMetadata <- sprintf("%s/metadata/eml/%s/%s/%s", baseUrl, packageInfo$scope, packageInfo$identifier, version)
  doc <- suppressMessages(XML::xmlParse(httr::GET(fullMetadata), encoding = "UTF-8"))
  if (isTRUE(all)) return(doc)

  title <- XML::xpathSApply(doc, "//dataset/title", XML::xmlValue)
  entityNames <- XML::xpathSApply(doc, "//dataset//dataTable/physical/objectName |
                                  //dataset//otherEntity/physical/objectName", XML::xmlValue)
  entityExtension <- tools::file_ext(entityNames)
  entityDescription <- XML::xpathSApply(doc, "//dataset/dataTable/entityDescription |
                                        //dataset/otherEntity/entityDescription", XML::xmlValue)
  fileSize <- XML::xpathSApply(doc, "//dataset/dataTable/physical/size |
                               //dataset/otherEntity/physical/size",
                               function(x) {
                                 as.numeric(XML::xmlValue(x))
                               })
  fileSizeParsed <- vapply(fileSize,
                           function(size) {
                             format(structure(size, class = "object_size"), units = "auto", standard = "IEC")
                           },
                           character(1))
  entityLink <- XML::xpathSApply(doc, "//dataset/dataTable/physical/distribution/online/url |
                                 //dataset/otherEntity/physical/distribution/online/url", XML::xmlValue)
  entityId <- basename(entityLink)
  publicationDate <- XML::xpathSApply(doc, "//dataset/pubDate", XML::xmlValue)

  list(
    df = data.frame(
      name = entityNames,
      extension = entityExtension,
      size = fileSizeParsed,
      sizeBytes = fileSize,
      description = entityDescription,
      link = entityLink,
      id = entityId
    ),
    packageTitle = title,
    publicationDate = publicationDate
  )
}
