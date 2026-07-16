#' Download or access a local file, with robust handling for URLs.
#'
#' @description
#' This function downloads a file from a URL or verifies the path to a local
#' file. It avoids re-downloading if the file already exists in the temporary
#' directory. It includes robust error handling, dynamic timeouts for large
#' files, and the ability to unzip and find specific file types (e.g., Access
#' databases). It is a modern replacement for the original getFile function,
#' using the httr package to handle downloads reliably and avoid common SSL/TLS
#' issues.
#'
#' @param file A character string: either a URL to a file or a local file path.
#' @param open A logical value. If `TRUE`, the final file will be opened using
#'   the system's default application.
#' @param timeout A numeric value in seconds to override the download timeout.
#'   If `NULL` (the default), the timeout is calculated dynamically based on
#'   the file size, with R's global `getOption("timeout")` (defaulting to 60) used as the floor.
#' @param targetExtension Extension of targeted file. Defaults to Access database
#'
#' @return The full path to the final, ready-to-use file (unzipped if necessary).
#'
#' @noRd
#' @importFrom httr GET HEAD headers progress timeout write_disk stop_for_status
#' @importFrom utils browseURL unzip
#' @keywords internal
getFile <- function(file, open = FALSE, timeout = NULL,
                    targetExtension = c("accdb", "mdb")) {

  isUrl <- grepl("^https?://", file, ignore.case = TRUE)
  fileName <- basename(file)
  filePath <- if (isUrl) file.path(tempdir(), fileName) else file
  if (isUrl && !file.exists(filePath)) {
    message("Downloading file from URL: ", sQuote(file))

    defaultTimeout <- getOption("timeout", 60)
    timeOut <- defaultTimeout
    downloadUrl <- file

    # Always attempt a HEAD request to resolve redirects and get file size
    try({
      head_response <- HEAD(file)

      # Use the final redirected URL to avoid redirecting during GET()
      if (!is.null(head_response$url)) {
        downloadUrl <- head_response$url
      }

      if (is.null(timeout)) {
        if (!is.null(headers(head_response)$`content-length`)) {
          fileSize <- as.numeric(headers(head_response)$`content-length`) / 1024^2
          timeOut  <- max(defaultTimeout, ceiling(fileSize))
          message(sprintf("File size is approx %.2f MB. Setting download timeout to %d seconds.",
                          fileSize, timeOut))
        }
      }
    }, silent = TRUE)

    if (!is.null(timeout)) {
      timeOut <- as.numeric(timeout)
      message(sprintf("Using user-specified download timeout of %d seconds.", timeOut))
    }

    tryCatch({
      response <- GET(
        url = downloadUrl,
        write_disk(filePath, overwrite = TRUE),
        progress(),
        timeout(timeOut)
      )
    }, error = function(e) {
      if (file.exists(filePath)) file.remove(filePath)
      stop(sprintf("Failed to download file. Error: %s", conditionMessage(e)), call. = FALSE)
    })
  }
  finalPath <- filePath

  if (grepl("\\.zip$", fileName, ignore.case = TRUE)) {
    zip_contents <- utils::unzip(filePath, list = TRUE)

    # Strip any leading dots so c("tif") and c(".tif") both work
    extPattern <- paste0("\\.(", paste(gsub("^\\.", "", targetExtension), collapse = "|"), ")$")
    targetFile <- zip_contents$Name[grepl(extPattern, zip_contents$Name, ignore.case = TRUE)]

    if (length(targetFile) == 0) {
      stop(sprintf(
        "No file with extension(s) %s was found in the .zip archive.",
        paste(targetExtension, collapse = ", ")
      ), call. = FALSE)
    }
    if (length(targetFile) > 1) {
      warning(paste("Multiple matching files found; using the first:", targetFile[1]),
              call. = FALSE)
      targetFile <- targetFile[1]
    }

    extractedPath <- file.path(tempdir(), targetFile)

    if (!file.exists(extractedPath)) {
      message("Extracting: ", sQuote(targetFile), " from zip archive.")
      utils::unzip(filePath, files = targetFile, exdir = tempdir(), overwrite = TRUE)
    }

    finalPath <- extractedPath
  }
  if (!file.exists(finalPath)) {
    stop("Could not find the final file at path: ", finalPath, call. = FALSE)
  }

  if (isTRUE(open)) {
    message("Opening file: ", sQuote(basename(finalPath)))
    utils::browseURL(finalPath)
  }

  return(finalPath)
}

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
    return(invisible(tables))
  }

  matchedTables <- files %in% tables$name

  if (!all(matchedTables)) {
    unmatchedNames <- files[!matchedTables]

    if (!isTRUE(quiet)) {
      print(tables[c("name", "extension", "size", "description")])
    }

    stop("The specified table(s) cannot be found in the EDI publication: ",
         paste(unmatchedNames, collapse = ", "),
         ". Please check your spelling.", call. = FALSE)
  }

  tables <- tables[tables[["name"]] %in% files, ]

  # For files that are csv, read them in directly
  # For files that are rds, read them in directly as a list output
  # All other files are downloaded
  fileFate <- function(name, size, extension, link) {

    filePath <- file.path(tempdir(), name)
    message("Downloading: ", name, " (", size, ")")

    response <- httr::GET(
      url = link,
      httr::write_disk(path = filePath, overwrite = TRUE),
      httr::progress()
    )
    httr::stop_for_status(response, task = paste("download", name))

    switch(extension,
           csv = read.csv(filePath),
           rds = readRDS(filePath),
           # For any other file type, return the path to the temporary file,
           # preserving the original function's behavior.
           filePath
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
    scan(text = content(httr::GET(versionUrl), "text", encoding = "UTF-8"), quiet = T)
  )

  if (packageInfo$revision != currentNewestVersion & version == "newest") {
    version <- currentNewestVersion
  } else version <- packageInfo$revision

  fullMetadataUrl <- sprintf("%s/metadata/eml/%s/%s/%s", baseUrl, packageInfo$scope, packageInfo$identifier, version)
  fullMetadata <- httr::GET(fullMetadataUrl)
  httr::stop_for_status(fullMetadata, task = "fetch EML metadata")

  # Modernize with xml2 instead of XML
  doc <- xml2::read_xml(fullMetadata)
  if (isTRUE(all)) return(doc)

  # Get namespaces to use in XPath
  ns <- xml2::xml_ns(doc)

  # Grab relevant elements
  title <- xml2::xml_text(xml2::xml_find_first(doc, ".//dataset/title", ns))
  pubDate <- xml2::xml_text(xml2::xml_find_first(doc, ".//dataset/pubDate", ns))
  entities <- xml2::xml_find_all(doc, ".//dataTable | .//otherEntity", ns)

  # Extract the metadata per entity
  entityDf <- dplyr::bind_rows(lapply(entities, function(entity) {
    name <- xml2::xml_text(xml2::xml_find_first(entity, ".//physical/objectName", ns))
    fileSize <- as.numeric(xml2::xml_text(xml2::xml_find_first(entity, ".//physical/size", ns)))
    link <- xml2::xml_text(xml2::xml_find_first(entity, ".//distribution/online/url", ns))
    entityDescription <- xml2::xml_text(xml2::xml_find_first(entity, ".//entityDescription", ns))

    fileSizeParsed <- if (!is.na(fileSize)) {
      format(structure(fileSize, class = "object_size"), units = "auto", standard = "IEC")
    } else {
      NA_character_
    }

    data.frame(
      name = name,
      extension = tools::file_ext(name),
      size = fileSizeParsed,
      sizeBytes = fileSize,
      description = entityDescription,
      link = link,
      id = basename(link)
    )
  }))

  list(
    df = entityDf,
    packageTitle = title,
    publicationDate = pubDate
  )
}
