#' Get full taxonomic information from ITIS
#'
#' @param taxonNames A vector of species names of their Taxonomic Species Number (TSN).
#' Supports different ranks as input. See details.
#' @param verbose Logical. Defaults to TRUE. Should progress notifications be
#' printed in the console?
#'
#' @return A data frame
#' @export
#'
#' @details
#' Only taxonomic rank information above the inputted value will be provided, e.g.,
#' if you provide a value at the phylum level, only the kingdom-associated levels
#' will be returned. Additionally, there may be different TSNs associated with
#' an input value, one being valid and the other not. Both will be returned. Users
#' should take care to check the 'validity' column for additional filtering
#' criteria.
#'
#' @examples
#' \dontrun{
#' taxa <- c("Chinese Mitten Crab", "Liparis", "Oncorhynchus tshawytscha")
#'
#' getTaxonomyFromItis(taxa)
#' }
getTaxonomyFromItis <- function(taxonNames, verbose = TRUE) {

  # Constants
  baseUrl <- "https://www.itis.gov/ITISWebService/services/ITISService/"
  ns <- c(
    ns = "http://itis_service.itis.usgs.gov",
    ax21 = "http://data.itis_service.itis.usgs.gov/xsd"
  )
  rankNames <- c(
    "kingdom", "subkingdom", "infrakingdom", "phylum", "subphylum",
    "infraphylum", "superclass", "class", "superorder", "order",
    "suborder", "family", "subfamily", "genus", "species"
  )

  # Helper functions
  makeRequest <- function(url) {
    response <- httr::GET(url, httr::timeout(10))
    if (httr::status_code(response) == 429) {
      Sys.sleep(2)
      response <- httr::GET(url, httr::timeout(10))
    }
    response
  }

  parseXml <- function(response) {
    # Use content as raw to ensure xml2 can read it robustly
    cont <- try(httr::content(response, as = "raw"), silent = TRUE)
    if (inherits(cont, "try-error") || is.null(cont)) return(NULL)
    doc <- try(xml2::read_xml(cont), silent = TRUE)
    if (inherits(doc, "try-error")) return(NULL)
    doc
  }

  xmlText0 <- function(nodeset) {
    if (length(nodeset) == 0) character(0) else xml2::xml_text(nodeset)
  }

  processHierarchy <- function(tsn, currentRow, index) {
    # Get validity
    validityUrl <- paste0(baseUrl, "getTaxonomicUsageFromTSN?tsn=", tsn)
    validityDoc <- parseXml(makeRequest(validityUrl))

    if (!is.null(validityDoc)) {
      validNode <- xml2::xml_find_all(validityDoc, "//ax21:taxonUsageRating", ns = ns)
      validName <- xmlText0(validNode)
      if (length(validName) > 0) {
        currentRow$validity[index] <- gsub("\\s+(\\w)", "\\U\\1", tolower(validName), perl = TRUE)
      } else {
        currentRow$validity[index] <- "noItisValue"
      }
    }

    # Get hierarchy
    hierarchyUrl <- paste0(baseUrl, "getFullHierarchyFromTSN?tsn=", tsn)
    hierarchyDoc <- parseXml(makeRequest(hierarchyUrl))

    if (!is.null(hierarchyDoc)) {
      taxonUnits <- xml2::xml_find_all(hierarchyDoc, "//ax21:hierarchyList", ns = ns)
      if (length(taxonUnits) > 0) {
        for (unit in taxonUnits) {
          rankNode <- xml2::xml_find_all(unit, ".//ax21:rankName", ns = ns)
          rankName <- tolower(trimws(xmlText0(rankNode)))
          if (length(rankName) == 0) next
          taxonNode <- xml2::xml_find_all(unit, ".//ax21:taxonName", ns = ns)
          taxonName <- trimws(xmlText0(taxonNode))
          if (length(taxonName) == 0) next
          if (rankName %in% rankNames) {
            currentRow[[rankName]][index] <- taxonName
          }
        }
      }
    }
    currentRow
  }

  # Input validation
  if (!is.character(taxonNames) || length(taxonNames) == 0) {
    stop("taxonNames must be a non-empty character vector")
  }

  taxonNames <- trimws(taxonNames[nzchar(taxonNames)])
  resultsList <- vector("list", length(taxonNames))

  # Process each taxon
  for (i in seq_along(taxonNames)) {
    name <- taxonNames[i]

    if (verbose) {
      message <- sprintf("Processing taxon %d of %d: %s", i, length(taxonNames), name)
      cat(message, "\n")
      utils::flush.console()
    }

    # Initialize current row
    currentRow <- c(
      list(
        inputName = name,
        tsn = NA_character_,
        validity = NA_character_
      ),
      setNames(
        replicate(length(rankNames), NA_character_, simplify = FALSE),
        rankNames
      )
    )

    # Check match count
    matchCountUrl <- paste0(
      baseUrl, "getAnyMatchCount?srchKey=",
      utils::URLencode(name, reserved = TRUE)
    )
    matchCountDoc <- parseXml(makeRequest(matchCountUrl))
    if (is.null(matchCountDoc)) {
      resultsList[[i]] <- currentRow
      next
    }

    matchedCountNode <- xml2::xml_find_all(matchCountDoc, "//ns:return", ns = ns)
    matchedCountTxt <- xmlText0(matchedCountNode)
    matchedCount <- suppressWarnings(as.numeric(matchedCountTxt))
    if (length(matchedCount) == 0 || is.na(matchedCount) || matchedCount == 0) {
      if (verbose) cat(sprintf("No matches found for: %s\n", name))
      currentRow$validity <- "noMatch"
      resultsList[[i]] <- currentRow
      next
    }

    # Search for matches
    searchUrl <- paste0(
      baseUrl, "searchForAnyMatch?srchKey=",
      utils::URLencode(name, reserved = TRUE)
    )
    xmlDoc <- parseXml(makeRequest(searchUrl))

    if (is.null(xmlDoc)) {
      resultsList[[i]] <- currentRow
      next
    }

    matches <- xml2::xml_find_all(xmlDoc, "//ax21:anyMatchList", ns = ns)
    tsns <- unique(unlist(lapply(matches, function(mn) {
      matchNameNodes <- xml2::xml_find_all(
        mn,
        ".//ax21:*[self::ax21:sciName or self::ax21:commonName]",
        ns = ns
      )
      matchNames <- tolower(xmlText0(matchNameNodes))
      if (length(matchNames) == 0) return(NULL)
      if (any(matchNames %in% tolower(name))) {
        tsnNodes <- xml2::xml_find_all(mn, ".//ax21:tsn", ns = ns)
        tsnVals <- suppressWarnings(as.numeric(xmlText0(tsnNodes)))
        tsnVals[!is.na(tsnVals)]
      } else {
        NULL
      }
    })))

    if (length(tsns) > 0) {
      # Reinitialize currentRow with correct dimensions
      currentRow <- lapply(currentRow, function(x) rep(NA_character_, length(tsns)))
      currentRow$inputName <- name
      currentRow$tsn <- tsns

      # Process each TSN
      for (t in seq_along(tsns)) {
        currentRow <- processHierarchy(tsns[t], currentRow, t)
      }
    } else {
      if (verbose) cat(sprintf("No matches found for: %s\n", name))
      currentRow$validity <- "noMatch"
    }

    resultsList[[i]] <- currentRow
  }

  if (verbose) cat("Processing complete!\n")

  if (all(sapply(resultsList, is.null))) {
    stop("No valid results obtained")
  }

  dplyr::bind_rows(lapply(resultsList, as.data.frame, stringsAsFactors = FALSE))
}

