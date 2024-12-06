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
#' @importFrom httr GET content timeout status_code
#' @importFrom XML xmlParse xmlValue getNodeSet xpathSApply
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
  namespaces <- c(
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

  parseXML <- function(response) {
    tryCatch(
      XML::xmlParse(response),
      error = function(e) NULL
    )
  }

  processHierarchy <- function(tsn, currentRow, index) {
    # Get validity
    validityUrl <- paste0(baseUrl, "getTaxonomicUsageFromTSN?tsn=", tsn)
    validityDoc <- parseXML(makeRequest(validityUrl))

    if (!is.null(validityDoc)) {
      validName <- XML::xpathSApply(
        validityDoc,
        "//ax21:taxonUsageRating",
        XML::xmlValue,
        namespaces = namespaces
      )
      currentRow$validity[index] <- if(length(validName) > 0)
        gsub("\\s+(\\w)", "\\U\\1", tolower(validName), perl = TRUE) else "noItisValue"
    }

    # Get hierarchy
    hierarchyUrl <- paste0(baseUrl, "getFullHierarchyFromTSN?tsn=", tsn)
    hierarchyDoc <- parseXML(makeRequest(hierarchyUrl))

    if (!is.null(hierarchyDoc)) {
      taxonUnits <- XML::getNodeSet(
        hierarchyDoc,
        "//ax21:hierarchyList",
        namespaces = namespaces
      )

      if (length(taxonUnits) > 0) {
        for (unit in taxonUnits) {
          rankName <- tolower(XML::xpathSApply(
            unit,
            ".//ax21:rankName",
            XML::xmlValue,
            namespaces = namespaces
          ))

          if (length(rankName) == 0) next

          taxonName <- XML::xpathSApply(
            unit,
            ".//ax21:taxonName",
            XML::xmlValue,
            namespaces = namespaces
          )

          if (length(taxonName) == 0) next

          rankName <- trimws(rankName)
          if (rankName %in% rankNames) {
            currentRow[[rankName]][index] <- trimws(taxonName)
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
      # cat(sprintf("\r%-*s", 80, message), "\n")
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
    matchCountUrl <- paste0(baseUrl, "getAnyMatchCount?srchKey=",
                            utils::URLencode(name, reserved = TRUE))
    matchCountDoc <- parseXML(makeRequest(matchCountUrl))

    if (is.null(matchCountDoc)) {
      resultsList[[i]] <- currentRow
      next
    }

    matchedCount <- as.numeric(XML::xpathSApply(
      matchCountDoc,
      "//ns:return",
      XML::xmlValue,
      namespaces = namespaces
    ))

    if (matchedCount == 0) {
      if (verbose) cat(sprintf("No matches found for: %s\n", name))
      currentRow$validity <- "noMatch"
      resultsList[[i]] <- currentRow
      next
    }

    # Search for matches
    searchUrl <- paste0(baseUrl, "searchForAnyMatch?srchKey=",
                        utils::URLencode(name, reserved = TRUE))
    xmlDoc <- parseXML(makeRequest(searchUrl))

    if (is.null(xmlDoc)) {
      resultsList[[i]] <- currentRow
      next
    }

    matches <- XML::getNodeSet(xmlDoc, "//ax21:anyMatchList", namespaces = namespaces)

    tsns <- unique(unlist(lapply(matches, function(match) {

      matchName <- XML::xpathSApply(
        match,
        ".//ax21:*[self::ax21:sciName or self::ax21:commonName]",
        XML::xmlValue,
        namespaces = namespaces
      )

      if (any(tolower(matchName) %in% tolower(name))) {
        XML::xpathSApply(
          match,
          ".//ax21:tsn",
          function(x) as.numeric(XML::xmlValue(x)),
          namespaces = namespaces
        )
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

  do.call(rbind, lapply(resultsList, as.data.frame))
}
