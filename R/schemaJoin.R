#' Produce a translation of the joining type and order of the underlying system
#' schema.
#'
#' @param schema A schema table that mirrors that structure of an Access schema.
#' @param verbose T/F, a logical argument to print the relationship within the
#' schema or not.
#'
#' @return A schema without the system tables and translated column names.
#'
#' @importFrom stats aggregate
#' @noRd
#' @keywords internal
translateSchema <- function(schema, verbose = FALSE) {
  # Filter out MSys relationships
  schema <- schema[!grepl("^MSys.*", schema$szRelationship), ]

  # Aggregate columns based on relationship details
  # Need this for relationships with multiple keys
  schema <- aggregate(cbind(szColumn, szReferencedColumn) ~ grbit + szObject + szReferencedObject + szRelationship,
                      data = schema, FUN = paste, collapse = ".")

  # Rename columns for clarity
  names(schema)[match(c("szColumn", "szReferencedColumn", "szObject", "szReferencedObject"),
                      names(schema))] <- c("foreignKeys", "primaryKeys", "foreignTable", "primaryTable")

  # Order by relationship name
  # schema <- schema[order(schema$szRelationship), ]

  # Determine join type based on grbit value
  schema$joinType <- ifelse(schema$grbit < 16777216, "inner_join",
                            ifelse(schema$grbit < 33554432, "left_join", "right_join"))

  # Create join function based on join type
  schema$joinFunction <- lapply(schema$joinType, function(x) {
    switch(x,
           inner_join = function(x, y, by.x, by.y) merge(x, y, by.x = by.x, by.y = by.y, all = FALSE),
           left_join = function(x, y, by.x, by.y) merge(x, y, by.x = by.x, by.y = by.y, all.x = TRUE),
           right_join = function(x, y, by.x, by.y) merge(x, y, by.x = by.x, by.y = by.y, all.y = TRUE),
           stop("Supply joinType as `inner_join`, `left_join`, or `right_join` only.", call. = FALSE))
  })

  # Print verbose output if requested
  if (verbose) {
    for (i in 1:nrow(schema)) {
      cat(schema$joinType[[i]], sQuote(schema$szReferencedObject[[i]]), "with", sQuote(schema$szObject[[i]]),
          "via columns", sQuote(schema$primaryKeys[[i]]), "and", sQuote(schema$foreignKeys[[i]]), "\n")
    }
    cat("\nALL tables within this relationship are:", paste(unique(c(schema$szObject, schema$szReferencedObject)), collapse = ", "), "\n")
  }

  return(schema)
}

#' Determine joining by ordering the schema
#'
#' @param schema A schema table that mirrors that structure of an Access schema.
#' @param providedTables A named vector of the provided relational tables
#'
#' @return A schema ordered in the joining order
#'
#' @noRd
#' @keywords internal
orderSchema <- function(schema, providedTables) {

  # Keep only tables that are provided
  filteredSchema <- schema[schema[["foreignTable"]] %in% providedTables & schema[["primaryTable"]] %in% providedTables, ]

  # Start with tables that have the least number of connections
  occurrences <- sort(table(c(filteredSchema[["primaryTable"]], filteredSchema[["foreignTable"]])))
  # Start with an edge table, that is a primary table, and forces cascade update/deletes
  # When grbit is 1, that's a 1:1 relationship in which both are primary tables
  oneToOne <- c(1, 3, 257, 4097, 5353, 16777217, 16777453, 16781313,
                16781569, 33554433, 33554689, 33558529, 33559785)
  oneToOneTables <- filteredSchema[which(filteredSchema$grbit %in% oneToOne), ]
  primaryTables <- unique(c(oneToOneTables$foreignTable, oneToOneTables$primaryTable, filteredSchema$primaryTable))
  edgeTables <- names(occurrences[which(occurrences == 1)])
  cascadeCandidates <- unlist(filteredSchema[which(filteredSchema$grbit > 2), c("foreignTable", "primaryTable")],
                              use.names = F)
  qualifyingVectors <- list(primaryTables, edgeTables, cascadeCandidates)
  startTable <- Reduce(intersect, qualifyingVectors)

  # Reorder the schema
  # May break if the starting table is a foreign table...
  start <- filteredSchema[filteredSchema$primaryTable == startTable |
                            filteredSchema$foreignTable == startTable, ]
  usedTables <- startTable

  while (nrow(start) > 0) {
    if (!start$grbit[nrow(start)] %in% oneToOne) {
      nextTable <- filteredSchema[filteredSchema$primaryTable == start$foreignTable[nrow(start)], ]
    } else {
      nextTable <- filteredSchema[filteredSchema$primaryTable == start$primaryTable[nrow(start)], ]
      if (nrow(nextTable) > 1) {
        # Occurs if there was a one-to-one and a table is listed as primary across 1+ relationships
        nextTable <- nextTable[!nextTable$grbit %in% oneToOne, ]
      }
    }

    if (nrow(nextTable) > 0 && !(nextTable$primaryTable %in% usedTables)) {
      start <- rbind(start, nextTable)
      usedTables <- c(usedTables, nextTable$primaryTable)
    } else {
      break  # Exit the loop if no more related tables or if a cycle is detected
    }
  }
  start
}

#' Applies the joins in accordance to a relationship schema.
#'
#' @description
#' This function pulls the system table within an Access database that dictates
#' the join order and types. It will then attempt to apply those joining rules
#' to the provided tables. Users can specify which tables they would like to
#' join through the tables that they provide the function. Ensure that the names
#' of the tables provided matches those required in the relationship table.
#'
#' @param schema A schema table that mirrors the structure of an Access schema.
#' @param data A list of data tables to be joined
#'
#' @details
#' The join type in the relationship table from Access can be modified directly if required. Simply add a `joinType` column to the relationship table and specify one of four options: `full_join`, `inner_join`, `left_join`, or `right_join`. The `data` argument can theoretically take in any data frame. This allows for manipulations of the relational tables if such operations are necessary before joining. Within the schema table itself, `szReferencedObject` refers to the relational table with the primary key, while `szObject` refers to the relational table with the foreign key.
#'
#'
#' @return A data frame joined according to the relationship schema.
#' @export
#'
#' @examples
#' \dontrun{
#' slsTables <- bridgeAccess(
#' "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
#' tables = c("Catch", "FishCodes", "Lengths", "Meter Corrections",
#' "SLS Stations", "Tow Info", "Water Info")
#' )
#'
#' schema <- bridgeAccess(
#' "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
#' tables = c("MSysRelationships"))[[1]]
#'
#' schemaJoin(schema, slsTables)
#' }
#' @note
#' The tables in the 20mm dataset aren't sequentially linked. There are tables that
#' are linked together first before being linked back to the original. How do I solve this?
#'
schemaJoin <- function(schema, data) {
  # Validate schema
  if (!is.data.frame(schema)) stop("Your schema is not provided as a data.frame.", call. = FALSE)

  # Translate schema
  schema <- translateSchema(schema)

  # Number of occurrences of each relational table
  numberOccurence <- names(sort(table(c(schema[["primaryTable"]], schema[["foreignTable"]])), decreasing = T))

  # Filter out relationships with grbit value of 2; these are generally
  # look up tables that do not enforce the relationship of the database
  schema <- schema[schema$grbit != 2, ]

  # Filter schema based on available tables
  providedTables <- names(data)
  filteredSchema <- orderSchema(schema, providedTables)

  # Start with the first referenced table
  startTable <- data[[filteredSchema[["primaryTable"]][1]]]
  usedTables <- filteredSchema[["primaryTable"]][[1]]

  for (i in 1:nrow(filteredSchema)) {
    a <- filteredSchema[["primaryTable"]][[i]]
    b <- filteredSchema[["foreignTable"]][[i]]
    primaryKey <- strsplit(filteredSchema[["primaryKeys"]][[i]], "\\.")[[1]]
    foreignKey <- strsplit(filteredSchema[["foreignKeys"]][[i]], "\\.")[[1]]

    cat(filteredSchema$joinType[[i]], sQuote(a), "with", sQuote(b), "via columns", sQuote(primaryKey), "and", sQuote(foreignKey), "\n")
    # If the primary table has already been joined, then use the join table as the primary table
    if (a %in% usedTables) {
      startTable <- filteredSchema$joinFunction[[i]](
        startTable, data[[b]],
        by.x = primaryKey, by.y = foreignKey
      )
    } else {
      startTable <- filteredSchema$joinFunction[[i]](
        data[[a]], startTable,
        by.x = primaryKey, by.y = foreignKey
      )
    }

    # Update the usedTables to account for what table have joined already
    usedTables <- c(usedTables, b)
  }
  return(startTable)
}
