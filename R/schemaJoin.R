#' Produce a translation of the joining type and order of the underlying system
#' schema.
#'
#' @param schema A schema table that mirrors that structure of an Access schema.
#' @param verbose T/F, a logical argument to print the relationship within the
#' schema or not.
#'
#' @return A table of the schema without the system tables.
#'
#' @importFrom stats aggregate
#' @noRd
#' @keywords internal
translateSchema <- function(schema, verbose = F) {

  filteredData <- schema[!grepl("^MSys.*", schema[["szRelationship"]]), ]

  if (is.null(schema$joinType)) {
    groupedData <- aggregate(cbind(szColumn, szReferencedColumn) ~ grbit + szObject + szReferencedObject + szRelationship,
                             data = filteredData, FUN = paste, collapse = ".")
  } else {
    groupedData <- aggregate(cbind(szColumn, szReferencedColumn) ~ grbit + szObject + szReferencedObject +
                               szRelationship + joinType,
                             data = filteredData, FUN = paste, collapse = ".")
  }

  names(groupedData)[which(names(groupedData) %in% c("szColumn", "szReferencedColumn"))] <- c("foreignKeys", "primaryKeys")
  groupedData$szRelationship <- factor(groupedData$szRelationship, levels = unique(groupedData$szRelationship))
  schema <- groupedData[order(groupedData$szRelationship), ]

  if (is.null(schema$joinType)) {
    if (any(tolower(names(schema)) %in% "grbit")) {
      schema$joinType <- sapply(schema$grbit, function(x) {
        if (x < 16777216) "inner_join"
        else (if (x >= 16777216 & x < 33554432) "left_join"
              else ("right_join"))
      })
    }
  }

  schema$joinFunction <- sapply(schema$joinType, function(x) {
    if (x == "inner_join") function(a, b, by) merge(a, b, by, all = F)
    else (if (x == "left_join") function(a, b, by) merge(a, b, by, all.x = T)
          else (if (x == "right_join") function(a, b, by) merge(a, b, by, all.y = T)
                else (if (x == "full_join") function(a, b, by) merge(a, b, by, all = T)
                      else(stop("Supply joinType as `inner_join`, `left_join`, or `right_join` only.", call. = F)))))
  })

  if (isTRUE(verbose)) {
    for (i in 1:nrow(schema)) {
      cat(schema$joinType[[i]], sQuote(schema$szReferencedObject[[i]]), "with", sQuote(schema$szObject[[i]]), "via columns", sQuote(schema$primaryKeys[[i]]), "and", sQuote(schema$foreignKeys[[i]]), "\n")
    }
    return(cat("\nALL tables within this relationship are:", paste(unique(c(schema$szObject, schema$szReferencedObject)),
                                                                   collapse = ", ")))
  }

  schema
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
#' @param start The starting relational table to begin construction. If NULL,
#' will start at the first table specified by the schema.
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
schemaJoin <- function(schema, data, start = NULL) {

  if (class(schema) == "list") stop("Your schema is not provided as a data.frame.", call. = F)

  schema <- translateSchema(schema)
  schema <- schema[-which(schema$grbit == 2), ]
  allTables <- factor(unique(c(schema$szObject, schema$szReferencedObject)),
                      levels = unique(c(unique(schema[["szReferencedObject"]]), unique(schema$szObject))))

  tableEssentialDFs <- data.frame(
    table = allTables,
    count = (allTables %in% schema$szObject) + (allTables %in% schema$szReferencedObject)
  )
  tableEssentialDFs <- tableEssentialDFs[order(tableEssentialDFs$table), ]
  tableEssentialDFs <- tableEssentialDFs[1:max(which(tableEssentialDFs[["table"]] %in% names(data))), ]
  tableEssentialDFs$providedTables <- tableEssentialDFs[["table"]] %in% names(data)

  tableRequired <- tableEssentialDFs[tableEssentialDFs[["count"]] == 2 & !tableEssentialDFs[["providedTables"]], "table"]

  if (length(tableRequired) > 0) {
    stop("You are missing intermediate table(s) required for completing the joins: ", paste(tableRequired, collapse = ", "), call. = F)
  }
  schema$score <- schema$szObject %in% names(data) + schema$szReferencedObject %in% names(data)
  schema <- schema[schema[["score"]] == 2, ]

  startTable <- data[[schema$szReferencedObject[1]]]
  usedTable <- schema$szReferencedObject[1]

  for (i in 1:nrow(schema)) {
    # If matches table already used, use the non-parent table
    result <- tryCatch({
      if (any(schema$szObject[[i]] %in% usedTable)) {
        xTable <- schema$szObject[i]
        yTable <- schema$szReferencedObject[i]
        xName <- strsplit(schema$foreignKeys[[i]], "\\.")[[1]]
        yName <- strsplit(schema$primaryKeys[[i]], "\\.")[[1]]
      } else {
        xTable <- schema$szReferencedObject[i]
        yTable <- schema$szObject[i]
        xName <- strsplit(schema$primaryKeys[[i]], "\\.")[[1]]
        yName <- strsplit(schema$foreignKeys[[i]], "\\.")[[1]]
      }
      cat(schema$joinType[[i]], sQuote(xTable), "with", sQuote(yTable), "via columns", sQuote(xName), "and", sQuote(yName), "\n")

      startTable <- schema$joinFunction[[i]](startTable,
                                             data[[yTable]],
                                             by = setNames(yName, xName))
      usedTable <- c(usedTable, yTable)
    },
    error = function(err) {
      if (grepl("cannot allocate vector of size", err)) {
        return("sizeError")
      }
    }
    )

    if (result[[1]] %in% "sizeError") {
      stop("Resulting table is size is too large. Pare down your list of tables.", call. = F)
    }
  }
  startTable
}
