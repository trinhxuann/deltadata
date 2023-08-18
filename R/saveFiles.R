#' Save the data tables pulled from an Access database as .csv files
#'
#' @param relationalTables A list of the relational tables, ideally taken
#' from \code{\link{bridgeAccess}}.
#' @param folderPath Path to the folder where the csv files will be saved.
#' @param fileNames A vector of file names (not file path). If `NULL`, will
#' pull the names from the provided `relationalTables` argument. Defaults to `NULL`
#'
#' @details
#' This is a simple wrapper function that will save the relational tables pulled
#' from an Access database. The function was developed specifically for the
#' surveys within the Native Fish Program from CDFW, requiring the UTF-8 encoding.
#'
#' @importFrom utils write.csv
#'
#' @return Each table will be written. A metadata table will be returned
#' indicating success of the file writes.
#' @export
#'
#' @examples
#' \dontrun{
#' slsTables <- bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
#' tables = c("Catch", "FishCodes"))
#'
#' # Saving to the temporary directory
#' saveRelationalTables(slsTables, folderPath = tempdir())
#' }

saveRelationalTables <- function(relationalTables, folderPath, fileNames = NULL) {

  if (is.null(fileNames)) {
    fileNames <- names(relationalTables)
  }

  writingFiles <- lapply(seq_along(relationalTables), function(i) {

    filePath <- file.path(folderPath, paste0(fileNames[[i]], ".csv"))
    write.csv(relationalTables[[i]], file = filePath, row.names = F, fileEncoding = "UTF-8")
    writingSuccess <- file.exists(filePath)

  })

  data.frame(table = c(fileNames),
             writeStatus = ifelse(writingFiles, "Success", "FAILED"),
             filePath = folderPath)
}
