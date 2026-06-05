#' Checks the architectures of your R and Microsoft Access programs.
#'
#' @param officeBit NULL, the architecture (32 or 64-bit) of your Microsoft
#' Access program. If you are on Windows, this will automatically be detected;
#' on a Linux system, you will have to provide this manually.
#' @param path32 Path to folder containing the 32-bit R executable.
#'
#' @return `TRUE`/`FALSE`, where `TRUE` means that your R and Access
#' architectures match.
#'
#' @noRd
#' @keywords internal
architectureCheck <- function(officeBit = NULL, path32) {

  # Only works on a Windows computer
  if (!Sys.info()["sysname"] %in% "Windows") {
    message("Operating system is not Windows. This function will likely fail.")
    return(NULL)
  }
  # What architecture of R are you on?
  rBit <- ifelse((.Machine$sizeof.pointer == 4), "x32", "x64")

  rscriptExe <- ""

  # Do you have 32 bit or 64 bit office installed
  # Can attempt to read this from the registry itself;
  # if unsuccessful, the user must specify
  if (is.null(officeBit)) {
    if (rBit == "x64") {
      fp <- file.path("SOFTWARE", "Microsoft", "Office",
                      "ClickToRun", "Configuration",
                      fsep = "\\")
      subkey <- "Platform"
    } else {
      fp <- file.path("SOFTWARE", "Microsoft", "Office", "16.0", "Outlook",
                      fsep = "\\")
      subkey <- "Bitness"
    }

    officeBit <- tryCatch(utils::readRegistry(fp)[[subkey]],
                          error = function(cond) {
                            ifelse(grepl("not found", cond$message),
                                   stop("Cannot automatically detect the architecture of your Microsoft Office. Please fill in `x32` or `x64` manually in the `officeBit` argument.", call. = F),
                                   stop(cond))
                          })
    officeBit <- ifelse((officeBit != "x64"), "x32", "x64")
  }

  # Are they the same?
  if (officeBit != rBit) {
    if (rBit == "x64" & officeBit == "x32") {
      if (path32 == "default") {
        # Try to find the 32-bit R path dynamically via the Registry
        path32Found <- tryCatch({
          utils::readRegistry("SOFTWARE\\R-core\\R32", hive = "HLM")[["InstallPath"]]
        }, error = function(e) NULL)

        # For non-admin right users
        if (is.null(path32Found)) {
          path32Found <- tryCatch({
            utils::readRegistry("SOFTWARE\\R-core\\R32", hive = "HCU")[["InstallPath"]]
          }, error = function(e) NULL)
        }

        if (!is.null(path32Found)) {
          path32 <- path32Found
        }
      }

      # Build the expected executable path if found
      if (!is.null(path32) && path32 != "default" && path32 != "") {
        if (grepl("Rscript\\.exe$", path32, ignore.case = TRUE)) {
          rscriptExe <- path32
        } else {
          rscriptExe <- file.path(path32, "bin", "i386", "Rscript.exe")
        }
      }

      if (!file.exists(rscriptExe)) {

        downloadURL <- "https://cran.r-project.org/bin/windows/base/old/4.1.3/"

        msg <- paste(
          "Error: A 32-bit R installation is required because your Microsoft Office is 32-bit.",
          "R 4.1.3 is the last version of R that supports 32-bit compilation on Windows.",
          "Please download and install it from the CRAN archive:",
          paste0("  -> ", downloadURL),
          sep = "\n"
        )

        # Interactive helper to assist the user directly in RStudio/Console
        if (interactive()) {
          message(msg)
          ans <- readline("Would you like to open the CRAN download page in your browser? (y/n): ")
          if (tolower(trimws(ans)) %in% c("y", "yes")) {
            utils::browseURL(downloadURL)
          }
          stop("32-bit R is missing. Please install R 4.1.3 32-bit to proceed.", call. = FALSE)
        } else {
          stop(msg, call. = FALSE)
        }
      }
    }
  }

  check <- ifelse(rBit == officeBit, T, F)

  list(check = check,
       rBit = rBit,
       officeBit = officeBit,
       path32 = rscriptExe)
}

#' Facilitates connection from R to Access.
#'
#' @description
#' This function creates the actual connection to the Access database. Requires odbc drivers, which should be installed alongside Access (32 or 64 bit).
#'
#' @param path File path to database.
#' @param driver ODBC driver. Defaults to using the Access drivers.
#' @param uid Username credential, if applicable to your database.
#' @param pwd Password credential, if applicable to your database.
#' @param ... Other details specific to your database connection. Usually can ignore if working with an Access database
#'
#' @return A DBIConnection object to allow interactions with the database.
#'
#' @noRd
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @keywords internal
connectAccess <- function(path,
                          driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                          uid = "", pwd = "", ...) {

  file <- normalizePath(path, winslash = "\\", mustWork = T)

  # Driver and path required to connect from RStudio to Access
  dbString <- paste0("Driver={", driver,
                     "};Dbq=", file,
                     ";Uid=", uid,
                     ";Pwd=", pwd,
                     ";")

  tryCatch(dbConnect(drv = odbc(),
                     .connection_string = dbString,
                     ...),
           error = function(cond) {
             if (grepl(c("IM002.*ODBC Driver Manager"), cond$message)) {
               message(cond, "\n")
               stop("IM002 and ODBC Driver Manager error generally means a 32-bit R needs to be installed or used.", call. = F)
             } else {
               if (grepl(c("IM006"), cond$message)) {
                 file.remove(file)
                 stop("File corrupted. Try setting `method = curl` to resolve this error.", call. = F)
               } else {
                 message(cond)
               }
             }
           })
}

#' Extract tables from a connection
#'
#' @description
#' Through an existing connection to an Access database, return the requested relational tables. If none are provided, will return a vector of possible tables tp choose from.
#'
#'
#' @param con A DBIConnection object.
#' @param tables The tables that you wish to pull from the database. This can
#' be left as its default, equal to "check", to return a list of tables to
#' choose from.
#' @param rBit Architecture of your R, "x32" or "x64"
#' @param officeBit Architecture of your Microsoft Office, "x32" or "x64"
#' @param out File path to store the rds file. This is required if you are on
#' 64-bit R but have a 32-bit version of your database application, e.g., Access
#' @param retry Logical. If `TRUE`, the function will retry extracting after waiting 25 seconds.
#'
#' @return A list of data tables.
#'
#' @importFrom odbc dbListTables
#' @importFrom DBI dbReadTable dbDisconnect dbGetInfo
#' @noRd
#' @keywords internal
extractTables <- function(con, tables, rBit, officeBit, out = out, retry = T) {

  on.exit(dbDisconnect(con))
  # Pulling just the table names
  tableNames <- dbListTables(conn = con)

  if (length(tables) == 1 & all(tables %in% "check")) {
    # If no table names are specified, then simply return the names of the possible databases for the user to pic
    cat("Specify at least one table to pull from: \n")
    return(print(tableNames))
  }

  namesMismatch <- !tables %in% tableNames
  if (any(namesMismatch)) {
    stop(sQuote(tables[which(namesMismatch)]), " does not exist in the database.")
  }

  # Apply the dbReadTable to each readable table in db
  returnedTables <- tryCatch(mapply(dbReadTable,
                                    name = tables,
                                    MoreArgs = list(conn = con),
                                    SIMPLIFY = F),
                             error = function(cond) {

                               errorCode <- gsub("\033\\[[0-9;]*[a-zA-Z]|\033\\]8;;\\a|\033\\]8;.*?\\a|\\n", "",
                                                 cond)

                               if (grepl("42000.*no read permission", errorCode, perl = T) &
                                   any(grepl("^MSys", tables))) {

                                 message('You are asking for a system table but do not have permissions. Opening the database file to allow you to do so.')

                                 shell.exec(dbGetInfo(con)$dbname)

                                 if (isTRUE(retry)) {
                                   message('Enable content, `Ctrl + g`, paste in: \nCurrentProject.Connection.Execute "GRANT SELECT ON MSysRelationships TO Admin;" \nHit `Enter`. Will retry once after 25 seconds.')
                                   Sys.sleep(25)
                                   cat("Retrying...")
                                   df <- mapply(dbReadTable,
                                                name = tables,
                                                MoreArgs = list(conn = con),
                                                SIMPLIFY = F)
                                   return(df)
                                 }
                                 stop('Enable content, `Ctrl + g`, paste in: \nCurrentProject.Connection.Execute "GRANT SELECT ON MSysRelationships TO Admin;" \nHit `Enter`, exit file, and rerun this code.', call. = F)

                               } else {
                                 stop(cond)
                               }
                             })

  if (rBit == "x64" & officeBit == "x32") {
    saveRDS(returnedTables, file = file.path(out, "savedAccessTables.rds"))
  } else {
    returnedTables
  }
}

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
#'
#' @return The full path to the final, ready-to-use file (unzipped if necessary).
#'
#' @noRd
#' @importFrom httr GET HEAD headers progress timeout write_disk stop_for_status
#' @importFrom utils browseURL unzip
#' @keywords internal
getFile <- function(file, open = FALSE, timeout = NULL) {

  # --- 1. Determine if the file is a URL or local path ---
  isUrl <- grepl("^https?://", file, ignore.case = TRUE)
  fileName <- basename(file)
  filePath <- if (isUrl) file.path(tempdir(), fileName) else file

  # --- 2. Handle URL Downloads (if necessary) ---
  if (isUrl && !file.exists(filePath)) {
    message("Downloading file from URL: ", sQuote(file))

    # Read R's global option if set, otherwise default to 60s
    defaultTimeout <- getOption("timeout", 60)
    timeOut <- defaultTimeout

    # If no explicit timeout is provided, calculate it dynamically
    if (is.null(timeout)) {
      try({
        head_response <- HEAD(file)
        if (!is.null(headers(head_response)$`content-length`)) {
          fileSize <- as.numeric(headers(head_response)$`content-length`) / 1024^2
          # Dynamic calculation: 1 second per MB, with the global default as the floor
          timeOut <- max(defaultTimeout, ceiling(fileSize))
          message(sprintf("File size is approx %.2f MB. Setting download timeout to %d seconds.", fileSize, timeOut))
        }
      }, silent = TRUE)
    } else {
      timeOut <- as.numeric(timeout)
      message(sprintf("Using user-specified download timeout of %d seconds.", timeOut))
    }

    # B. Perform the download using httr::GET for robustness
    tryCatch({
      response <- GET(
        url = file,
        write_disk(filePath, overwrite = TRUE), # Save directly to disk
        progress(), # Display a progress bar
        timeout(timeOut) # Use the calculated timeout
      )

    }, error = function(e) {
      # Clean up partially downloaded file on error
      if (file.exists(filePath)) file.remove(filePath)
      stop(sprintf("Failed to download file. Error: %s", conditionMessage(e)), call. = FALSE)
    })

  }

  # --- 3. Handle Zip File Extraction ---
  finalPath <- filePath

  if (grepl("\\.zip$", fileName, ignore.case = TRUE)) {
    # List contents to find the target Access database
    zip_contents <- utils::unzip(filePath, list = TRUE)
    targetFile <- zip_contents$Name[grepl("(\\.accdb|\\.mdb)$", zip_contents$Name, ignore.case = TRUE)]

    if (length(targetFile) == 0) {
      stop("No Access file (.accdb or .mdb) was found in the .zip archive.")
    }
    # Handle cases with multiple matches (take the first one)
    if (length(targetFile) > 1) {
      warning(paste("Multiple Access files found, using the first one:", targetFile[1]), call. = FALSE)
      targetFile <- targetFile[1]
    }

    extractedPath <- file.path(tempdir(), targetFile)

    # Unzip only if the target file doesn't already exist
    if (!file.exists(extractedPath)) {
      message("Extracting file: ", sQuote(targetFile), " from the zip archive.")
      unzip(filePath, files = targetFile, exdir = tempdir(), overwrite = TRUE)
    }

    finalPath <- extractedPath
  }

  # --- 4. Open File or Return Path ---
  if (!file.exists(finalPath)) {
    stop("Could not find the final file at path: ", finalPath, call. = FALSE)
  }

  if (isTRUE(open)) {
    message("Opening file: ", sQuote(basename(finalPath)))
    # Use browseURL for cross-platform compatibility (replaces shell.exec)
    browseURL(finalPath)
  }

  return(finalPath)
}

#' Connect to an Access database
#'
#' @description
#' Create the connection to an Access database and pull the requested tables. This function will attempt to account for mismatched architectures (R vs Microsoft Access) but will take longer to execute if so.
#'
#'
#' @param file File path to the Access database file. Can be a path to a hard
#' drive or a URL.
#' @param tables A vector of table names to determine which relational tables
#' to pull. This can be left blank to provide a list of options. If a system
#' table is provided, you may need to provide read permission before it will
#' work. This has to be done in the Access DB itself. Open the file, select
#' "Enable Content" if prompted, `Ctrl + G`, paste in the "Immediate" window:
#' `CurrentProject.Connection.Execute "GRANT SELECT ON MSysRelationships TO Admin;"`,
#' and run the command by pressing `Enter` before exiting Access database.
#' @param path32 File path to your 32 bit R executable, `Rscript.exe`. Only needed
#' if you're using 32-bit Office.
#' @param ... Additional arguments to be passed onto `connectAccess()`. Used to
#' pass on a specific driver if the default Access driver does not work, a user
#' name, or password.
#'
#' @importFrom httr HEAD headers
#'
#' @return A list of relational tables read from the Access database connection.
#' @export
#'
#' @examples
#' \dontrun{
#' bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip")
#'
#' bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
#' tables = c("Catch", "FishCodes", "Lengths", "Meter Corrections",
#' "SLS Stations", "Tow Info", "Water Info"))
#' }
bridgeAccess <- function(file, tables = "check",
                         path32 = "default",
                         ...) {

  retry <- if (is.null(list(...)$retry)) FALSE else list(...)$retry

  bitCheck <- architectureCheck(path32 = path32)

  file <- getFile(file, open = F)

  out <- tempdir()

  if (isTRUE(bitCheck$check)) {
    con <- connectAccess(file, ...)

    extractTables(con = con,
                  tables = tables,
                  rBit = bitCheck$rBit,
                  officeBit = bitCheck$officeBit,
                  out = out,
                  retry = retry)
  } else {
    script <- shQuote(normalizePath(system.file("internal", "connectAccessTerminal.R",
                                                package = "deltadata"), winslash = "\\", mustWork = T))

    params <- list(
      file = normalizePath(file, winslash = "\\", mustWork = T),
      rBit = bitCheck$rBit,
      officeBit = bitCheck$officeBit,
      out = out,
      retry = retry,
      tables = tables,
      extraArgs = list(...)
    )

    paramFile <- tempfile(pattern = "access_params_", fileext = ".rds")
    saveRDS(params, file = paramFile)
    on.exit(unlink(paramFile), add = TRUE)

    path32Exe <- bitCheck$path32

    # Run the 32-bit Rscript. Paths are quoted to safely handle directory structures with spaces.
    terminalOutput <- system2(
      command = path32Exe,
      args = c(script, paramFile)
    )

    if (!any(tables %in% "check")) {
      readRDS(file.path(out, "savedAccessTables.rds"))
    }
  }
}
