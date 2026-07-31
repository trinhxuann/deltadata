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
