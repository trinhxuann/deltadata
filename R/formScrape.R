#' Find possible fields and their options
#'
#' @description
#' This function finds the fields and their options based on the form supplied
#' by the website. The function will assume that all radio fields that are
#' meant to toggle against each other are named the same.
#'
#' The function will try to flag dynamic components of a form if there is no
#' readable options for that field and it is not a text field.
#'
#' If there are fields that have duplicated names, the function attempts to
#' rename these with a sequential numeric label suffix.
#'
#' @param form html form taken from `rvest::html_form()`
#'
#' @return A list with three components: possible fields after accounting for
#' radio buttons, fields with no readable options, and the name of all fields
#' after accounting for duplicates
#'
#' @noRd
#' @keywords internal
findFieldOptions <- function(form) {

  # Check to see if an option is empty, if so, might be an dynamic component
  valuePerField <- lapply(form$fields, '[[', 'value')

  # Field types
  fieldTypes <- sapply(form$fields, "[[", "type")

  # Radios fields
  radioFields <- lapply(valuePerField[fieldTypes == "radio"], '[[', 1)
  radioOptions <- lapply(split(radioFields, names(radioFields)), unlist, use.names = F)

  # Select fields
  selectOptions <- lapply(form$fields[fieldTypes == "select"], '[[', 'options')

  # Checkbox fields
  checkboxIndices <- which(fieldTypes == "checkbox")
  checkboxFields <- lapply(form$fields[checkboxIndices], function(x) "T/F")

  # If any field has a value of "", it may be a dynamic component. Warn user
  emptyValue <- sapply(valuePerField, function(x) {
    noValue <- x == ""
    if (isTRUE(noValue) || identical(noValue, FALSE)) return(noValue)
    else {
      if (identical(noValue, logical(0))) return(T)
      else {stop("Unaccounted state of emptyValue. Check code")}
    }
  })

  totalFields <- c(radioOptions, selectOptions, checkboxFields)
  totalParsedFields <- names(totalFields)

  if (any(duplicated(totalParsedFields))) {
    totalParsedUnique <- uniqueNames(totalParsedFields)
    names(totalFields) <- totalParsedUnique$uniqueNames

    # Fixing duplicated checkbox names; this is a very specific scenario...hard to generalize
    duplicatedCheckbox <- which(duplicated(names(checkboxIndices)) | duplicated(names(checkboxIndices), fromLast = T))
    names(form$fields)[checkboxIndices[duplicatedCheckbox]] <- totalParsedUnique$firstIndex
  }

  remainingEmptyFields <- setdiff(names(which(emptyValue)), totalParsedFields)

  list(totalFields = totalFields,
       emptyFields = paste0(c("The following field has no readable options: ", paste(remainingEmptyFields, collapse = ", "))),
       formNames = names(form$fields))
}

#' Append duplicated names with a sequential numeric index suffix
#'
#' @param x A character vector of field names
#'
#' @return A list of two: the appended field names and the first index where
#' the duplicated value occurred
#'
#' @noRd
#' @keywords internal
uniqueNames <- function(x) {
  firstIndex <- sapply(unique(x[duplicated(x)]), function(i) {
    matchedIndex <- which(x == i)
    releventIndex <- 2:length(x[matchedIndex])
    # <<- likely frowned on
    x[matchedIndex][releventIndex] <<- paste(i, releventIndex, sep = "_")
    x[matchedIndex[releventIndex]]
  })
  list(uniqueNames = x,
       firstIndex = firstIndex[, 1])
}

#' Scrape, fill, and submit a static html form from the SacPas website
#'
#' @description
#' The function allows users to submit a form query and return the resulting
#' session. If no field arguments are specified, the function will return
#' all available fields and their options for users to select. This function
#' was created with the SacPas webpages in mind but should theoretically work
#' for any webpage with a static form interface.
#'
#' @param url URL containing the form(s) of interest
#' @param formIndex This argument is only required if there are more than 1 form
#' on the webpage. A numeric index indicating the form of interest
#' @param ... Field names with their appropriate options to submit as a query.
#' Can be left empty to get back a list of available fields and their options.
#' Fields allowing multiple selections should be provided as a vector.
#' @param returnForm Logical, will return the html form if TRUE
#'
#' @importFrom rvest session html_form html_form_set session_submit
#'
#' @return An `rvest_session`
#' @export
#'
#' @examples
#' \dontrun{
#' # Find available fields and their options on this webpage:
#' scrapeForm("https://www.cbr.washington.edu/sacramento/data/query_river_graph.html"
#'
#' #' # This specific query has duplicated field names across the checkboxes and a
#' # select field. The function creates a generic label to make these unique,
#' # however, these are uninformative labels. To know which ones you want,
#' # you can look at the website or the form itself. Here, the latter works
#' scrapeForm("https://www.cbr.washington.edu/sacramento/data/query_river_graph.html",
#' returnForm = T)
#'
#' # we want "WaterTempAvg" as the 10 year avg value, which is `data[]_7`:
#'
#' scrapeForm("https://www.cbr.washington.edu/sacramento/data/query_river_graph.html",
#' hafilter = "All", `year[]` = 2023, outputFormat = "graph", tempUnit = "C",
#' `loc[]` = c("RDB", "WLK"), `data[]` = "WaterTemperature",
#' avgyear = 2023, size = "large", `data[]_2` = F, `data[]_3` = F,
#' `data[]_4` = F, `data[]_5` = F, `data[]_6` = F,
#' monochrome = F, datalink = F)
#' }
scrapeForm <- function(url, formIndex = NULL, ..., returnForm = F) {

  session <- rvest::session(url)

  # Check for form existence
  form <- rvest::html_form(session)
  if (length(form) == 0) {
    stop("No static form found on the website.", call. = FALSE)
  }
  # Multiple forms
  if (is.null(formIndex)) {
    if (length(form) > 1) {
      message("More than one form found. Specify which one you want.")
      return(form)
    } else {
      formIndex <- 1
    }
  }
  form <- form[[formIndex]]

  if (returnForm) return(form)

  # Identify fields and their options
  fieldOptions <- findFieldOptions(form)
  # Check if user provided any fields
  if (length(list(...)) == 0) {
    # message("Available fields in the form: ", paste(availableFields, collapse = ", "), "\n")

    print(fieldOptions$totalFields)

    if (fieldOptions$emptyFields[2] != "") message(fieldOptions$emptyFields)
    return(message("Choose appropriate fields and their options based on the above."))
  }

  # Updating the names of the forms after accounting for duplicates
  names(form$fields) <- fieldOptions$formNames

  userFields <- list(...)
  # Check to see if user provided values are actually available

  exists <- lapply(names(userFields), function(x) {
    index <- which(names(form$fields) %in% x)
    type <- unique(sapply(form$fields[index], '[[', 'type'))
    if (type == "radio") {
      exists <- userFields[[x]] %in% sapply(form$fields[index], '[[', 'value')
    } else {
      if (type == "select") {
        exists <- userFields[[x]] %in% sapply(form$fields[index], '[[', 'options')
      } else {
        if (type == "checkbox") exists <- userFields[[x]] %in% c(T, F)
      }
    }
    names(exists) <- x
    exists
  })

  doesNotExists <- unlist(userFields)[!unlist(exists)]
  if (length(doesNotExists) > 0) {
    stop("The following are not valid option fields: ", paste(doesNotExists, collapse = ", "), call. = F)
  }

  # Fill in form fields with user-provided values

  # Radios
  radioFields <- unique(names(form$fields)[sapply(form$fields, function(x) x$type == "radio")])
  fieldNameValue <- sapply(form$fields, function(x) paste(x$name, x$value, sep = "."))
  userNameValue <- paste0(names(userFields), ".", sapply(userFields, '[[', 1))

  form$fields[names(fieldNameValue) %in% names(userFields) &
                !fieldNameValue %in% userNameValue &
                names(fieldNameValue) %in% radioFields] <- NULL
  # Remove the radio fields as already done
  userFields[names(userFields) %in% radioFields] <- NULL

  # Checkboxes
  checkboxFields <- unique(names(form$fields)[sapply(form$fields, function(x) x$type == "checkbox")])
  userCheckbox <- names(userFields)[names(userFields) %in% checkboxFields]

  if (any(sapply(userFields[userCheckbox], function(x) x == F))) {
    form$fields[names(form$fields) %in% names(which(userFields[userCheckbox] == F))] <- NULL
  }

  remainingCheckboxes <- setdiff(checkboxFields, names(userFields))
  if (length(remainingCheckboxes) > 0) {
    warning("The following checkboxes were left unspecified. Defaulting to checked: ",
            paste(setdiff(checkboxFields, names(userFields)), collapse = ", "),
            call. = F)
  }
  userFields[names(userFields) %in% checkboxFields] <- NULL

  # Rest of the fields can just be provided as a list
  filledForm <- rvest::html_form_set(form, !!!userFields)

  # Submit the form
  session <- rvest::session_submit(session, filledForm)

  return(session)
}

