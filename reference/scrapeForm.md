# Scrape, fill, and submit a static html form from the SacPas website

The function allows users to submit a form query and return the
resulting session. If no field arguments are specified, the function
will return all available fields and their options for users to select.
This function was created with the SacPas webpages in mind but should
theoretically work for any webpage with a static form interface.

## Usage

``` r
scrapeForm(url, formIndex = NULL, ..., returnForm = F)
```

## Arguments

- url:

  URL containing the form(s) of interest

- formIndex:

  This argument is only required if there are more than 1 form on the
  webpage. A numeric index indicating the form of interest

- ...:

  Field names with their appropriate options to submit as a query. Can
  be left empty to get back a list of available fields and their
  options. Fields allowing multiple selections should be provided as a
  vector.

- returnForm:

  Logical, will return the html form if TRUE

## Value

An `rvest_session`

## Examples
