# Scrape sensor list from CDEC webpage

Scrape sensor list from CDEC webpage

## Usage

``` r
readCdecSensorList(
  url = "https://cdec.water.ca.gov/reportapp/javareports?name=SensList",
  naStrings = c("N/A", "XX", "xxx"),
  naThreshold = 0.9,
  maxSearchRows = 15
)
```

## Arguments

- url:

  Webpage to SENSLIST

- naStrings:

  Vector of potential NAs used in the table

- naThreshold:

  Threshold beyond which a column will be removed from the output

- maxSearchRows:

  Max number of rows to search through to find: the header row and data
  start row

## Value

Data frame

## Examples

``` r
if (FALSE) { # \dontrun{
readCdecSensorList()
} # }
```
