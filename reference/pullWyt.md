# Scrape water year type data from DWR

Scrape water year type data from DWR

## Usage

``` r
pullWyt(url = "https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST")
```

## Arguments

- url:

  URL of the historical Water Supply Index (WSIHIST) website

## Value

Returns a list of three tables, reconstructed wyt, eight river runoff,
and official wyt classification.

- reconstructedWyt:

  All information from the Sacramento and San Joaquin Valleys water year
  type indices table

- eightRiver:

  All information from the Eight River Runoff table

- officialWyt:

  Contains only the wyt index and classification for the Sacramento and
  San Joaquin River Valleys

## Examples

``` r
if (FALSE) { # \dontrun{
pullWyt()
} # }
```
