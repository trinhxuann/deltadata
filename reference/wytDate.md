# Water year type of a date

Water year type of a date

## Usage

``` r
wytDate(date, wyt, valley = c("sac", "sjr"), value = c("wyt", "index"))
```

## Arguments

- date:

  A single or vector of dates, in date format

- wyt:

  A data frame of the official wyt from the `pullWyt` function

- valley:

  Which system, either "sac" or "sjr"

- value:

  What value to return, either "wyt" or "index"

## Value

Returns water year type index or classification associated with the
date(s)

## Examples

``` r
if (FALSE) { # \dontrun{
wytTable <- pullWyt()
randomDates <- sample(0:5000, 100, replace = TRUE) + as.Date("2010-01-01")
wytDate(randomDates, wyt = wytTable$officialWyt)
} # }
```
