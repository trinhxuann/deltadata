# Pulling CDEC gage data

This function will first pull the metadata of a CDEC gage. This allows
the function to direct the user accordingly if various arguments are
missing. Once all arguments are provided, the query is created and data
downloaded.

## Usage

``` r
pullCDEC(
  station,
  sensor = NULL,
  duration = c("event", "hourly", "daily"),
  dateStart,
  dateEnd = NULL,
  temperatureUnits = c("C", "F"),
  coordinates,
  verbose = T,
  maxAttempt = 3,
  fallbackDuration = FALSE
)
```

## Arguments

- station:

  A character vector of station names. Can be multiple station.

- sensor:

  A singular sensor value of interest.

- duration:

  The duration data, can be `event`, `hourly`, or `daily` and depends on
  data availability for the gage of interest.

- dateStart:

  Beginning date for the period of interest.

- dateEnd:

  Ending date for the period of interest. Will default to today if left
  as `NULL`

- temperatureUnits:

  Either `C` or `F` to convert temperature. Only applicable to
  temperature data.

- coordinates:

  A vector of length = 2 containing the lat and lon, in that order. This
  argument can be used instead of `station`. See 'Details' for
  additional comments.

- verbose:

  Logical. Should the function provide a narrative of its operations?
  Defaults to `TRUE`.

- maxAttempt:

  Number of retries to attempt if a connection fails. Defaults to 3.

- fallbackDuration:

  Logical. Should the function try to use a coarser duration if data
  cannot be found? Order is event \> hourly \> daily, in that order,
  never backwards.

## Value

A data frame of the requested data pull.

## Details

The `coordinates` argument can be used in place of the `station`
argument. The
[`calcNearestCDEC`](https://trinhxuann.github.io/deltadata/reference/calcNearestCDEC.md)
function will be used to calculate the nearest CDEC station to your
point of interest and pull data from that gage. use that function if you
are specifically only interested in the metadata of the nearest CDEC
gage.

## Examples

``` r
if (FALSE) { # \dontrun{
pullCDEC("MAL")
pullCDEC("MAL", 25, "hourly", "06/13/1986", "06/14/1986")
# If coordinates are used instead, must specify the argument names.
pullCDEC(coordinates = c(38.04281, -121.9201), sensor = 25,
duration = "hourly", dateStart = "06/13/1986", dateEnd = "06/14/1986")
# Can specify multiple coordinates, just like you can with multiple station names
pullCDEC(coordinates =
data.frame(c(38.04281, 38.04281),
c(-121.9201, -121.9203)),
sensor = 25, duration = "hourly", dateStart = Sys.Date() - 5)
} # }
```
