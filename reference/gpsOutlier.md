# Isolate outlying stations

Identify stations that are more than `d` distance in miles, defaulting
to 0.5, away from the theoretical coordinate.

## Usage

``` r
gpsOutlier(df, d = 0.5, returnAll = F)
```

## Arguments

- df:

  A dataframe with 6 required columns: date, station, legend, layer,
  lat, and lon. See
  [`plotGPS`](https://trinhxuann.github.io/deltadata/reference/plotGPS.md)
  for details.

- d:

  Miles threshold to call a coordinate outlying. This distance is
  measured as the crow flies.

- returnAll:

  Logical, to return all rows or not. Defaults to F, returning only the
  outlying points

## Value

A data frame with all outlying coordinates.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(date = 2023,
station = c(508, 513, 520, 801, 801),
legend = c(rep("Theoretical", 4), "StartTow"),
layer = c(rep("Theoretical", 4), "13"),
lat = c(38.04717, 38.05886, 38.03217, 38.04369, 38.05500),
lon = c(-121.9172, -121.8677, -121.8631, -121.8440, -121.8487))

gpsOutlier(df, d = 0.5)
} # }
```
