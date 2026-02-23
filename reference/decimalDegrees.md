# Convert GPS Coordinates to Decimal Degrees

This function attempts to convert latitude and longitude coordinates to
decimal degrees. It supports two formats: degrees minutes seconds (DMS)
and degrees decimal minutes (DDS).

## Usage

``` r
decimalDegrees(x, type = c("dms", "ddm"), isLongitude = FALSE)
```

## Arguments

- x:

  A value or vector of latitude or longitude in dms or ddm format
  separated by a whitespace

- type:

  Either "dms" or "ddm". The lat/lon format of the input.

- isLongitude:

  T/F. Will expect the first set of number to have 3 digits. Will assign
  the value as negative and is specific only to the Bay-Delta area.

## Value

A numeric vector in decimal degrees

## Examples

``` r
gpsDF <- data.frame(
Latitude = paste(c(rep(38, 7)), c(2, 3, 3, 4, 4, 3, 5), c(34.4, 37.1, 49, 35, 16, 39.9, 57.2)),
Longitude = paste(c(rep(122, 7)), c(2, 3, 3, 4, 4, 3, 5), c(34.4, 37.1, 49, 35, 16, 39.9, 57.2))
)

decimalDegrees(gpsDF$Latitude, type = "dms")
#> [1] 38.04289 38.06031 38.06361 38.07639 38.07111 38.06108 38.09922
decimalDegrees(gpsDF$Longitude, type = "ddm", isLongitude = TRUE)
#> [1] -125.9067 -127.6183 -127.8167 -129.2500 -128.9333 -127.6650 -131.2867
```
