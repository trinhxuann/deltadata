# Find the Nth nearest CDEC station with specific data

Identifies the n-th nearest CDEC station to one or more input
coordinates. It can filter station based on a specific sensor number or
by a general variable and water column combination. By default, all CDEC
station are searched.

## Usage

``` r
calcNearestCDEC(
  df,
  n = 1,
  sensor = NULL,
  variable = c("temp", "turbidity", "ec"),
  waterColumn = c("top", "bottom"),
  method = c("fast", "accurate"),
  verbose = T,
  cdecGPS = NULL,
  cdecMetadata = NULL
)
```

## Arguments

- df:

  A data.frame with at least 'lat' and 'lon' columns. All other columns
  will be retained.

- n:

  The rank of the nearest station to find (e.g., n = 1 for the closest).
  If n = "all", all applicable station and metadata will be returned in
  a nested format per point.

- sensor:

  An optional integer sensor number. If provided, this will override the
  'variable' and 'waterColumn' arguments.

- variable:

  The type of sensor data required. One of "temp", "turbidity", "ec".
  Ignored if 'sensor' is provided.

- waterColumn:

  The position of the sensor in the water column. One of "top" or
  "bottom". Ignored if 'sensor' is provided.

- method:

  Determines the distance calculation function. `fast` will utilize the
  Haversine method, while `accurate` the Vincenty Ellipsoid method. For
  use within the Delta, the Haversine method is sufficient and is much
  less computationally intensive. Defaults to `fast`.

- verbose:

  Set to FALSE to disable specific messages. Defaults to TRUE.

- cdecGPS:

  Internal package data with station GPS locations. Can provide this if
  the internal package data is not updated, although you will have to
  adhere to formatting.

- cdecMetadata:

  Internal package data with sensor metadata. Can provide this if the
  internal package data is not updated, although you will have to adhere
  to formatting.

## Value

A data.frame containing the input point identifiers, the found CDEC
station, the distance, and the sensor metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)

calcNearestCDEC(df)
} # }
```
