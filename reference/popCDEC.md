# Populate the closest CDEC station data.

This function will search for the closest CDEC station to each point
within your data frame and retrieve water tempearture, turbidity, or
electroconductivity data from the closest time point to the sampling
time of your point of interest. If a CDEC gage does not have data of
interest, either in terms of the sensor of interest or at the time point
of interest, `NA` or the value of the next closest time point for that
station will be provided, if available.

## Usage

``` r
popCDEC(
  df,
  cdecClosest = NULL,
  variable = c("temp", "turbidity", "ec"),
  waterColumn = c("top", "bottom"),
  ...
)
```

## Arguments

- df:

  A data frame with your station name (`station`), latitude (`lat`),
  longitude (`lon`), and time (`time`). Ensure that `time` is a
  date-time format, YYYY-MM-DD HH:MM:SS.

- cdecClosest:

  A data frame of the closest station per coordinate of interest. This
  df should include three columns: `cdecGage` (the CDEC station name),
  `sensorNumber` (sensor number that you're interested in), and
  `duration` (the sampling interval of interest). If not provided,
  [`calcNearestCDEC()`](https://github.com/trinhxuann/deltadata/reference/calcNearestCDEC.md)
  will automatically populate the closest cdec station per sampling
  location from `df`.

- variable:

  Which water quality variable are you after. Supports only water
  temperature (`temp`), turbidity (`turbidity`), or electroconductivity
  (`ec`). Will default to `temp`.

- waterColumn:

  Where in the water column to look for sensor data, top (`top`) or
  bottom (`bottom`)? Will default to `top`

- ...:

  Additional parameters to be passed onto
  [`calcNearestCDEC()`](https://github.com/trinhxuann/deltadata/reference/calcNearestCDEC.md).

## Value

A data frame with water quality of interest from the closest CDEC gage
at the closest time stamp.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(station = "306", lat = 38.00064,
lon = -122.4136, time = "2023-01-01 10:00:00", temp = 10)

popCDEC(df)
} # }
```
