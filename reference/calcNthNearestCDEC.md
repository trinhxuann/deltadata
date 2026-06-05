# Find the n-th closest CDEC gage

**\[deprecated\]** Identifies the n-th nearest CDEC gage to a lat/lon of
interest. This function requires metadata of all CDEC station of
interest. By default, all CDEC station are used.

## Usage

``` r
calcNthNearestCDEC(
  df,
  n = 1,
  cdecGPS = cdecStation,
  cdecMetadata = cdecMetadata,
  variable = c("temp", "turbidity", "ec"),
  waterColumn = c("top", "bottom")
)
```

## Arguments

- df:

  A data frame that contains at least the lat/lon of station(s) of
  interest, named as `lat` and `lon`.

- n:

  A number reflecting the desired relative distance of the CDEC gage
  from the lat/lon of interest. n=1 means return the closest gage, n=2
  means return the second closest gage, etc. n should be an integer or
  should otherwise be convertible to an integer.

- cdecGPS:

  A data frame containing the GPS coordinates of the CDEC gages of
  interest, as `lat` and `lon`.

- cdecMetadata:

  A data frame containing the metadata table of the CDEC gages of
  interest. This table must match the format provided by the DWR
  website. It is recommended to use
  [`pullMetadataCDEC()`](https://trinhxuann.github.io/deltadata/reference/pullMetadataCDEC.md)
  to get this data.

- variable:

  The water quality variable of interest. Currently only supports water
  temperature as `temp`, turbidity as `turbidity`, and
  electro-conductivity as `ec`. This defaults to water temperature.

- waterColumn:

  Where in the water column should the variable of interest be
  prioritized? Supports only `top` and `bottom`, defaulting to `top`.
  For now, top data will be used in the calculation even if you ask for
  bottom data.

## Value

A data frame of the metadata of the n-th closest CDEC station to your
point of interest that has data for the variable of interest.

## Note

This functionality has been incorporated into
[`calcNearestCDEC`](https://trinhxuann.github.io/deltadata/reference/calcNearestCDEC.md).

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)

calcNthNearestCDEC(df)
} # }
```
