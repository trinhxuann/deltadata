# Plot station GPS coordinates with layer control.

This is a leaflet wrapper. The input data frame requires very specific
columns that must be included. The format caters to IEP surveys but
should be generalizable to other datasets.

## Usage

``` r
plotGPS(df, layerName = "Layer", dateName = "Date", height = 1200, ...)
```

## Arguments

- df:

  A dataframe with 6 required columns: date, station, legend, layer,
  lat, and lon. See the "details" section for additional information.

- layerName:

  Character vector used to label the layer element within the pop up at
  each plotted point.

- dateName:

  Character vector used to label the date element within the pop up at
  each plotted point.

- height:

  Height of the leaflet map.

- ...:

  Optional. Currently only used to determine the `provider` argument
  within the addProviderTiles.

## Value

A leaflet plot.

## Details

The input data frame must have at least six specific columns.

`date` represents when the samples were taken and is used to label each
data point in the pop-up label. This does not technically need to be a
date and can be any date-related label that makes sense for your
project, e.g., month or year.

`station` represents the name of the sampling points and is used to
label each data point on the map.

`legend` contains the legend labels and marks the colors of the data
points on the map. This is useful if there are different instances of
the same sampling point, e.g., start and end GPS coordinates.

`layer` determines the layers that are depicted in the layer control of
the plot. Use this to filter data points on the map, e.g., per survey
number.

`lat` contains the latitude coordinates of your sampling point

`lon` contains the longitude coordinates of your sampling point

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(date = 2023,
station = c(508, 513, 520, 801),
legend = c("Theoretical"),
layer = c("Theoretical"),
lat = c(38.04717, 38.05886, 38.03217, 38.04369),
lon = c(-121.9172, -121.8677, -121.8631, -121.8440))

plotGPS(df, layerName = "Survey", dateName = "Year")
} # }
```
