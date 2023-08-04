
This package contains various workflow functions for working with data
within the Sacramento-San Joaquin River Delta. There is a primary focus
on IEP (Interagency Ecological Program) Surveys and supporting their
data publication workflow. Additional features pertaining to other
surveys may be supported in the future.

``` r
library(deltadata)
#> The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, will retire in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> The sp package is now running under evolution status 2
#>      (status 2 uses the sf package in place of rgdal)
```

## Installation

The `deltadata` package can be installed from its GitHub repository.

``` r
# Install `devtools` if we need to
install.packages("devtools")

# Installing `deltadata` if we do not already have it
  devtools::install_github("trinhxuann/deltadata")
```

## Reading in the data

The `bridgeAccess()` function can be used to connect to an Access
database and extract any table of interest. First and foremost, we will
need a 32-bit version of R for this function to work due to the
limitations of the odbc drivers. Once that is satisfied, we can work in
64-bit R (32-bit R will be invoked when appropriate).

The function can directly pull from a URL or connect to a file on our
hard drive. When downloading from a URL, it is suggested to download
from a zipped source to minimize the file size–the function will extract
the Access file from the zipped file. If we only provide the file path
in the function, a table of available tables will be provided to us to
explore. The function can also access system tables, however, we will be
prompted to provide file permissions before the function can pull those
tables.

First, download the database (zipped form) and explore what tables are
available to extract: the file will only be downloaded once if we remain
in the same R session (downloaded to the temporary folder)

``` r
bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip")
#> Extracting file: 'SLS.mdb' from the zip file.
```

SLS has 7 main relational tables of interest:

``` r
slsTables <- bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
                          tables = c("Catch", "FishCodes", "Lengths", "Meter Corrections",
                                     "SLS Stations", "Tow Info", "Water Info"))
```

We can also download the relationship table. This is an important table
that reflects the relational structure of the relational tables.
Unfortunately, this table does require special permissions, but
fortunately, we can easily give ourselves the necessary permission. When
we first try to pull the table, an error is given and the Access file
will be opened. The function will output in the console’s error message
isntructions on how to give ourselves permissions: “Enable content,
`Ctrl + g`, enter
`CurrentProject.Connection.Execute "GRANT SELECT ON MSysRelationships TO Admin;"`,
`Enter`, exit file, and rerun this code.”

``` r
schema <- bridgeAccess(
  "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
  tables = c("MSysRelationships"), retry = T
  )

# The function outputs a list, of which we can index to grab just the table
schema <- schema[[1]]
```

## Data joining

The schema table contains instructions on how to join our relational
tables together. The `schemaJoin()` function deciphers this table and
does the joining for us. This approach is theoretically preferable
compared to joining by hand as it removes any guesswork on how we should
join the tables together–this is not absolute since it depends on the
database being set up correctly in the first place.

``` r
joinedData <- schemaJoin(schema, slsTables)
#> inner_join 'Catch' with 'Lengths' via columns 'Date' 'Station' 'Tow' 'FishCode' and 'Date' 'Station' 'Tow' 'FishCode' 
#> inner_join 'Catch' with 'Tow Info' via columns 'Date' 'Station' 'Tow' and 'Date' 'Station' 'Tow' 
#> inner_join 'Tow Info' with 'Water Info' via columns 'Date' 'Station' and 'Date' 'Station'
```

The `schemaJoin()` function provides a narration of the join type, the
two relational tables being joined, and the column keys the join is
occurring on. We can double check this narration to ensure that we are
getting the expected joins. If these are not the joins that we want, the
function can take in any schema table. We can simply modify what join
type we would like the function to use by specifying the `joinType`
column in the schema table. The four join options are: “full_join”,
“inner_join”, “left_join”, and “right_join”.

``` r
schemaFullJoin <- schema
schemaFullJoin$joinType <- "full_join"
joinedDataFullJoin <- schemaJoin(schemaFullJoin, slsTables)
#> full_join 'Catch' with 'Lengths' via columns 'Date' 'Station' 'Tow' 'FishCode' and 'Date' 'Station' 'Tow' 'FishCode' 
#> full_join 'Catch' with 'Tow Info' via columns 'Date' 'Station' 'Tow' and 'Date' 'Station' 'Tow' 
#> full_join 'Tow Info' with 'Water Info' via columns 'Date' 'Station' and 'Date' 'Station'

# Would expect number of rows to differ between an inner vs full join
data.frame(joinType = c("auto", "fullJoin"),
           nRows = c(nrow(joinedData), nrow(joinedDataFullJoin)),
           nCols = c(ncol(joinedData), ncol(joinedDataFullJoin)))
#>   joinType  nRows nCols
#> 1     auto 195473    33
#> 2 fullJoin 195648    33
```

### Notes on `schemaJoin()`

Besides supplying our own join type to the schema table, we can also
provide manipulated relational tables to the function. There are several
reasons why this may be required: 1) the columns were not read in
correctly, e.g., date data as characters, 2) column names need to
change, 3) unit conversion of a data column, or 4) other reasons.
Ultimately, this function simply looks at the schema table and applies
those joins to the provided data–we can manipulate either argument as we
would like.

## Checking GPS coordinate outliers

Two functions are dedicated to checking GPS coordinates for outliers:

1.  `plotGPS()`: plots sampling GPS coordinates on a leaflet map.
    Requires a data frame with 6 columns, `date`, `station`, `legend`,
    `layer`, `lat`, and `lon.` The `legend` column determines the color
    labels for the plotted points, and it is recommended that one group
    be the theoretical coordinates each of sampling station. The `layer`
    column determines the layer control of the output map and is
    generally the survey number.
2.  `gpsOutlier()`: returns a data frame of GPS coordinates that are
    beyond a specified distance (default is 0.5 mile, measured as the
    crow flies) from each point’s theoretical coordinates. This function
    also requires a data frame with 6 columns, the same requirement as
    `plotGPS()`, with a firm requirement for a “Theoretical” group of
    GPS coordinates.

``` r
gpsDF <- slsTables[["Water Info"]]
gpsDF$SeasonYear <- as.numeric(format(gpsDF$Date, format = "%Y")) + 
  (as.numeric(format(gpsDF$Date, format = "%m")) > 11)
gpsDF <- subset(gpsDF, SeasonYear == 2023)

# Split lat/lon into degrees
for (i in c("StartLat", "StartLong", "EndLat", "EndLong")) {
  gpsDF[[i]] <- gsub("[-. ]", "", gpsDF[[i]])
}

# Creating a function to do this since we'll be reusing it later:
createGPS <- function(data) {

  data$StartLatD <- as.numeric(substr(data$StartLat, 1, 2))
  data$StartLatM <- as.numeric(substr(data$StartLat, 3, 4))
  data$StartLatS <- as.numeric(substr(data$StartLat, 5, 7))
  data$EndLatD <- as.numeric(substr(data$EndLat, 1, 2))
  data$EndLatM <- as.numeric(substr(data$EndLat, 3, 4))
  data$EndLatS <- as.numeric(substr(data$EndLat, 5, 7))
  data$StartLonD <- as.numeric(substr(data$StartLong, 1, 3))
  data$StartLonM <- as.numeric(substr(data$StartLong, 4, 5))
  data$StartLonS <- as.numeric(substr(data$StartLong, 6, 8))
  data$EndLonD <- as.numeric(substr(data$EndLong, 1, 3))
  data$EndLonM <- as.numeric(substr(data$EndLong, 4, 5))
  data$EndLonS <- as.numeric(substr(data$EndLong, 6, 8))
  # Note the seconds calculation here. 36000 due to format of the LatS columns
  data$Start_lat <- data$StartLatD + data$StartLatM/60 + data$StartLatS/36000
  data$End_lat <- data$EndLatD + data$EndLatM/60 + data$EndLatS/36000
  data$Start_lon <- -(data$StartLonD + data$StartLonM/60 + data$StartLonS/36000)
  data$End_lon <- -(data$EndLonD + data$EndLonM/60 + data$EndLonS/36000)

  data
}

gpsDF <- createGPS(gpsDF)
# Required columns for plotGPS()
gpsDF$date <- gpsDF$SeasonYear
gpsDF$layer <- gpsDF$Survey

# GPS start coordinates
gpsLat<- gpsDF[, which(!names(gpsDF) %in% c("Start_lon", "End_lon"))]
gpsLat <- reshape(gpsLat, direction = "long",
                        varying = c("Start_lat", "End_lat"),
                        v.names = "lat",
                        times = c("Start", "End"),
                        timevar = "legend")

gpsLon<- gpsDF[, which(!names(gpsDF) %in% c("Start_lat", "End_lat"))]
gpsLon <- reshape(gpsLon, direction = "long",
                  varying = c("Start_lon", "End_lon"),
                  v.names = "lon",
                  times = c("Start", "End"),
                  timevar = "legend")

gpsDFLong <- cbind(gpsLat, lon = gpsLon$lon)

# Joining in the theoretical coordinates
gpsTheoretical <- slsTables$`SLS Stations`

for (i in c("LatD", "LatM", "LatS", "LonD", "LonM", "LonS")) {
  gpsTheoretical[[i]] <- as.numeric(gpsTheoretical[[i]])
}

gpsTheoretical$lat <- gpsTheoretical$LatD + gpsTheoretical$LatM/60 + 
  gpsTheoretical$LatS/3600
gpsTheoretical$lon <- -(gpsTheoretical$LonD + gpsTheoretical$LonM/60 + 
                          gpsTheoretical$LonS/3600)
gpsTheoretical$legend <- "Theoretical"
gpsTheoretical$layer <- "Theoretical"
gpsTheoretical$date <- NA

gpsData <- rbind(gpsDFLong[, c("date", "Station", "legend", "layer", "lat", 
                               "lon")],
                 gpsTheoretical[, c("date", "Station", "legend", "layer", "lat", 
                                    "lon")])

# Can leave the NAs in, but plotGPS will throw back warnings about them
gpsData <- subset(gpsData, !is.na(lat) | !is.na(lon))

# layerName and dateName are specific to the popup labels at each point.
plotGPS(gpsData, layerName = "Survey", dateName = "Year", height = 500)
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

`plotGPS()` allows us to quickly visualize our GPS coordinates. It is
most useful when we also visualize the theoretical coordinates alongside
our points of interest. The outputted leaflet map has several useful
features: 1) an interactive map in which we can zoom in and out and move
around, 2) a legend is provided if we have multiple groupings of data
points, 3) layer controls allow us to plot only the layers we want, and
4) each point is labeled and provides additional data when clicked upon.
This function should allow us to quickly see the most egregious
outliers.

We can then do a finer search for outliers by using `gpsOutlier()`. This
function calculates the distance between each sampling point and that
location’s theoretical GPS coordinates and returns any points above the
distance threshold (defaults to 0.5 mile). This implies that we must
supply theoretical GPS coordinates of our sampling points to the
function and is why it is recommended to construct our GPS data frame to
include the theoretical when using `plotGPS()`. The data frame output
for `gpsOutlier()` is the same format as required by `plotGPS`, allowing
us to pipe the two together.

``` r
# By default, d = 0.5
gpsOutliersData <- gpsOutlier(gpsData)

plotGPS(gpsOutliersData, height = 500)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r

head(gpsOutliersData)
#>           date Station legend layer      lat       lon lonTheoretical
#> 413.End   2023     723    End     6 38.69222 -121.6719      -121.6731
#> 389.End   2023     330    End     5 38.01586 -122.3822      -122.3282
#> 389.Start 2023     330  Start     5 38.02122 -122.3821      -122.3282
#> 130.End   2023     336    End     1 38.05789 -122.3248      -122.2780
#> 130.Start 2023     336  Start     1 38.05925 -122.3032      -122.2780
#> 127.End   2023     329    End     1 38.06406 -122.3238      -122.3040
#>           latTheoretical  distance outlier
#> 413.End         38.23725 31.382077    TRUE
#> 389.End         38.02853  3.075955    TRUE
#> 389.Start       38.02853  2.987429    TRUE
#> 130.End         38.06111  2.560575    TRUE
#> 130.Start       38.06111  1.378380    TRUE
#> 127.End         38.06361  1.078905    TRUE
```

Note that the distance calculated between the sampling point and the
theoretical coordinates is “as the crow flies”, i.e., a straight line
(taking into account the curvature of the Earth). The default 0.5 mile
is the ideal trawling condition for most CDFW survey trawls.

## Comparing water quality values to CDEC gages

`popCDEC()` is used to populate a data frame of sampling points with
water temperature, water turbidity, or water electroconductivity data
from the nearest CDEC gage. Only these three water quality metrics are
support currently. The input data frame requires at least five
columns: 1) `station` (name of sampling point), 2) `lat` (latitude of
sampling point), 3) `lon` (longitude of sampling point), 4) `time`
(date-time of sample), and 5) the water quality variable of interest
(`temp`, `ec`, or `turbidity`).

``` r
# Using the joined SLS data as an example
waterQualityData <- subset(
  joinedData, format(Date, format = "%Y") == "2023" &
    Survey == 1 & (!is.na(StartLat) | !is.na(StartLong))
  )

# Split lat/lon into degrees
for (i in c("StartLat", "StartLong", "EndLat", "EndLong")) {
  waterQualityData[[i]] <- gsub("[-. ]", "", waterQualityData[[i]])
}

# createGPS is a helper function defined on line 108 above.
waterQualityData <- createGPS(waterQualityData)
waterQualityData$time <- paste(
  waterQualityData$Date, 
  format(waterQualityData$Time, format = "%H:%M:%S")
  )
waterQualityData <- unique(waterQualityData[, c("Station", "Start_lat", 
                                                "Start_lon", "time",
                                                "TopTemp", "TopEC", "NTU")])
waterQualityData <- unique(waterQualityData[, names(waterQualityData)])
names(waterQualityData) <- c("Station", "lat", "lon", "time", 
                             "temp", "ec", "turbidity")

startTime <- Sys.time()
cdecTable <- popCDEC(waterQualityData)
totalTime <- Sys.time() - startTime

head(cdecTable)
#>   station      lat       lon                time cdecStation temp tempCDEC
#> 1     306 38.00064 -122.4145 2023-01-03 11:36:00         MRZ 10.4 9.555556
#> 2     308 38.01542 -122.4095 2023-01-03 11:14:00         MRZ 10.5 9.555556
#> 3     311 38.02083 -122.3827 2023-01-03 12:01:00         MRZ 10.1 9.555556
#> 4     315 38.03833 -122.3926 2023-01-03 10:52:00         MRZ 10.2 9.555556
#> 5     322 38.02550 -122.3524 2023-01-03 12:27:00         MRZ 10.1 9.555556
#> 6     323 38.03958 -122.2944 2023-01-03 13:14:00         MRZ 10.0 9.500000
#>   timeDifference  distance sensorNumber         sensorDescription units
#> 1         9 mins 15.067258           25 TEMPERATURE, WATER, DEG F DEG C
#> 2        31 mins 14.700119           25 TEMPERATURE, WATER, DEG F DEG C
#> 3         1 mins 13.222769           25 TEMPERATURE, WATER, DEG F DEG C
#> 4        53 mins 13.772100           25 TEMPERATURE, WATER, DEG F DEG C
#> 5         3 mins 11.561585           25 TEMPERATURE, WATER, DEG F DEG C
#> 6         1 mins  8.437388           25 TEMPERATURE, WATER, DEG F DEG C
#>   duration         dataAvailable
#> 1    event 08/21/2008 to present
#> 2    event 08/21/2008 to present
#> 3    event 08/21/2008 to present
#> 4    event 08/21/2008 to present
#> 5    event 08/21/2008 to present
#> 6    event 08/21/2008 to present
```

For each sampling point, the function will return the desired water
quality data from the nearest CDEC station, in terms of space and time.
Each sampling point will be returned in a data frame with the desired
water quality data from the nearest CDEC station. Various metadata is
also reported back by the function: the nearest CDEC station
(`cdecStation`), the difference in minutes between the sampling point of
interest and the CDEC data point (`timeDifference`), the distance in
miles between the sampling point and the CDEC station (`distance`), and
CDEC station metadata information (`sensorNumber`, `sensorDescription`,
`units`, `duration`, `dataAvailable`).

### Notes on nuanced operations by `popCDEC`

There are several operations of the function to be aware of:

1.  the nearest CDEC station is calculated as a as-the-crow-flies
    distance
2.  the reported value from the CDEC station is the closest time point
    to the requested sampling point that has data. This means that if
    the station was sampling at time points closer to when our sampling
    occurred but valid data was not recorded, those time points were
    removed. The reported value is the closest *valid* value in time to
    when our sampling occurred. There may still be values that are NAs;
    these are due to the station not having any valid data within the
    requested date range.
3.  the function scrapes from the CDEC website and can get bogged down
    as we increase the number of data points to populate and/or the
    range of dates of the initial dataset. It is also dependent on the
    internet connection speed to the CDEC website.

### `pullCDEC`

Function `popCDEC` relies on `pullCDEC` in the background to pull data
from CDEC gages. This function can be used as a stand-alone function.

``` r
malGage <- pullCDEC(station = "MAL", sensor = 25, duration = "hourly", 
                    dateStart = "06/13/1986", dateEnd = "06/14/1986")

head(malGage)
#>   stationId duration sensorNumber sensorType            dateTime    obsDate
#> 1       MAL        H           25     TEMP W 1986-06-13 00:00:00 1986-06-13
#> 2       MAL        H           25     TEMP W 1986-06-13 01:00:00 1986-06-13
#> 3       MAL        H           25     TEMP W 1986-06-13 02:00:00 1986-06-13
#> 4       MAL        H           25     TEMP W 1986-06-13 03:00:00 1986-06-13
#> 5       MAL        H           25     TEMP W 1986-06-13 04:00:00 1986-06-13
#> 6       MAL        H           25     TEMP W 1986-06-13 05:00:00 1986-06-13
#>   value dataFlag units
#> 1    NA     <NA> DEG C
#> 2    NA     <NA> DEG C
#> 3    NA     <NA> DEG C
#> 4    NA     <NA> DEG C
#> 5    NA     <NA> DEG C
#> 6    NA     <NA> DEG C
```

The function will pull the requested data into R if all the arguments
are provided. Since it is difficult to know what data is available for a
gage of interest, the function will provide metadata on all available
data if only the station argument is provided, e.g.,
`pullCDEC(station = "MAL")`.

## Conclusion

In this vignette, we explored how to use the functions with `deltadata`
to replicate portions of the SLS QAQC pipeline. `bridgeAccess` is a
convenient wrapper to connect to an Access database, allowing us to pull
out relational tables of interest. The function can also access the
relationship schema, which can be used to guide the joining process of
our desired relational tables via `schemaJoin`. Once these relational
tables are read into R, we can leverage any R package to QAQC our data.
Within `deltadata`, there are currently two specific QAQC functions that
are available: 1) exploring outlying GPS coordinates based on distance
from the theoretical via `plotGPS` and `gpsOutlier`, and 2) comparing
water quality data to the nearest CDEC station via `popCDEC`.

There may be additional functionalities added to this package in the
future. If you have any suggestions or encounter any bugs, please feel
free to contribute to the package via a pull request or directly contact
[Trinh Nguyen](mailto:trinh.nguyen@wildlife.ca.gov).
