
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
library(kableExtra)
```

## Installation

The `deltadata` package can be installed from its GitHub repository.

``` r
# Install `devtools` if you need to
if (!require("devtools")) {
  install.packages("devtools")
}

# Installing `deltadata` if you do not already have it
if (!require("deltadata")) {
  devtools::install_github("trinhxuann/deltadata")
}
```

## Reading in the data

The `bridgeAccess()` function can be used to connect to an Access
database and extract any table of interest. First and foremost, you will
need a 32-bit version of R for this function to work due to the
limitations of the odbc drivers. Once that is satisfied, you can work in
64-bit R and 32-bit R will be invoked when appropriate.

The function can directly pull from a URL or connect to a file on your
harddrive. If you download from a URL, it is suggested that you download
from a zipped source to minimize the file size–the function will extract
the Access file from the zipped file. If you only provide the file path
in the function, a table of available tables will be provided to you to
explore. The function can also access system tables, however, you will
be prompted to provide file permissions before the function can pull
those tables.

First, download the database (zipped form) and explore what tables are
available to extract: the file will only be downloaded once if you are
in the same R session (downloaded to the temporary folder)

``` r
bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip")
#> Extracting file: 'SLS.mdb' from the zip file.
#> Specify at least one table to pull from: 
#>  [1] "MSysAccessObjects"          "MSysACEs"                  
#>  [3] "MSysNameMap"                "MSysNavPaneGroupCategories"
#>  [5] "MSysNavPaneGroups"          "MSysNavPaneGroupToObjects" 
#>  [7] "MSysNavPaneObjectIDs"       "MSysObjects"               
#>  [9] "MSysQueries"                "MSysRelationships"         
#> [11] "Catch"                      "FishCodes"                 
#> [13] "Lengths"                    "Meter Corrections"         
#> [15] "SLS Stations"               "Tow Info"                  
#> [17] "Water Info"
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
#> You are asking for a system table but do not have permissions. Opening the database file to allow you to do so.
#> Enable content, `Ctrl + g`, enter `CurrentProject.Connection.Execute "GRANT SELECT ON MSysRelationships TO Admin;"`, `Enter`, exit file, and rerun this code.
#> Retrying...

# The function outputs a list, of which we can index to grab just the table
schema <- schema[[1]]
```

## Data joining

The schema table contains instructions on how to join your relational
tables together. The `schemaJoin()` function deciphers this table and
does the joining for us. This approach is theoretically preferable
compared to joining by hand as it removes any guesswork on how you
should join the tables together–this is not absolute since it depends on
the database being set up correctly in the first place.

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
dimCompare <- kable(
  data.frame(joinType = c("auto", "fullJoin"),
             nRows = c(nrow(joinedData), nrow(joinedDataFullJoin)),
             nCols = c(ncol(joinedData), ncol(joinedDataFullJoin)))
  )
dimCompare <- kable_styling(
  dimCompare,
  bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )
```

### Notes on `schemaJoin()`

Besides supplying your own join type to the schema table, you can also
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

`plotGPS()` allows you to quickly visualize your GPS coordinates. It is
most useful when you also visualize the theoretical coordinates
alongside your points of interest. The outputted leaflet map has several
useful features: 1) an interactive map in which you can zoom in and out
and move around, 2) a legend is provided if you have multiple groupings
of data points, 3) layer controls allow you to plot only the layers you
want, and 4) each point is labeled and provides additional data when
clicked upon. This function should allow us to quickly see the most
egregious outliers.

We can then do a finer search for outliers by using `gpsOutlier()`. This
function calculates the distance between each sampling point and that
location’s theoretical GPS coordinates and returns any points above the
distance threshold (defaults to 0.5 mile). This implies that you must
supply theoretical GPS coordinates of your sampling points to the
function and is why it is recommended to construct your GPS data frame
to include the theoretical when using `plotGPS()`. The data frame output
for `gpsOutlier()` is the same format as required by `plotGPS`, allowing
us to pipe the two together.

``` r
# By default, d = 0.5
gpsOutliersData <- gpsOutlier(gpsData)

plotGPS(gpsOutliersData, height = 500)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r

gpsOutliersDataTable <- kable(gpsOutliersData)
gpsOutliersDataTable <- kable_styling(
  gpsOutliersDataTable, 
  bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )
scroll_box(gpsOutliersDataTable, width = "100%", height = "400px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:400px; overflow-x: scroll; width:100%; ">

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
date
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
Station
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
legend
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
layer
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
lat
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
lon
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
lonTheoretical
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
latTheoretical
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
distance
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
outlier
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
413.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
723
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:right;">
38.69222
</td>
<td style="text-align:right;">
-121.6719
</td>
<td style="text-align:right;">
-121.6731
</td>
<td style="text-align:right;">
38.23725
</td>
<td style="text-align:right;">
31.3820766
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
389.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
330
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
38.01586
</td>
<td style="text-align:right;">
-122.3822
</td>
<td style="text-align:right;">
-122.3282
</td>
<td style="text-align:right;">
38.02853
</td>
<td style="text-align:right;">
3.0759550
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
389.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
330
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
38.02122
</td>
<td style="text-align:right;">
-122.3821
</td>
<td style="text-align:right;">
-122.3282
</td>
<td style="text-align:right;">
38.02853
</td>
<td style="text-align:right;">
2.9874287
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
130.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
336
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
38.05789
</td>
<td style="text-align:right;">
-122.3248
</td>
<td style="text-align:right;">
-122.2780
</td>
<td style="text-align:right;">
38.06111
</td>
<td style="text-align:right;">
2.5605751
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
130.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
336
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
38.05925
</td>
<td style="text-align:right;">
-122.3032
</td>
<td style="text-align:right;">
-122.2780
</td>
<td style="text-align:right;">
38.06111
</td>
<td style="text-align:right;">
1.3783803
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
127.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
329
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
38.06406
</td>
<td style="text-align:right;">
-122.3238
</td>
<td style="text-align:right;">
-122.3040
</td>
<td style="text-align:right;">
38.06361
</td>
<td style="text-align:right;">
1.0789050
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
363.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
912
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
37.98172
</td>
<td style="text-align:right;">
-121.3672
</td>
<td style="text-align:right;">
-121.3686
</td>
<td style="text-align:right;">
37.96642
</td>
<td style="text-align:right;">
1.0580260
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
245.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
37.96092
</td>
<td style="text-align:right;">
-121.5296
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
0.8981125
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
127.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
329
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
38.06464
</td>
<td style="text-align:right;">
-122.3193
</td>
<td style="text-align:right;">
-122.3040
</td>
<td style="text-align:right;">
38.06361
</td>
<td style="text-align:right;">
0.8391130
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
105.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
801
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
13
</td>
<td style="text-align:right;">
38.05500
</td>
<td style="text-align:right;">
-121.8487
</td>
<td style="text-align:right;">
-121.8440
</td>
<td style="text-align:right;">
38.04369
</td>
<td style="text-align:right;">
0.8207123
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
142.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
37.96308
</td>
<td style="text-align:right;">
-121.5299
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
0.7917561
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
121.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
311
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
38.01889
</td>
<td style="text-align:right;">
-122.3916
</td>
<td style="text-align:right;">
-122.3774
</td>
<td style="text-align:right;">
38.02086
</td>
<td style="text-align:right;">
0.7848387
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
200.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
336
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
38.06161
</td>
<td style="text-align:right;">
-122.2919
</td>
<td style="text-align:right;">
-122.2780
</td>
<td style="text-align:right;">
38.06111
</td>
<td style="text-align:right;">
0.7566492
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
78.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
13
</td>
<td style="text-align:right;">
37.96414
</td>
<td style="text-align:right;">
-121.5300
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
0.7455706
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
396.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
508
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
38.04819
</td>
<td style="text-align:right;">
-121.9039
</td>
<td style="text-align:right;">
-121.9172
</td>
<td style="text-align:right;">
38.04717
</td>
<td style="text-align:right;">
0.7276489
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
383.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
311
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
38.01200
</td>
<td style="text-align:right;">
-122.3843
</td>
<td style="text-align:right;">
-122.3774
</td>
<td style="text-align:right;">
38.02086
</td>
<td style="text-align:right;">
0.7174970
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
78.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
13
</td>
<td style="text-align:right;">
37.96642
</td>
<td style="text-align:right;">
-121.5308
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
0.6862903
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
381.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
306
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
37.99889
</td>
<td style="text-align:right;">
-122.4027
</td>
<td style="text-align:right;">
-122.4149
</td>
<td style="text-align:right;">
38.00042
</td>
<td style="text-align:right;">
0.6723281
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
100.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
706
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
13
</td>
<td style="text-align:right;">
38.09072
</td>
<td style="text-align:right;">
-121.7397
</td>
<td style="text-align:right;">
-121.7504
</td>
<td style="text-align:right;">
38.08608
</td>
<td style="text-align:right;">
0.6676507
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
245.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
37.96758
</td>
<td style="text-align:right;">
-121.5309
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
0.6530172
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
259.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
336
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
38.06283
</td>
<td style="text-align:right;">
-122.2896
</td>
<td style="text-align:right;">
-122.2780
</td>
<td style="text-align:right;">
38.06111
</td>
<td style="text-align:right;">
0.6427194
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
381.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
306
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
38.00342
</td>
<td style="text-align:right;">
-122.4038
</td>
<td style="text-align:right;">
-122.4149
</td>
<td style="text-align:right;">
38.00042
</td>
<td style="text-align:right;">
0.6392863
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
384.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
315
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
38.03250
</td>
<td style="text-align:right;">
-122.3850
</td>
<td style="text-align:right;">
-122.3936
</td>
<td style="text-align:right;">
38.03814
</td>
<td style="text-align:right;">
0.6086803
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
100.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
706
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
13
</td>
<td style="text-align:right;">
38.09000
</td>
<td style="text-align:right;">
-121.7407
</td>
<td style="text-align:right;">
-121.7504
</td>
<td style="text-align:right;">
38.08608
</td>
<td style="text-align:right;">
0.5961975
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
24.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
308
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
12
</td>
<td style="text-align:right;">
38.02606
</td>
<td style="text-align:right;">
-122.4043
</td>
<td style="text-align:right;">
-122.4044
</td>
<td style="text-align:right;">
38.01744
</td>
<td style="text-align:right;">
0.5939605
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
200.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
336
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
38.06269
</td>
<td style="text-align:right;">
-122.2884
</td>
<td style="text-align:right;">
-122.2780
</td>
<td style="text-align:right;">
38.06111
</td>
<td style="text-align:right;">
0.5799186
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
24.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
308
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
12
</td>
<td style="text-align:right;">
38.02578
</td>
<td style="text-align:right;">
-122.4057
</td>
<td style="text-align:right;">
-122.4044
</td>
<td style="text-align:right;">
38.01744
</td>
<td style="text-align:right;">
0.5793393
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
195.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
327
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
38.04364
</td>
<td style="text-align:right;">
-122.3758
</td>
<td style="text-align:right;">
-122.3667
</td>
<td style="text-align:right;">
38.04772
</td>
<td style="text-align:right;">
0.5685669
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
1.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
338
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
12
</td>
<td style="text-align:right;">
38.05947
</td>
<td style="text-align:right;">
-122.2386
</td>
<td style="text-align:right;">
-122.2489
</td>
<td style="text-align:right;">
38.06003
</td>
<td style="text-align:right;">
0.5663139
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
105.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
801
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
13
</td>
<td style="text-align:right;">
38.05147
</td>
<td style="text-align:right;">
-121.8472
</td>
<td style="text-align:right;">
-121.8440
</td>
<td style="text-align:right;">
38.04369
</td>
<td style="text-align:right;">
0.5640238
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
424.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:right;">
37.96828
</td>
<td style="text-align:right;">
-121.5293
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
0.5559421
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
19.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
12
</td>
<td style="text-align:right;">
37.96833
</td>
<td style="text-align:right;">
-121.5293
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
0.5544213
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
309.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
37.96842
</td>
<td style="text-align:right;">
-121.5293
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
0.5521822
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
451.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
336
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:right;">
38.06203
</td>
<td style="text-align:right;">
-122.2880
</td>
<td style="text-align:right;">
-122.2780
</td>
<td style="text-align:right;">
38.06111
</td>
<td style="text-align:right;">
0.5504679
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
201.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
338
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
38.06036
</td>
<td style="text-align:right;">
-122.2389
</td>
<td style="text-align:right;">
-122.2489
</td>
<td style="text-align:right;">
38.06003
</td>
<td style="text-align:right;">
0.5442915
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
201.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
338
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
38.06044
</td>
<td style="text-align:right;">
-122.2391
</td>
<td style="text-align:right;">
-122.2489
</td>
<td style="text-align:right;">
38.06003
</td>
<td style="text-align:right;">
0.5354885
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
415.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
804
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:right;">
38.01831
</td>
<td style="text-align:right;">
-121.8007
</td>
<td style="text-align:right;">
-121.7913
</td>
<td style="text-align:right;">
38.01644
</td>
<td style="text-align:right;">
0.5296005
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
185.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
37.96850
</td>
<td style="text-align:right;">
-121.5289
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
0.5289850
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
367.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
336
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
38.06294
</td>
<td style="text-align:right;">
-122.2873
</td>
<td style="text-align:right;">
-122.2780
</td>
<td style="text-align:right;">
38.06111
</td>
<td style="text-align:right;">
0.5229556
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
380.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
918
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
37.86528
</td>
<td style="text-align:right;">
-121.5725
</td>
<td style="text-align:right;">
-121.5671
</td>
<td style="text-align:right;">
37.85900
</td>
<td style="text-align:right;">
0.5220196
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
281.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
606
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
38.17194
</td>
<td style="text-align:right;">
-122.0185
</td>
<td style="text-align:right;">
-122.0279
</td>
<td style="text-align:right;">
38.17058
</td>
<td style="text-align:right;">
0.5197689
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
364.Start
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
37.96872
</td>
<td style="text-align:right;">
-121.5288
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
0.5175392
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
388.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
328
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
38.05467
</td>
<td style="text-align:right;">
-122.3562
</td>
<td style="text-align:right;">
-122.3500
</td>
<td style="text-align:right;">
38.06028
</td>
<td style="text-align:right;">
0.5136983
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
46.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
401
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
12
</td>
<td style="text-align:right;">
38.05956
</td>
<td style="text-align:right;">
-122.2215
</td>
<td style="text-align:right;">
-122.2124
</td>
<td style="text-align:right;">
38.05758
</td>
<td style="text-align:right;">
0.5136814
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
387.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
327
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
38.04719
</td>
<td style="text-align:right;">
-122.3573
</td>
<td style="text-align:right;">
-122.3667
</td>
<td style="text-align:right;">
38.04772
</td>
<td style="text-align:right;">
0.5133763
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
133.End
</td>
<td style="text-align:right;">
2023
</td>
<td style="text-align:right;">
404
</td>
<td style="text-align:left;">
End
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
38.05144
</td>
<td style="text-align:right;">
-122.1858
</td>
<td style="text-align:right;">
-122.1789
</td>
<td style="text-align:right;">
38.04644
</td>
<td style="text-align:right;">
0.5133495
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
328
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.06028
</td>
<td style="text-align:right;">
-122.3500
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
329
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.06361
</td>
<td style="text-align:right;">
-122.3040
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
336
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.06111
</td>
<td style="text-align:right;">
-122.2780
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
508
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.04717
</td>
<td style="text-align:right;">
-121.9172
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
606
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.17058
</td>
<td style="text-align:right;">
-122.0279
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
706
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.08608
</td>
<td style="text-align:right;">
-121.7504
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
32
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
801
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.04369
</td>
<td style="text-align:right;">
-121.8440
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
33
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
804
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.01644
</td>
<td style="text-align:right;">
-121.7913
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
41
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
912
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
37.96642
</td>
<td style="text-align:right;">
-121.3686
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
42
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
914
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
37.97150
</td>
<td style="text-align:right;">
-121.5200
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
44
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
918
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
37.85900
</td>
<td style="text-align:right;">
-121.5671
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
46
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
330
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.02853
</td>
<td style="text-align:right;">
-122.3282
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
50
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
723
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.23725
</td>
<td style="text-align:right;">
-121.6731
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
51
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
306
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.00042
</td>
<td style="text-align:right;">
-122.4149
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
52
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
308
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.01744
</td>
<td style="text-align:right;">
-122.4044
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
53
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
311
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.02086
</td>
<td style="text-align:right;">
-122.3774
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
54
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
315
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.03814
</td>
<td style="text-align:right;">
-122.3936
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
56
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
327
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.04772
</td>
<td style="text-align:right;">
-122.3667
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
57
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
401
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.05758
</td>
<td style="text-align:right;">
-122.2124
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
58
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
404
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.04644
</td>
<td style="text-align:right;">
-122.1789
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
59
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
338
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:left;">
Theoretical
</td>
<td style="text-align:right;">
38.06003
</td>
<td style="text-align:right;">
-122.2489
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
</tbody>
</table>

</div>

Note that the distance calculated between the sampling point and the
theoretical coordinates is “as the crow flies”, i.e., a straight line
(taking into account the curviture of the Earth). The default 0.5 mile
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

cdecTable <- kable(cdecTable)
cdecTable <- kable_styling(cdecTable, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
scroll_box(cdecTable, width = "100%", height = "400px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:400px; overflow-x: scroll; width:100%; ">

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
station
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
lat
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
lon
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
time
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
cdecStation
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
temp
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
tempCDEC
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
timeDifference
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
distance
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
sensorNumber
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
sensorDescription
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
units
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
duration
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
dataAvailable
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
306
</td>
<td style="text-align:right;">
38.00064
</td>
<td style="text-align:right;">
-122.4145
</td>
<td style="text-align:left;">
2023-01-03 11:36:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
10.4
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
9 mins
</td>
<td style="text-align:right;">
15.0672583
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
308
</td>
<td style="text-align:right;">
38.01542
</td>
<td style="text-align:right;">
-122.4095
</td>
<td style="text-align:left;">
2023-01-03 11:14:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
10.5
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
31 mins
</td>
<td style="text-align:right;">
14.7001185
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
311
</td>
<td style="text-align:right;">
38.02083
</td>
<td style="text-align:right;">
-122.3827
</td>
<td style="text-align:left;">
2023-01-03 12:01:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
10.1
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
1 mins
</td>
<td style="text-align:right;">
13.2227687
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
315
</td>
<td style="text-align:right;">
38.03833
</td>
<td style="text-align:right;">
-122.3926
</td>
<td style="text-align:left;">
2023-01-03 10:52:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
10.2
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
53 mins
</td>
<td style="text-align:right;">
13.7721004
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
322
</td>
<td style="text-align:right;">
38.02550
</td>
<td style="text-align:right;">
-122.3524
</td>
<td style="text-align:left;">
2023-01-03 12:27:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
10.1
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
3 mins
</td>
<td style="text-align:right;">
11.5615852
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
323
</td>
<td style="text-align:right;">
38.03958
</td>
<td style="text-align:right;">
-122.2944
</td>
<td style="text-align:left;">
2023-01-03 13:14:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
10.0
</td>
<td style="text-align:right;">
9.500000
</td>
<td style="text-align:left;">
1 mins
</td>
<td style="text-align:right;">
8.4373877
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
327
</td>
<td style="text-align:right;">
38.04975
</td>
<td style="text-align:right;">
-122.3700
</td>
<td style="text-align:left;">
2023-01-03 10:33:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
9.9
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
72 mins
</td>
<td style="text-align:right;">
12.6119443
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
328
</td>
<td style="text-align:right;">
38.06125
</td>
<td style="text-align:right;">
-122.3508
</td>
<td style="text-align:left;">
2023-01-03 10:12:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
9.7
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
93 mins
</td>
<td style="text-align:right;">
11.7019029
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
329
</td>
<td style="text-align:right;">
38.06464
</td>
<td style="text-align:right;">
-122.3193
</td>
<td style="text-align:left;">
2023-01-03 09:35:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
9.6
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
130 mins
</td>
<td style="text-align:right;">
10.0829544
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
330
</td>
<td style="text-align:right;">
38.02881
</td>
<td style="text-align:right;">
-122.3323
</td>
<td style="text-align:left;">
2023-01-03 12:51:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
10.0
</td>
<td style="text-align:right;">
9.333333
</td>
<td style="text-align:left;">
6 mins
</td>
<td style="text-align:right;">
10.4665533
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
335
</td>
<td style="text-align:right;">
38.06831
</td>
<td style="text-align:right;">
-122.3270
</td>
<td style="text-align:left;">
2023-01-03 09:53:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
9.6
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
112 mins
</td>
<td style="text-align:right;">
10.5536339
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
336
</td>
<td style="text-align:right;">
38.05925
</td>
<td style="text-align:right;">
-122.3032
</td>
<td style="text-align:left;">
2023-01-03 09:14:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
9.6
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
151 mins
</td>
<td style="text-align:right;">
9.1371593
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
338
</td>
<td style="text-align:right;">
38.06017
</td>
<td style="text-align:right;">
-122.2406
</td>
<td style="text-align:left;">
2023-01-03 08:48:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
9.6
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
177 mins
</td>
<td style="text-align:right;">
5.9012187
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
401
</td>
<td style="text-align:right;">
38.05872
</td>
<td style="text-align:right;">
-122.2131
</td>
<td style="text-align:left;">
2023-01-03 14:12:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
9.5
</td>
<td style="text-align:right;">
9.444444
</td>
<td style="text-align:left;">
3 mins
</td>
<td style="text-align:right;">
4.5041805
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
404
</td>
<td style="text-align:right;">
38.05028
</td>
<td style="text-align:right;">
-122.1837
</td>
<td style="text-align:left;">
2023-01-03 13:50:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
9.3
</td>
<td style="text-align:right;">
9.444444
</td>
<td style="text-align:left;">
5 mins
</td>
<td style="text-align:right;">
2.8284494
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
405
</td>
<td style="text-align:right;">
38.03986
</td>
<td style="text-align:right;">
-122.1430
</td>
<td style="text-align:left;">
2023-01-06 10:51:00
</td>
<td style="text-align:left;">
MRZ
</td>
<td style="text-align:right;">
9.6
</td>
<td style="text-align:right;">
9.611111
</td>
<td style="text-align:left;">
651 mins
</td>
<td style="text-align:right;">
0.8539239
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/21/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
418
</td>
<td style="text-align:right;">
38.06656
</td>
<td style="text-align:right;">
-122.0999
</td>
<td style="text-align:left;">
2023-01-06 11:18:00
</td>
<td style="text-align:left;">
FLT
</td>
<td style="text-align:right;">
9.6
</td>
<td style="text-align:right;">
10.000000
</td>
<td style="text-align:left;">
678 mins
</td>
<td style="text-align:right;">
0.9606551
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
10/21/2010 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
504
</td>
<td style="text-align:right;">
38.05342
</td>
<td style="text-align:right;">
-121.9885
</td>
<td style="text-align:left;">
2023-01-06 14:57:00
</td>
<td style="text-align:left;">
PCT
</td>
<td style="text-align:right;">
9.7
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA mins
</td>
<td style="text-align:right;">
0.6659752
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
04/02/1999 to 03/26/2001
</td>
</tr>
<tr>
<td style="text-align:right;">
513
</td>
<td style="text-align:right;">
38.05997
</td>
<td style="text-align:right;">
-121.8702
</td>
<td style="text-align:left;">
2023-01-06 12:03:00
</td>
<td style="text-align:left;">
C31
</td>
<td style="text-align:right;">
9.6
</td>
<td style="text-align:right;">
9.388889
</td>
<td style="text-align:left;">
723 mins
</td>
<td style="text-align:right;">
1.1905603
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
05/28/2021 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
519
</td>
<td style="text-align:right;">
38.07256
</td>
<td style="text-align:right;">
-121.9572
</td>
<td style="text-align:left;">
2023-01-06 12:51:00
</td>
<td style="text-align:left;">
HON
</td>
<td style="text-align:right;">
9.5
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
771 mins
</td>
<td style="text-align:right;">
0.9796518
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
06/19/2015 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
520
</td>
<td style="text-align:right;">
38.03192
</td>
<td style="text-align:right;">
-121.8608
</td>
<td style="text-align:left;">
2023-01-06 13:41:00
</td>
<td style="text-align:left;">
PTS
</td>
<td style="text-align:right;">
9.8
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA mins
</td>
<td style="text-align:right;">
1.8901394
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
04/02/1999 to 04/05/1999
</td>
</tr>
<tr>
<td style="text-align:right;">
602
</td>
<td style="text-align:right;">
38.11444
</td>
<td style="text-align:right;">
-122.0427
</td>
<td style="text-align:left;">
2023-01-06 11:44:00
</td>
<td style="text-align:left;">
GZL
</td>
<td style="text-align:right;">
9.6
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
704 mins
</td>
<td style="text-align:right;">
0.7203064
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
06/19/2015 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
606
</td>
<td style="text-align:right;">
38.16953
</td>
<td style="text-align:right;">
-122.0237
</td>
<td style="text-align:left;">
2023-01-06 12:12:00
</td>
<td style="text-align:left;">
VOL
</td>
<td style="text-align:right;">
9.7
</td>
<td style="text-align:right;">
10.333333
</td>
<td style="text-align:left;">
732 mins
</td>
<td style="text-align:right;">
1.4482172
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
05/15/2009 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
609
</td>
<td style="text-align:right;">
38.16722
</td>
<td style="text-align:right;">
-121.9379
</td>
<td style="text-align:left;">
2023-01-06 12:42:00
</td>
<td style="text-align:left;">
BLL
</td>
<td style="text-align:right;">
9.7
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA mins
</td>
<td style="text-align:right;">
1.9067759
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
05/15/2009 to 07/31/2019
</td>
</tr>
<tr>
<td style="text-align:right;">
610
</td>
<td style="text-align:right;">
38.12311
</td>
<td style="text-align:right;">
-121.8894
</td>
<td style="text-align:left;">
2023-01-06 13:11:00
</td>
<td style="text-align:left;">
NSL
</td>
<td style="text-align:right;">
9.8
</td>
<td style="text-align:right;">
9.555556
</td>
<td style="text-align:left;">
791 mins
</td>
<td style="text-align:right;">
0.1000495
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
05/21/2009 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
707
</td>
<td style="text-align:right;">
38.11631
</td>
<td style="text-align:right;">
-121.7052
</td>
<td style="text-align:left;">
2023-01-06 07:59:00
</td>
<td style="text-align:left;">
TMS
</td>
<td style="text-align:right;">
9.4
</td>
<td style="text-align:right;">
9.611111
</td>
<td style="text-align:left;">
479 mins
</td>
<td style="text-align:right;">
0.7650790
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
04/13/2006 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
804
</td>
<td style="text-align:right;">
38.01703
</td>
<td style="text-align:right;">
-121.7945
</td>
<td style="text-align:left;">
2023-01-06 14:25:00
</td>
<td style="text-align:left;">
ANH
</td>
<td style="text-align:right;">
9.9
</td>
<td style="text-align:right;">
9.777778
</td>
<td style="text-align:left;">
865 mins
</td>
<td style="text-align:right;">
0.4635702
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
10/16/2008 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
809
</td>
<td style="text-align:right;">
38.05042
</td>
<td style="text-align:right;">
-121.6946
</td>
<td style="text-align:left;">
2023-01-03 08:47:00
</td>
<td style="text-align:left;">
SJJ
</td>
<td style="text-align:right;">
9.7
</td>
<td style="text-align:right;">
9.611111
</td>
<td style="text-align:left;">
2 mins
</td>
<td style="text-align:right;">
0.3234965
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
12/01/2009 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
901
</td>
<td style="text-align:right;">
38.04589
</td>
<td style="text-align:right;">
-121.6167
</td>
<td style="text-align:left;">
2023-01-03 15:22:00
</td>
<td style="text-align:left;">
BET
</td>
<td style="text-align:right;">
9.7
</td>
<td style="text-align:right;">
8.722222
</td>
<td style="text-align:left;">
7 mins
</td>
<td style="text-align:right;">
0.9046672
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
03/29/2006 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
912
</td>
<td style="text-align:right;">
37.96661
</td>
<td style="text-align:right;">
-121.3692
</td>
<td style="text-align:left;">
2023-01-03 11:21:00
</td>
<td style="text-align:left;">
RRI
</td>
<td style="text-align:right;">
10.3
</td>
<td style="text-align:right;">
10.333333
</td>
<td style="text-align:left;">
6 mins
</td>
<td style="text-align:right;">
0.3383534
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
08/28/2000 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
914
</td>
<td style="text-align:right;">
37.96897
</td>
<td style="text-align:right;">
-121.5285
</td>
<td style="text-align:left;">
2023-01-03 12:40:00
</td>
<td style="text-align:left;">
BIR
</td>
<td style="text-align:right;">
10.0
</td>
<td style="text-align:right;">
9.277778
</td>
<td style="text-align:left;">
5 mins
</td>
<td style="text-align:right;">
1.8117491
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
03/02/2018 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
919
</td>
<td style="text-align:right;">
38.10525
</td>
<td style="text-align:right;">
-121.4930
</td>
<td style="text-align:left;">
2023-01-03 16:23:00
</td>
<td style="text-align:left;">
LPS
</td>
<td style="text-align:right;">
9.7
</td>
<td style="text-align:right;">
9.777778
</td>
<td style="text-align:left;">
7 mins
</td>
<td style="text-align:right;">
0.6329446
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
event
</td>
<td style="text-align:left;">
12/01/2009 to present
</td>
</tr>
<tr>
<td style="text-align:right;">
706
</td>
<td style="text-align:right;">
38.08606
</td>
<td style="text-align:right;">
-121.7502
</td>
<td style="text-align:left;">
2023-01-06 10:49:00
</td>
<td style="text-align:left;">
EMM
</td>
<td style="text-align:right;">
9.5
</td>
<td style="text-align:right;">
9.611111
</td>
<td style="text-align:left;">
649 mins
</td>
<td style="text-align:right;">
0.6295539
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
TEMPERATURE, WATER, DEG F
</td>
<td style="text-align:left;">
DEG C
</td>
<td style="text-align:left;">
hourly
</td>
<td style="text-align:left;">
02/23/1999 to present
</td>
</tr>
</tbody>
</table>

</div>

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
