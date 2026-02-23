# Apply basic QAQC procedures to a survey dataset

Apply basic QAQC procedures to a survey dataset

## Usage

``` r
qaqcData(
  data,
  year,
  survey = c("ybfmp", "djfmp", "frp", "edsm", "sls", "20mm", "stn", "fmwt", "skt", "sms"),
  convertNames = F,
  officialGPS = NULL,
  gpsDistance = 0.5,
  startingGPSFormat = c("dms", "ddm"),
  towSchedule = NULL,
  meterSchedule = NULL,
  waterQualityVariables = c("BottomDepth", "WaterTemperature", "WaterTemperatureTop",
    "WaterTemperatureBottom", "SpecificConductance", "SpecificConductanceTop",
    "SpecificConductanceBottom", "Secchi", "TurbidityNTU", "TurbidityTopNTU",
    "TurbidityBottomNTU", "Salinity", "SalinityTop", "SalinityBottom"),
  stdev = 2,
  waterQualityGroupings = list("StationCode", c("StationCode", "Month"))
)
```

## Arguments

- data:

  A data frame with the required data, dependent on what QAQC procedures
  you want to run. It is advised to provide the fully joined/merged
  dataset.

- year:

  A filtering year. This can support custom year labels if desired,
  e.g., your sampling season extends across two years.

- survey:

  Name of a qualifying IEP survey name. Currently explicit support only
  for sls, 20mm, stn, fmwt, and bs.

- convertNames:

  T/F. Should the names be changed to their respective IEP recommended
  names

- officialGPS:

  Calculate outlying gps points if provided, defaulting to NULL. A data
  frame containing the official GPS coordinates of the sampling
  stations. Required to have at least three columns: `station`, `lat`,
  and `lon`. Coordinates should be provided in degree decimals.

- gpsDistance:

  A value in miles indicating the threshold distance beyond which is an
  outlier. Defaults to 0.5 miles

- startingGPSFormat:

  Either degrees, minutes, and seconds (dms) or degrees and decimal
  minutes (ddm). Format of your GPS coordinates. This will be convert to
  decimal degrees

- towSchedule:

  Calculate outlying cable length values if provided. A data frame
  containing the tow schedule. Several tow schedules are provided in the
  package within the `towSchedule` list. See details.

- meterSchedule:

  Calculate outlying flow meter readings if provided. A data frame
  containing the expected range for a meter reading based on the
  duration of the tow. Several meter schedules are provided in the
  package within the `meterSchedule` list. You can provide your own data
  frame following the format.

- waterQualityVariables:

  Calculate outlying water quality variables.

- stdev:

  Defaults to 2. The number of standard deviation away from the mean to
  flag as an outlying water quality value

- waterQualityGroupings:

  A list of grouping variables to iterate through the water quality
  check. By default, calculates per station and per station and month.

## Value

A list of objects containing identified outliers or rows with missing
data points

## Details

For the `towSchedule` and `meterSchedule` arguments, you can use a
schedules recorded in the package (`towSchedule` or `meterSchedule`),
currently available only for center surveys, which were based on
protocol documentation. If not available, you can provide your own
schedule. See the example on how to create such a table.

## Examples
