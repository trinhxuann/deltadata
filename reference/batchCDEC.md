# Batch download CDEC data

Implements a workflow to allow for robustly downloading a large amount
of CDEC data. The CDEC API currently has a limit of returning a maximum
of 3-4 million rows after which it will stop transferring data. This
function estimates how large a data pull will be and paginates the
request into more manageable chunks. Defaults to paginate every 1
million rows.

## Usage

``` r
batchCDEC(
  station,
  sensor,
  duration,
  dateStart,
  dateEnd = NULL,
  rowLimit = 2e+06,
  ...
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

- rowLimit:

  Maximum number of rows to download at once. Maximum from the server
  appears to be around 3-4 million rows. Defaults to 1 million. A larger
  value can be used but depending on internet speed may bog down.

- ...:

  Any other arguments to be passed onto
  [`pullCDEC`](https://github.com/trinhxuann/deltadata/reference/pullCDEC.md).

## Value

A data frame of the request data pull.

## Examples

``` r
if (FALSE) { # \dontrun{
# This theoretically can be ran via pullCDEC as well since it does not return
# an excess amount of rows
batchCDEC(
station = c("BDT","DLC","DLC","DSJ","DWS","FAL","FCT","FPT","FPT","FPT","FPX","GES","GLC","GLC",
"GLE","GSS","HLT","HLT","HOL","HWB","IST","IST","JTR","LIB","LIS","LIS","LPS","M13",
"MAB","MAL","MDM","MDM","MIR","MOK","MRU","MSD","MSD","NMR","NSL","NSL","OBI","OBI",
"ODM","OH1","OH1","OH4","OH4","OLD","OMR","ORI","ORM","ORQ","ORX","OSJ","OSJ","PDC",
"PRI","RRI","RYF","RYI","SDC","SDC","SDI","SGG","SJC","SJD","SJG","SJJ","SJL","SJL",
"SMR","SOI","SPE","SRV","SSS","SUT","SXS","TOE","TRN","TSL","UCS","ULC","VCU","WCI"),
sensor = 20, duration = "event", dateStart = Sys.Date() - 90
)
} # }
```
