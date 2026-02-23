# EDI Workflows

The following demonstrates how to grab files from the EDI Repository.
These functions offer similar services to those from the ‘EDIUtils’
package but are opinionated to work similarly to other workflows in
‘deltadata’.

``` r
library(deltadata)
```

## Reading in entities from EDI

The
[`getEDI()`](https://github.com/trinhxuann/deltadata/reference/getEDI.md)
function is a one-stop shop function that allows us to download entities
from an EDI repository. To get started, provide the function with a URL
to the data package of interest.

``` r
# Data package of the CDFW IEP SLS Survey
possibleEntities <- getEDI("https://portal.edirepository.org/nis/mapbrowse?packageid=edi.534.9")
```

    ## Specify files to download: 
    ##                       name extension      size
    ## 1                  SLS.csv       csv   8.8 MiB
    ## 2                Catch.csv       csv 369.3 KiB
    ## 3            FishCodes.csv       csv     4 KiB
    ## 4              Lengths.csv       csv   8.1 MiB
    ## 5     MeterCorrections.csv       csv  16.3 KiB
    ## 6       Station_Lookup.csv       csv   7.8 KiB
    ## 7              TowInfo.csv       csv 375.1 KiB
    ## 8            WaterInfo.csv       csv 730.9 KiB
    ## 9        SLSIntegrateEDI.R         R  21.8 KiB
    ## 10           SLSTables.rds       rds 908.2 KiB
    ## 11        SLS_Metadata.pdf       pdf 843.4 KiB
    ## 12 SLSDatabaseEditLog.xlsx      xlsx  25.7 KiB
    ##                                                                                                                                                                                                                                                  description
    ## 1                                                                                                                                                                                                        Joined long formatted data frame of the base tables
    ## 2                                                                                                                                                                                                                Fish catch data from the Smelt Larva Survey
    ## 3                                                                                                                                                                                                            Taxa, common name, and corresponding fish codes
    ## 4                                                                                                                                                         Data related to fish size including fork length, yolk sac, and adipose fin of measured individuals
    ## 5                                                                                                                                                                                          Instrument information for flowmeters used throughout the seasons
    ## 6                                                                                                                                                                                                            Theoretical GPS coordinates of all SLS stations
    ## 7                                                                                                                                                                                                                       Metrics relating directly to the tow
    ## 8                                                                                                                                                                                                 Metrics relating directly to water measured during the tow
    ## 9                                                                                                                                                       R code to join 6 relational tables together (excluding "FishCodes.cvs") to output the "SLS.csv" file
    ## 10 Compressed data file of the 7 relational tables uploaded in csv format (containing exactly the same data). This file is for use in R. Once read, the 7 relational tables are structured in a list form, with each table being a named element of the list
    ## 11                                                                                                                                                                                                                                        Formatted metadata
    ## 12                                                                                                                                                                                                            Document outlining corrections made to dataset

When only a URL is provided, the function will return a list of entities
available to download. The `name` is the file name of the entity,
`extension` is the file extension, `size` is the file size, and
`description` is a short description of the entity provided by the
creators. This printed table contains only a subset of the entire
data.frame, which can be accessible if you assign the function to a
variable.

``` r
# Full data.frame has more information than what is printed
head(possibleEntities)
```

    ##                   name extension      size sizeBytes
    ## 1              SLS.csv       csv   8.8 MiB   9225612
    ## 2            Catch.csv       csv 369.3 KiB    378117
    ## 3        FishCodes.csv       csv     4 KiB      4136
    ## 4          Lengths.csv       csv   8.1 MiB   8478413
    ## 5 MeterCorrections.csv       csv  16.3 KiB     16710
    ## 6   Station_Lookup.csv       csv   7.8 KiB      8032
    ##                                                                                          description
    ## 1                                                Joined long formatted data frame of the base tables
    ## 2                                                        Fish catch data from the Smelt Larva Survey
    ## 3                                                    Taxa, common name, and corresponding fish codes
    ## 4 Data related to fish size including fork length, yolk sac, and adipose fin of measured individuals
    ## 5                                  Instrument information for flowmeters used throughout the seasons
    ## 6                                                    Theoretical GPS coordinates of all SLS stations
    ##                                                                                     link
    ## 1 https://pasta.lternet.edu/package/data/eml/edi/534/11/d5f0c1509f89ff1784971832af4e7670
    ## 2 https://pasta.lternet.edu/package/data/eml/edi/534/11/c5d59568835d0760e318829b800ee06d
    ## 3 https://pasta.lternet.edu/package/data/eml/edi/534/11/93a50ec0337ddbc6df6d8f6d36ab8997
    ## 4 https://pasta.lternet.edu/package/data/eml/edi/534/11/b6911e22e9ac870f20a9da9854bb367a
    ## 5 https://pasta.lternet.edu/package/data/eml/edi/534/11/6da12710629f46831cbd02499c626532
    ## 6 https://pasta.lternet.edu/package/data/eml/edi/534/11/4905aa8f3bafdd75423c63e66a2256e9
    ##                                 id
    ## 1 d5f0c1509f89ff1784971832af4e7670
    ## 2 c5d59568835d0760e318829b800ee06d
    ## 3 93a50ec0337ddbc6df6d8f6d36ab8997
    ## 4 b6911e22e9ac870f20a9da9854bb367a
    ## 5 6da12710629f46831cbd02499c626532
    ## 6 4905aa8f3bafdd75423c63e66a2256e9

To download file(s), we can provide the values of interest in the `name`
column to the function.

``` r
entities <- getEDI("https://portal.edirepository.org/nis/mapbrowse?packageid=edi.534.9", 
                   files = c("Catch.csv", "SLSTables.rds", "SLS_Metadata.pdf"))
```

    ## Downloading: Catch.csv (369.3 KiB)

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   2%  |                                                                              |======                                                                |   9%  |                                                                              |=========                                                             |  13%  |                                                                              |============                                                          |  17%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  26%  |                                                                              |========================                                              |  35%  |                                                                              |==============================                                        |  43%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |===================================                                   |  51%  |                                                                              |=====================================                                 |  53%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  59%  |                                                                              |===========================================                           |  61%  |                                                                              |============================================                          |  64%  |                                                                              |======================================================                |  77%  |                                                                              |================================================================      |  91%  |                                                                              |======================================================================| 100%

    ## Downloading: SLSTables.rds (908.2 KiB)

    ##   |                                                                              |                                                                      |   0%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |=====                                                                 |   7%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |==============                                                        |  19%  |                                                                              |===============                                                       |  22%  |                                                                              |=======================                                               |  33%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  38%  |                                                                              |============================                                          |  41%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=============================================                         |  64%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |=============================================================         |  87%  |                                                                              |======================================================================| 100%

    ## Downloading: SLS_Metadata.pdf (843.4 KiB)

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   5%  |                                                                              |=====                                                                 |   8%  |                                                                              |=======                                                               |   9%  |                                                                              |========                                                              |  11%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  15%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  19%  |                                                                              |===============                                                       |  21%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |===================                                                   |  27%  |                                                                              |============================                                          |  40%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  52%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=================================================                     |  70%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%

## Files are stored differently

The function download and stores the entities based on what the file
extension. Specifically, ‘.csv’ and ‘.rds’ files are read directly into
R, while all other file types are downloaded to the temporary folder and
a link to the file is provided.

``` r
# csv files are read in directly
head(entities$Catch.csv)
```

    ##         Date Station Tow FishCode Catch X1.4.Subsampled X1.2.Subsampled CatchID
    ## 1 2009-01-05     902   1       49     7               0               0       4
    ## 2 2009-01-05     906   1        2     1               0               0       5
    ## 3 2009-01-05     912   1       49     1               0               0       6
    ## 4 2009-01-05     914   1       49     1               0               0       7
    ## 5 2009-01-05     915   1       49     6               0               0       8
    ## 6 2009-01-05     918   1       49     1               0               0       9

``` r
# rds files are read in directly, outputted as a list
lapply(entities$SLSTables, head)
```

    ## $Catch
    ##         Date Station Tow FishCode Catch X1.4.Subsampled X1.2.Subsampled CatchID
    ## 1 2009-01-05     902   1       49     7               0               0       4
    ## 2 2009-01-05     906   1        2     1               0               0       5
    ## 3 2009-01-05     912   1       49     1               0               0       6
    ## 4 2009-01-05     914   1       49     1               0               0       7
    ## 5 2009-01-05     915   1       49     6               0               0       8
    ## 6 2009-01-05     918   1       49     1               0               0       9
    ## 
    ## $Lengths
    ##         Date Station Tow FishCode Length EntryOrder YolkSacOrOilPresent
    ## 1 2009-01-05     902   1       49      6        274               FALSE
    ## 2 2009-01-05     902   1       49      6        275               FALSE
    ## 3 2009-01-05     902   1       49      6        276               FALSE
    ## 4 2009-01-05     902   1       49      6        277               FALSE
    ## 5 2009-01-05     902   1       49      6        278               FALSE
    ## 6 2009-01-05     902   1       49      6        279               FALSE
    ## 
    ## $MeterCorrections
    ##   StudyYear MeterSerial CalibrationDate  kFactor Notes
    ## 1      1994        7539      1993-10-01 0.026858  <NA>
    ## 2      1994        9794      1994-08-31 0.026974  <NA>
    ## 3      1994        9887      1994-08-31 0.025497  <NA>
    ## 4      1994       10369      1994-08-31 0.026630  <NA>
    ## 5      1994       10378      1994-08-31 0.026577  <NA>
    ## 6      1994       11228      1994-08-30 0.026947  <NA>
    ## 
    ## $TowInfo
    ##         Date Station Tow  Time Tide BottomDepth CableOut Duration
    ## 1 2009-01-05     902   1 10:12    4          26      110       10
    ## 2 2009-01-05     906   1 14:23    2          39      140       10
    ## 3 2009-01-05     910   1 12:49    4          40      155       10
    ## 4 2009-01-05     912   1 13:26    4          36      140       10
    ## 5 2009-01-05     914   1 12:08    4          35      110       10
    ## 6 2009-01-05     915   1 10:41    4          21       90       10
    ##   NetMeterSerial NetMeterStart NetMeterEnd NetMeterCheck
    ## 1          21920        103454      124337         20883
    ## 2          21920        247554      267406         19852
    ## 3          21920        188797      211813         23016
    ## 4          21920        225510      247557         22047
    ## 5          21920        167578      188797         21219
    ## 6          21920        124337      146419         22082
    ##                                                                                         Comments
    ## 1                                                                           crew: KE, KF, LD, VA
    ## 2                                                                           crew: KE, KF, LD, VA
    ## 3 crew: KE, KF, LD, VA.  Sample splashed when pouring into formalin jar. Less than 5% of sample.
    ## 4                                    crew: KE, KF, LD, VA.  Re-tow due to incorrect cable length
    ## 5                                             crew: KE, KF, LD, VA.  Net will come up to 25 feet
    ## 6                                                                           crew: KE, KF, LD, VA
    ## 
    ## $WaterInfo
    ##   Survey       Date Station TopTemp TopEC BottomEC Secchi FNU NTU StartLat
    ## 1      1 2009-01-05     902     7.4   858      777     92  NA  NA     <NA>
    ## 2      1 2009-01-05     906     8.0   469      478    129  NA  NA     <NA>
    ## 3      1 2009-01-05     910     8.2   548      587     77  NA  NA     <NA>
    ## 4      1 2009-01-05     912     8.2   997      991     63  NA  NA     <NA>
    ## 5      1 2009-01-05     914     7.8   604      601    157  NA  NA     <NA>
    ## 6      1 2009-01-05     915     7.5   803      801    140  NA  NA     <NA>
    ##   StartLatDeg StartLatMin StartLatSec StartLong StartLongDeg StartLongMin
    ## 1        <NA>        <NA>        <NA>      <NA>         <NA>         <NA>
    ## 2        <NA>        <NA>        <NA>      <NA>         <NA>         <NA>
    ## 3        <NA>        <NA>        <NA>      <NA>         <NA>         <NA>
    ## 4        <NA>        <NA>        <NA>      <NA>         <NA>         <NA>
    ## 5        <NA>        <NA>        <NA>      <NA>         <NA>         <NA>
    ## 6        <NA>        <NA>        <NA>      <NA>         <NA>         <NA>
    ##   StartLongSec EndLat EndLatDeg EndLatMin EndLatSec EndLong EndLongDeg
    ## 1         <NA>   <NA>      <NA>      <NA>      <NA>    <NA>       <NA>
    ## 2         <NA>   <NA>      <NA>      <NA>      <NA>    <NA>       <NA>
    ## 3         <NA>   <NA>      <NA>      <NA>      <NA>    <NA>       <NA>
    ## 4         <NA>   <NA>      <NA>      <NA>      <NA>    <NA>       <NA>
    ## 5         <NA>   <NA>      <NA>      <NA>      <NA>    <NA>       <NA>
    ## 6         <NA>   <NA>      <NA>      <NA>      <NA>    <NA>       <NA>
    ##   EndLongMin EndLongSec Comments
    ## 1       <NA>       <NA>     <NA>
    ## 2       <NA>       <NA>     <NA>
    ## 3       <NA>       <NA>     <NA>
    ## 4       <NA>       <NA>     <NA>
    ## 5       <NA>       <NA>     <NA>
    ## 6       <NA>       <NA>     <NA>
    ## 
    ## $Station_Lookup
    ##   ID Station
    ## 1 25     705
    ## 2 26     706
    ## 3 27     707
    ## 4 28     711
    ## 5 29     716
    ## 6 30     723
    ##                                                                                                                                    Description
    ## 1                               Horse Shoe Bend. Inside Horse Shoe Bend by north entrance to Sacramento River. Tow on west side @ 11-13' depth
    ## 2          Sacramento River. below south tip of Decker Is. Target midpoint is between PG&E towers. Tow on north side out of main channel @ 22'
    ## 3                                Sacramento River at Three Mile Sl. Target midpoint in between light #19 & #21. Tow in channel @ 24-26' depth.
    ## 4  Sacramento River at the tip of Grand Is. Start upstream of light #1 and towing upstream only @ 13-16' depth. Tow along north side in shoal.
    ## 5 Cache Sl. by old ferry crossing near south end of Prospect Sl.Tow parallel to road after scour hole. Do not tow scour hole. Tow @ 30' depth.
    ## 6                                           Sacramento Deepwater Channel. Midpoint target @ Channel markers 51 & 52. Tow in middle of channel.
    ##          Lat        Long
    ## 1 38 05 51.4 121 42 31.5
    ## 2 38 05 09.9 121 45 01.6
    ## 3 38 06 52.9 121 42 28.3
    ## 4 38 10 38.7 121 39 44.1
    ## 5 38 14 18.8 121 41 02.1
    ## 6 38 14 14.1 121 40 23.1
    ## 
    ## $FishCodes
    ##   Fish.Code           Common.Name                 Taxa
    ## 1        18        Black Bullhead       Ameiurus melas
    ## 2        76 California Tonguefish  Symphurus atricauda
    ## 3        72            Red Shiner   Notropis lutrensis
    ## 4        73        Fathead Minnow  Pimephales promelas
    ## 5        74        Shokihaze Goby Tridentiger barbatus
    ## 6        75      Tridentiger spp.     Tridentiger spp.

``` r
# All other file types are temporarily downloaded and a file path provided
entities$SLS_Metadata
```

    ## [1] "/tmp/Rtmpxo6g05/SLS_Metadata.pdf"

You can use `shell.exec()` to open any downloaded files.

``` r
shell.exec(entities$SLS_Metadata)
```
