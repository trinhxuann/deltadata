#' CDEC gage GPS coordinates
#'
#' GPS Coordinates of all available CDEC stations
#'
#' @format A data frame with 3 variables
#' \describe{
#'   \item{station}{Name of the CDEC station}
#'   \item{latitude}{latitude value in degrees}
#'   \item{longitude}{longitude value in degree}
#'   }
"CDECGPS"

#' CDEC gage metadata
#'
#' Sensor metadata of all available CDEC stations
#'
#' @format A data frame with 7 variables
#' \describe{
#'   \item{sensorDescription}{Description of the sensor}
#'   \item{sensorNumber}{Value of the sensor}
#'   \item{duration}{Data duration flag}
#'   \item{plot}{Plot flag}
#'   \item{dataCollection}{Collection source}
#'   \item{dataAvailable}{Range of data availability}
#'   \item{gage}{CDEC gage name}
#'   }
"CDECMetadata"
