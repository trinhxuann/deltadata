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

#' A crosswalk of IEP survey names
#'
#' Relates IEP survey names to recommended names created by the DUWG
#'
#' @format A data frame with 29 columns
#' \describe{
#'   \item{variableType}{Tow duration}
#'   \item{variable}{Maximum depth recorded at sampling location}
#'   \item{recommendedName}{Depth range label created by the cut() function}
#'   \item{recommendedUnit}{Length of cable to be let out}
#'   \item{notes}{Maximum depth recorded at sampling location}
#'   \item{ybfmp}{Depth range label created by the cut() function}
#'   \item{YBFMP Units}{Length of cable to be let out}
#'   \item{djfmp}{Maximum depth recorded at sampling location}
#'   \item{DJFMP Units}{Depth range label created by the cut() function}
#'   \item{Delta Smelt formalin preservation data}{Length of cable to be let out}
#'   \item{Delta boat electrofishing survey }{Maximum depth recorded at sampling location}
#'   \item{Electrofishing Units}{Depth range label created by the cut() function}
#'   \item{EDSM}{Length of cable to be let out}
#'   \item{EDSM Units}{Maximum depth recorded at sampling location}
#'   \item{sls}{Depth range label created by the cut() function}
#'   \item{SLS units}{Length of cable to be let out}
#'   \item{20mm}{Maximum depth recorded at sampling location}
#'   \item{20mm units}{Depth range label created by the cut() function}
#'   \item{stn}{Length of cable to be let out}
#'   \item{STN units}{Maximum depth recorded at sampling location}
#'   \item{fmwt}{Depth range label created by the cut() function}
#'   \item{FMWT units}{Length of cable to be let out}
#'   \item{skt}{Maximum depth recorded at sampling location}
#'   \item{SKT units}{Depth range label created by the cut() function}
#'   \item{sms}{Length of cable to be let out}
#'   \item{SMS units}{Maximum depth recorded at sampling location}
#'   \item{bs}{Depth range label created by the cut() function}
#'   }
"crosswalk"

#' Tow schedules of IEP surveys
#'
#' Replicated tow schedules for IEP surveys
#'
#' @format A list of data frames of each survey. Each data frame has 4 columns
#' \describe{
#'   \item{duration}{Tow duration}
#'   \item{maxDepth}{Maximum depth recorded at sampling location}
#'   \item{depth}{Depth range label created by the cut() function}
#'   \item{cableLength}{Length of cable to be let out}
#'   }
"towSchedule"

#' Meter schedules of IEP surveys
#'
#' Data frame outlining the acceptable flowmeter range per tow duration
#'
#' @format A list of data frames of each survey. Each data frame has 4 columns
#' \describe{
#'   \item{duration}{Tow duration}
#'   \item{meterMin}{Minimum acceptable value}
#'   \item{meterMax}{Maximum acceptable value}
#'   \item{gear}{Gear type, leave as NA if none}
#'   }
"meterSchedule"
