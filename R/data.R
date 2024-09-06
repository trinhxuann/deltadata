#' #' CDEC gage GPS coordinates
#' #'
#' #' GPS Coordinates of all available CDEC stations
#' #'
#' #' @format A data frame with 3 variables
#' #' \describe{
#' #'   \item{station}{Name of the CDEC station}
#' #'   \item{latitude}{latitude value in degrees}
#' #'   \item{longitude}{longitude value in degree}
#' #'   }
#' "CDECGPS"
#'
#' #' CDEC gage metadata
#' #'
#' #' Sensor metadata of all available CDEC stations
#' #'
#' #' @format A data frame with 7 variables
#' #' \describe{
#' #'   \item{sensorDescription}{Description of the sensor}
#' #'   \item{sensorNumber}{Value of the sensor}
#' #'   \item{duration}{Data duration flag}
#' #'   \item{plot}{Plot flag}
#' #'   \item{dataCollection}{Collection source}
#' #'   \item{dataAvailable}{Range of data availability}
#' #'   \item{gage}{CDEC gage name}
#' #'   }
#' "CDECMetadata"

#' A crosswalk of IEP survey names
#'
#' Relates IEP survey names to recommended names created by the DUWG
#'
#' @format A data frame with 29 columns
#' \describe{
#'   \item{variableType}{Category of variable of interest}
#'   \item{variable}{Name used to describe the data collected}
#'   \item{recommendedName}{Value recommended by IEP to use to describe the relevant data}
#'   \item{recommendedUnit}{Value recommended by IEP to use to describe the unit of the relevant data}
#'   \item{notes}{Freeform comments by IEP}
#'   \item{ybfmp}{Yolo Bypass Fish Monitoring Program}
#'   \item{YBFMP Units}{Units of}
#'   \item{djfmp}{Delta Juvenile Fish Monitoring Program}
#'   \item{DJFMP Units}{Units of}
#'   \item{frp}{Fish Restoration Program}
#'   \item{FRP Units}{Units of}
#'   \item{Delta Smelt formalin preservation data}{Not familiar with this}
#'   \item{Delta boat electrofishing survey }{Not familiar with this}
#'   \item{Electrofishing Units}{Units of}
#'   \item{EDSM}{Enhanced Delta Smelt Monitoring Program}
#'   \item{EDSM Units}{Units of}
#'   \item{sls}{Smelt Larval Survey}
#'   \item{SLS units}{Units of}
#'   \item{20mm}{20 mm Survey}
#'   \item{20mm units}{Units of}
#'   \item{stn}{Summer Townet Survey}
#'   \item{STN units}{Units of}
#'   \item{fmwt}{Fall Midwater Trawl}
#'   \item{FMWT units}{Units of}
#'   \item{skt}{Spring Kodiak Trawl}
#'   \item{SKT units}{Units of}
#'   \item{sms}{Suisun Marsh Study}
#'   \item{SMS units}{Units of}
#'   \item{bs}{Bay Study}
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
