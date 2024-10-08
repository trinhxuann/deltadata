% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdecHelpers.R
\name{pullCDEC}
\alias{pullCDEC}
\title{Pulling CDEC gage data}
\usage{
pullCDEC(
  station,
  sensor = NULL,
  duration = c("event", "hourly", "daily"),
  dateStart,
  dateEnd = NULL,
  temperatureUnits = c("C", "F"),
  coordinates,
  verbose = T
)
}
\arguments{
\item{station}{A character vector of station names. Can be multiple stations.}

\item{sensor}{A singular sensor value of interest.}

\item{duration}{The duration data, can be \code{event}, \code{hourly}, or \code{daily} and
depends on data availability for the gage of interest.}

\item{dateStart}{Beginning date for the period of interest.}

\item{dateEnd}{Ending date for the period of interest. Will default to today
if left as \code{NULL}}

\item{temperatureUnits}{Either \code{C} or \code{F} to convert temperature. Only
applicable to temperature data.}

\item{coordinates}{A vector of length = 2 containing the lat and lon, in
that order. This argument can be used instead of \code{station}. See 'Details'
for additional comments.}

\item{verbose}{Logical. Should the function guide the user through the
argument selection if arguments are missing? This also prints the link that
the function will download from. Defaults to \code{TRUE}.}
}
\value{
A data frame of the requested data pull.
}
\description{
This function will first pull the metadata of a CDEC gage. This allows the
function to direct the user accordingly if various arguments are missing.
Once all arguments are provided, the query is created and data downloaded.
}
\details{
The \code{coordinates} argument can be used in place of the \code{station} argument.
The \code{\link{calcNearestCDEC}} function will be used to calculate the nearest
CDEC station to your point of interest and pull data from that gage. use
that function if you are specifically only interested in the metadata
of the nearest CDEC gage.
}
\examples{
\dontrun{
pullCDEC("MAL")
pullCDEC(coordinates = c(38.04281, -121.9201))
pullCDEC("MAL", 25, "hourly", "06/13/1986", "06/14/1986")
# If coordinates are used instead, must specify the argument names.
pullCDEC(coordinates = c(38.04281, -121.9201), sensor = 25,
duration = "hourly", dateStart = "06/13/1986", dateEnd = "06/14/1986")
}
}
