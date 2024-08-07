% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gpsHelpers.R
\name{decimalDegrees}
\alias{decimalDegrees}
\title{Convert GPS Coordinates to Decimal Degrees}
\usage{
decimalDegrees(degrees, minutes, seconds)
}
\arguments{
\item{degrees}{A numeric vector}

\item{minutes}{A numeric vector}

\item{seconds}{A numeric vector}
}
\value{
A numeric vector in decimal degrees
}
\description{
Convert GPS Coordinates to Decimal Degrees
}
\examples{
gpsDF <- data.frame(LatD = c(rep(38, 7)),
LatM = c(2, 3, 3, 4, 4, 3, 5),
LatS = c(34.4, 37.1, 49, 35, 16, 39.9, 57.2))

decimalDegrees(degrees = gpsDF$LatD,
minutes = gpsDF$LatM,
Seconds = gpsDF$LatS)
}
