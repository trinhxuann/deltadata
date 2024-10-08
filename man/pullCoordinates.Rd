% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdecHelpers.R
\name{pullCoordinates}
\alias{pullCoordinates}
\title{Pull CDEC gage lat/lon}
\usage{
pullCoordinates(gage)
}
\arguments{
\item{gage}{Name of the gage of interest, as a character.}
}
\value{
A data frame containing the station name, lat, and lon.
}
\description{
Pull CDEC gage lat/lon
}
\examples{
\dontrun{
pullCoordinates("MAL")
}
}
