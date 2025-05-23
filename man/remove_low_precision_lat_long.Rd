% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_low_precision_lat_long.R
\name{remove_low_precision_lat_long}
\alias{remove_low_precision_lat_long}
\title{Filter Out Low Precision Camera Locations}
\usage{
remove_low_precision_lat_long(datalist, coordinate_precision = coord_prec)
}
\arguments{
\item{datalist}{list object containing location, effort, and detection dataframes.}

\item{coordinate_precision}{Number of digits after decimal for determining whether a location is precise or not}

\item{locations}{Data frame of camera locations}
}
\value{
Locations data frame with high precision coordinates
}
\description{
Remove camera_location_seq_no's that have a low precision latitude or longitude coordinate.
Does this first in locations dataframe before filtering the effort and detections
dataframes by camera_location_seq_no.
}
\details{
Until summer 2023, there was no requirement for the number of decimal places
when recording latitude or longitude coordinates, or the ability to record trailing zeros in the database.
}
\examples{
\dontrun{
# set coordinate precision
coordinate_precision <- 4
locs_df \%>\% remove_low_precision_lat_long(coordinate_precision)
}
}
