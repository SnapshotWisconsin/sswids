% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_nearby_cameras.R
\name{merge_nearby_cameras}
\alias{merge_nearby_cameras}
\title{Merge Nearby Camera Locations}
\usage{
merge_nearby_cameras(locations, cam_distance, effort_df)
}
\arguments{
\item{locations}{Data frame of camera_location_seq_no's and coordinates}

\item{cam_distance}{Distance (meters) in which nearby camera locations should be combined}

\item{effort_df}{Data frame of days of effort for each camera_location_seq_no}
}
\description{
Combine nearby camera_location_seq_no's and create new camera site IDs (cam_site_id).
}
