
#' Filter Out Low Precision Camera Locations
#'
#' Remove camera_location_seq_no's that have a low precision latitude or longitude coordinate.
#' Does this first in locations dataframe before filtering the effort and detections
#' dataframes by camera_location_seq_no.
#'
#' Until summer 2023, there was no requirement for the number of decimal places
#' when recording latitude or longitude coordinates, or the ability to record trailing zeros in the database.
#'
#' @param datalist list object containing location, effort, and detection dataframes.
#' @param locations Data frame of camera locations
#' @param coordinate_precision Number of digits after decimal for determining whether a location is precise or not
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr matches
#'
#' @return Locations data frame with high precision coordinates
#' @export
#'
#' @examples
#' \dontrun{
#' # set coordinate precision
#' coordinate_precision <- 4
#' locs_df %>% remove_low_precision_lat_long(coordinate_precision)
#' }

remove_low_precision_lat_long <- function(datalist, coordinate_precision = coord_prec) {


  locs_df_removelowpreclocs <- datalist[["locs DF"]] %>% # locations data frame
    # function in next line needs to be done on a row-by-row basis
    rowwise() %>%
    # n_digits() is a custom function that counts digits after decimal
    mutate(
      lat_digits = n_digits(lat),
      long_digits = n_digits(lon)
    ) %>%
    ungroup() %>%
    # keep a location if digits for lat OR long >= coordinate_precision
    filter(lat_digits >= coordinate_precision | long_digits >= coordinate_precision) %>%
    # clean up; drop digit count columns
    select(-matches('digits'))



  # retain precise locations in the detections
  # this also drops out those detections that did not have locations in locs_df, but now those are removed above.
  detections_df_removelowpreclocs <-
    datalist[["detections DF"]] %>%
    filter(camera_location_seq_no %in% locs_df_removelowpreclocs$camera_location_seq_no)

  # and effort
  effort_df_removelowpreclocs <-
    datalist[["effort DF"]] %>%
    filter(camera_location_seq_no %in% locs_df_removelowpreclocs$camera_location_seq_no)

  list_removelowpreclocs <- list("locs DF"=locs_df_removelowpreclocs, "effort DF"=effort_df_removelowpreclocs,
                                  "detections DF"= detections_df_removelowpreclocs)
  return(list_removelowpreclocs)

}
