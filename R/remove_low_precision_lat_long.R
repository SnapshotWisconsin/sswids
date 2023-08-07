
#' Filter Out Low Precision Camera Locations
#'
#' Remove camera_location_seq_no's that have a low precision latitude or longitude coordinate.
#' @param df Data frame of camera locations
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

remove_low_precision_lat_long <- function(df, coordinate_precision) {

  # locations data frame
  df %>%
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

}
