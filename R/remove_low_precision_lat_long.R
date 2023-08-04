
#' Title
#'
#' @param df
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr matches
#'
#' @return
#' @export
#'
#' @examples

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


# determine number of digits after decimal place for lat/long
n_digits <- function(x) {

  max(
    stringr::str_length(
      stringr::str_replace(
        x,
        ".*\\.(\\d+)",
        "\\1"
      )
    )
  )

}
