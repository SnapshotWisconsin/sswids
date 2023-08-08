
#' Title
#'
#' @return
#' @export
#'
#' @examples

check_season_dates <- function() {

  create_season_dates(
    min_date,
    max_date,
    years
  ) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      season_length_days = purrr::map2_int(start_date, end_date, ~length(seq.Date(.x, .y, by = 'day')))
    )

}
