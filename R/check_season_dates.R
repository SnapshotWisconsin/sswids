
#' Title
#'
#' @param seasons_df
#' @return
#' @export
#'
#' @examples

check_season_dates <- function(seasons_df) {

  seasons_df %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      season_length_days = purrr::map2_int(start_date, end_date, ~length(seq.Date(.x, .y, by = 'day')))
    )

}
