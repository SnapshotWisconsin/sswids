
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

detect_overlap <- function(df) {

  df %>%
    # we need the start/end date of the next batch to calculate overlap
    mutate(
      next_start_date = dplyr::lead(start_date),
      next_end_date = dplyr::lead(end_date)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      days_overlap_next = date_overlap(c(start_date, end_date), c(next_start_date, next_end_date))
    ) %>%
    dplyr::ungroup() %>%
    # calculate the max days of overlap by camera_location_seq_no and year
    dplyr::summarise(days_overlap_next = max(days_overlap_next, na.rm = TRUE)) %>%
    dplyr::pull(days_overlap_next)

}
