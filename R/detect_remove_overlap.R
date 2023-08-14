
#' Title
#'
#' @param effort_df
#'
#' @return
#' @export
#'
#' @examples

detect_remove_overlap <- function(effort_df) {

  # warnings indicate instances of no overlap
  suppressWarnings(
    effort_df <-
      effort_df %>%
      # look for overlap at the level of a cam_site_id and year
      dplyr::group_by(cam_site_id, year) %>%
      tidyr::nest() %>%
      # detect_batch_overlap() can take a while
      # overlap = number of days overlapping between two batches
      dplyr::mutate(overlap = purrr::map_dbl(data, detect_overlap)) %>%
      tidyr::unnest(data) %>%
      dplyr::ungroup() %>%
      # if 0/-Inf (only one batch or no overlap in batches); retain these
      dplyr::filter(overlap %in% c(-Inf, 0)) %>%
      # now discard column
      dplyr::select(-overlap)
  )

}
