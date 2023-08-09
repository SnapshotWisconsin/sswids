
#' Title
#'
#' @param effort_df
#'
#' @return
#' @export
#'
#' @examples

effort_by_day <- function(effort_df) {

  effort_by_day_df <-
    effort_df %>%
    # do this by each cam_site_id, camera_location_seq_no, year, and batch combination
    dplyr::group_by(cam_site_id, camera_location_seq_no, year, batch_seq_no) %>%
    tidyr::nest() %>%
    # fills in dates between start/end dates
    dplyr::mutate(
      activity = purrr::map(data, activity_by_date)
    ) %>%
    tidyr::unnest(activity) %>%
    dplyr::ungroup() %>%
    # don't really need the batch ID at this point
    dplyr::select(cam_site_id, camera_location_seq_no, year, date_active) %>%
    # distinct here as end date can still overlap by 1 with start date in next row or batch
    # this occurs when an SD card is checked/replaced on the same day
    dplyr::distinct()

}
