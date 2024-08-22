
#' Summarize detection data by cam site id-year-occasions
#'
#'
#'
#' @param detections detections dataframe
#' @param seasons seasons dataframe
#' @param summary_value character string either "max count" or "count triggers", indicating if you would like data summarized by the maximum count of detections in an occasion or the total number of triggers per occasion. Default value is "count triggers"
#'
#' @return
#' @export
#'
#' @examples

summarize_detections <- function(detections, seasons, summary_value = "count triggers") {
  #                  %in% c("count triggers", "max count")
  # convert detections to wide format
  # so there's a column of counts for different age/sex classes, etc.
  detections <-
    detections %>%
    tidyr::pivot_wider(
      names_from = c(class_key),
      values_from = count,
      # fill in 0 adult/young; male/female, etc. counts if absent
      values_fill = 0
    ) %>%
    # tidy up
    dplyr::arrange(year, cam_site_id, detection_datetime)

  day_occasion_df <-
    seasons %>%
    dplyr::group_by(year) %>%
    tidyr::nest() %>%
    dplyr::mutate(date = purrr::map(data, date_sequence)) %>%
    tidyr::unnest(date) %>%
    dplyr::select(-data) %>%
    dplyr::mutate(day_of_season =  dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(occ = dplyr::ntile(day_of_season, num_occasions))

  # now create date in the detection data frame
  # use that to join in the occasion #
  # and summarize detection counts by occasion
  detections <-
    detections %>%
    # date not in detections yet so create it
    dplyr::mutate(date = as.Date(detection_datetime)) %>%
    # assign day to equal interval bin
    dplyr::left_join(., day_occasion_df, by = c("year", "date")) %>%
    # matches() here picks out the sswi keys (which are always capitalized)
    dplyr::select(cam_site_id:species, date, day_of_season, occ, matches("[A-Z]", ignore.case = FALSE)) %>%
    dplyr::group_by(year, cam_site_id, occ) %>%
    if (summary_value == "max count") {
    # calculate max count over each occasion
    # this works as key column headings are capitalized
    dplyr::summarise(dplyr::across(matches("[A-Z]", ignore.case = FALSE), max, na.rm = TRUE))
        }else{
  # COUNT instead of MAX
  dplyr::summarise(dplyr::across(matches("[A-Z]", ignore.case = FALSE), ~sum(. > 0, na.rm=TRUE)))} %>%
    dplyr::arrange(cam_site_id, year, occ) %>%
    dplyr::ungroup()

  return(detections)

}
