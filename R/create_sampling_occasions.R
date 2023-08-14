
#' Title
#'
#' @param seasons
#' @param effort_by_day
#' @param num_occasions
#'
#' @return
#' @export
#'
#' @examples

create_sampling_occasions <- function(seasons, effort_by_day, num_occasions) {

  # create day of season data frame to join occasion number to effort/detection dates
  day_occasion_df <-
    seasons %>%
    dplyr::group_by(year) %>%
    tidyr::nest() %>%
    # create date sequence for each year
    dplyr::mutate(date = purrr::map(data, date_sequence)) %>%
    tidyr::unnest(date) %>%
    dplyr::select(-data) %>%
    # using row_number give day of season starting with day 1
    dplyr::mutate(day_of_season = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    # split season into equal intervals (1-day, 3-day, ...1 week)
    # ntile() assigns each day into a sampling occasion
    dplyr::mutate(
      occ = dplyr::ntile(day_of_season, num_occasions)
    )

  # now summarize effort over sampling occasion
  effort_by_day <-
    effort_by_day %>%
    dplyr::left_join(
      .,
      # rename date as date_active to match effort data frame
      day_occasion_df %>% dplyr::select(date_active = date, day_of_season, occ),
      by = "date_active"
    ) %>%
    # remove any overlapping dates (i.e., when camera_location_seq_no effort ends/starts at same site)
    dplyr::distinct(year, cam_site_id, date_active, day_of_season, occ) %>%
    dplyr::group_by(year, cam_site_id, occ) %>%
    # calculate number of days per occasion by using add_count()
    dplyr::add_count() %>%
    dplyr::summarise(
      # days of effort for each cam_site x occ x year = max of n
      days_active = max(n)
    ) %>%
    dplyr::ungroup()

  # zero-fill in effort for any missing occasions here
  # ONLY for cam_site_id x year combinations where a camera is already operational
  effort_by_day <-
    effort_by_day %>%
    dplyr::group_by(cam_site_id, year) %>%
    tidyr::complete(occ = 1:num_occasions, fill = list(days_active = 0)) %>%
    dplyr::ungroup()

  return(effort_by_day)

}
