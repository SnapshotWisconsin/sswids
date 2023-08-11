
#' Title
#'
#' @param seasons
#' @param effort
#' @param num_occasions
#'
#' @return
#' @export
#'
#' @examples

create_sampling_occasions <- function(seasons, effort, num_occasions) {

  # create day of season data frame to join occasion number to effort/detection dates
  day_occasion_df <-
    seasons %>%
    group_by(year) %>%
    nest() %>%
    # create date sequence for each year
    mutate(date = map(data, date_sequence)) %>%
    unnest(date) %>%
    select(-data) %>%
    # using row_number give day of season starting with day 1
    mutate(day_of_season = row_number()) %>%
    ungroup() %>%
    # split season into equal intervals (1-day, 3-day, ...1 week)
    # ntile() assigns each day into a sampling occasion
    mutate(
      occ = ntile(day_of_season, num_occasions)
    )

  # now summarize effort over sampling occasion
  effort <-
    effort %>%
    left_join(
      .,
      # rename date as date_active to match effort data frame
      day_occasion_df %>% select(date_active = date, day_of_season, occ),
      by = "date_active"
    ) %>%
    # remove any overlapping dates (i.e., when camera_location_seq_no effort ends/starts at same site)
    distinct(year, cam_site_id, date_active, day_of_season, occ) %>%
    group_by(year, cam_site_id, occ) %>%
    # calculate number of days per occasion by using add_count()
    add_count() %>%
    summarise(
      # days of effort for each cam_site x occ x year = max of n
      days_active = max(n)
    ) %>%
    ungroup()

  # zero-fill in effort for any missing occasions here
  # ONLY for cam_site_id x year combinations where a camera is already operational
  effort <-
    effort %>%
    group_by(cam_site_id, year) %>%
    complete(occ = 1:num_occasions, fill = list(days_active = 0)) %>%
    ungroup()

  return(effort)

}
