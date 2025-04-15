
#' Assign effort data to sampling occasions
#'
#' From user specified number of occasion arguments assigns days of effort to specific occasions.
#' Returns a data frame sorted by cam_site_id, season, and occasion with coordinates, start and
#' end dates, classification effort summarized by occasion and camera location sequence number(s).
#'
#' @param daterange data frame from `create_season_dates` or vector of dates specifying time
#'                frame of your data pull.
#' @param locationeffort nested data frame with location and effort data created by
#'                       `merge_nearby_cameras()`.
#' @param num_occasions numeric. Number of occasions to split daily effort data in to.
#'                      If the total time frame is not divisible by the number of occasions
#'                      extra days will be distributed to earlier occasions, with an extra day
#'                      added until there is no more remainder according to behavior of `ntile()`
#'                      function.
#' @param class_threshold numeric, the threshold for the proportion of photos classified by which
#'                        to filter effort data. Defaults to 0.95.
#'
#' @return
#' @export
#'
#' @examples

create_sampling_occasions <- function(daterange=NULL, locationeffort, num_occasions, class_threshold=0.95) {

  if(is.null(num_occasions)){
    print("Please specify the number of sampling occasions")
  }

  if(is.null(daterange)){
    previousyear <- lubridate::year(Sys.Date()-365)
    start_date <- as.Date(stringr::str_c(previousyear, "-01-01"), tz="America/Chicago")
    end_date <- as.Date(stringr::str_c(previousyear, "-12-31"), tz="America/Chicago")
    daterange <- data.frame("start_date"=start_date, "end_date"=end_date)
  }

  if(!inherits(daterange, "data.frame")){
    daterange <- data.frame("start_date"=as.Date(min(daterange)), "end_date"=as.Date(max(daterange)))
  }

  # create day of season data frame to join occasion number to effort/detection dates
  day_occasion_df <-
    daterange %>%
    dplyr::mutate(season = dplyr::row_number()) %>%
    dplyr::group_by(season) %>%
    tidyr::nest() %>%
    # create date sequence for each year
    dplyr::mutate(date = purrr::map(data, date_sequence)) %>%
    tidyr::unnest(date) %>%
    dplyr::select(-data) %>%
    # using row_number give day of season starting with day 1
    dplyr::mutate(day_of_season = dplyr::row_number()) %>%
    # split season into equal intervals (1-day, 3-day, ...1 week)
    # ntile() assigns each day into a sampling occasion
    dplyr::mutate(
      occ = dplyr::ntile(day_of_season, num_occasions)) %>%
      dplyr::ungroup()



  # now summarize effort over sampling occasion
  effort_by_day <-
    locationeffort %>%tidyr::unnest(cols=effort)%>%
    dplyr::left_join(
      .,
      # rename date as final_date to match effort data frame
      day_occasion_df %>% dplyr::select(final_date = date, day_of_season, occ, season),
      by = c("final_date", "season")
    ) %>%
    dplyr::group_by(season, occ) %>%
    dplyr::mutate(start_date=min(final_date),
           end_date=max(final_date))%>%
    dplyr::group_by(cam_site_id, season, occ, lat, lon, start_date,end_date)%>%
    # calculate number of days per camsite id x year x occasion by using add_count()
    #dplyr::add_count() %>%
      mutate(days_active=dplyr::n_distinct(final_date)) %>%
    dplyr::summarise(
      # days of effort for each cam_site x occ x year = max of n
      #days_active = max(n),
      dplyr::across(dplyr::ends_with("count"), ~sum(.x, na.rm=TRUE)),
      #add occasions with multiple camera_location_seq_no to same column
      camera_location_seq_no=paste(unique(camera_location_seq_no),collapse =","),
      days_active=unique(days_active)) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("class"), ~.x/motion_trigger_count, .names = "prop_classified"))%>%
    dplyr::ungroup()%>% dplyr::arrange(cam_site_id, season, occ)

  #rearrange columns
  effort_by_day <- effort_by_day[,c(1:10,13,12,11)]

  #filter by prop_classified which is at occasion level
  effort_by_day2 <- effort_by_day%>%dplyr::filter(prop_classified >= class_threshold)

  #i dont think this chunk is necessary with how we pull data now
  # zero-fill in effort for any missing occasions here
  # ONLY for cam_site_id x year combinations where a camera is already operational
  # effort_by_day <-
  #   effort_by_day %>%
  #   dplyr::group_by(cam_site_id, season) %>%
  #   tidyr::complete(occ = 1:num_occasions, fill = list(days_active = 0)) %>%
  #   dplyr::ungroup()

  return(effort_by_day2)

}
