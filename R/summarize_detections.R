
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
  if(!(summary_value %in% c("count triggers", "max count"))){
    stop("\nPlease supply either 'count triggers' or 'max count' as argument to summary_value")
  }
  # convert detections to wide format
  # so there's a column of counts for different age/sex classes, etc.
  detections <-
    detections %>%
    tidyr::pivot_wider(
      names_from = c(class_key),
      values_from = count,
      # fill in 0 adult/young; male/female, etc. counts if absent
      values_fill = 0
    ) %>% mutate(detection_date=as.Date(detection_datetime),
                 #cam_loc_seq_no must be same data type to later left_join
                 camera_location_seq_no=as.character(camera_location_seq_no))%>%
    # tidy up
    dplyr::arrange(season, camera_location_seq_no, detection_datetime)

  longerdelim <- tidyr::separate_longer_delim(Q4, camera_location_seq_no, delim = ",")
  joined1 <-  dplyr::right_join(detections, longerdelim,
                               by=dplyr::join_by("camera_location_seq_no", "season", between(detection_date, start_date, end_date)))





  if(!("season" %in% colnames(seasons))){
    seasons$season <- dplyr::row_number(seasons)
  }
  day_occasion_df <-
    seasons %>%
    dplyr::group_by(season) %>%
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
  if(summary_value == "max count"){detections <-
    detections %>%
    # date not in detections yet so create it
    dplyr::mutate(date = as.Date(detection_datetime)) %>%
    # assign day to equal interval bin
    dplyr::left_join(., day_occasion_df, by = c("season", "date")) %>%
    # matches() here picks out the sswi keys (which are always capitalized)
    dplyr::select(-c(batch_seq_no, trigger_id, detection_datetime, class_method)) %>%
    dplyr::group_by(camera_location_seq_no, season, occ) %>%
    # calculate max count over each occasion
    # this works as key column headings are capitalized
    dplyr::summarise(dplyr::across(tidyselect::matches("[A-Z]", ignore.case = FALSE), ~max(. , na.rm = TRUE))) %>%
    dplyr::arrange(camera_location_seq_no, season, occ) %>%
    dplyr::ungroup()
  } else{
    detections <-
      detections %>%
      # date not in detections yet so create it
      dplyr::mutate(date = as.Date(detection_datetime)) %>%
      # assign day to equal interval bin
      dplyr::left_join(., day_occasion_df, by = c("season", "date")) %>%
      # matches() here picks out the sswi keys (which are always capitalized)
      dplyr::select(-c(batch_seq_no, trigger_id, detection_datetime, class_method)) %>%
      dplyr::group_by(camera_location_seq_no, season, occ) %>%
      # COUNT instead of MAX
      dplyr::summarise(dplyr::across(tidyselect::matches("[A-Z]", ignore.case = FALSE), ~sum(. > 0, na.rm=TRUE))) %>%
      dplyr::arrange(camera_location_seq_no, season, occ) %>%
      dplyr::ungroup()
  }

  return(detections)

}
