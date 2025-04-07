
#' Summarize detection data by cam site id-year-occasions
#'
#'
#'
#' @param detections detections dataframe
#' @param locationeffort data frame containing location and effort data
#' @param summary_value character string either "max count" or "count triggers", indicating if you would like data summarized by the maximum count of detections in an occasion or the total number of triggers per occasion. Default value is "count triggers"
#'
#' @return
#' @export
#'
#' @examples

summarize_detections <- function(detections, locationeffort, summary_value = "count triggers") {
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
                               by=dplyr::join_by("camera_location_seq_no", "season", between(detection_date, start_date, end_date)))%>%
    # and fix counts while we're at it
    # NAs here are actually 0s; otherwise use count as is
    dplyr::mutate(
      dplyr::across(
        matches("[A-Z]", ignore.case = FALSE),
        ~dplyr::case_when(
          is.na(.) ~ 0,
          TRUE ~ .
        )
      )
    )
  #reordering columns, getting rid of unnecessary columns
  joined2 <- joined1[,c(11,1,12,4,8,9,13:21)]%>%dplyr::arrange(cam_site_id, season, occ)




  # if(!("season" %in% colnames(seasons))){
  #   seasons$season <- dplyr::row_number(seasons)
  # }
  # day_occasion_df <-
  #   seasons %>%
  #   dplyr::group_by(season) %>%
  #   tidyr::nest() %>%
  #   dplyr::mutate(date = purrr::map(data, date_sequence)) %>%
  #   tidyr::unnest(date) %>%
  #   dplyr::select(-data) %>%
  #   dplyr::mutate(day_of_season =  dplyr::row_number()) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(occ = dplyr::ntile(day_of_season, num_occasions))

  # now create date in the detection data frame
  # use that to join in the occasion #
  # and summarize detection counts by occasion
  if(summary_value == "max count"){
    detections <-
    joined2  %>%
    dplyr::group_by(cam_site_id, season, occ) %>%
    # calculate max count over each occasion
    # this works as key column headings are capitalized
    dplyr::summarise(camera_location_seq_no=paste(unique(camera_location_seq_no),collapse =","),
                     dplyr::across(tidyselect::matches("[A-Z]", ignore.case = FALSE), ~max(. , na.rm = TRUE)),
                     across(lat:days_active, .fns = ~unique(.x))) %>%
    dplyr::arrange(cam_site_id, season, occ) %>%
    dplyr::ungroup()
  } else{
    detections <-
      joined2  %>%
      dplyr::group_by(cam_site_id, season, occ) %>%
      # COUNT instead of MAX
      dplyr::summarise(camera_location_seq_no=paste(unique(camera_location_seq_no),collapse =","),
                       dplyr::across(tidyselect::matches("[A-Z]", ignore.case = FALSE), ~sum(. > 0, na.rm=TRUE)),
                       across(lat:days_active, .fns = ~unique(.x))) %>%
      dplyr::arrange(cam_site_id, season, occ) %>%
      dplyr::ungroup()
  }

  return(detections)

}
