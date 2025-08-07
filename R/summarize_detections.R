
#' Summarize detection data by cam site id-year-occasions
#'
#'
#'
#' @param detections detections dataframe
#' @param locationeffort data frame containing location and effort data
#' @param summary_value character string either "max count" or "count triggers", indicating if you would like data summarized by the maximum count of detections in an occasion or the total number of triggers per occasion. Default value is "count triggers"
#' @param event_threshold numeric, specifying time in minutes between triggers for them to be considered independent events
#'
#' @return
#' @export
#'
#' @examples

summarize_detections <- function(detections, locationeffort, summary_value = "count triggers", event_threshold= NULL) {



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




  longerdelim <- tidyr::separate_longer_delim(locationeffort, camera_location_seq_no, delim = ",")
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

  if(!is.null(event_threshold)){
    if(summary_value == "max count"){
      stop("event_threshold argument is meant to be used with count triggers only")
    }
    colskeep <- c("cam_site_id", "season", "occ", "camera_location_seq_no", "start_date", "end_date", "detection_datetime", "species",
                  sort(grep(pattern = "[A-Z]*_AMT", x = colnames(joined1), value = TRUE)), "lat", "lon", "motion_trigger_count",
                  "time_lapse_trigger_count", grep(pattern = "class_.*_trigger_count", x = colnames(joined1), value = TRUE), "prop_classified", "days_active")
    joined2 <- joined1[,colskeep]%>%dplyr::arrange(cam_site_id, season, occ)
    joined2 <- joined2%>%group_by(cam_site_id, species)%>%dplyr::arrange(cam_site_id, detection_datetime)%>%mutate(deltaTime=difftime(detection_datetime, dplyr::lag(detection_datetime), units = "mins"))%>%
      ungroup()%>%dplyr::arrange(cam_site_id, season, occ)%>%mutate(event=cumsum(deltaTime >= event_threshold | is.na(deltaTime)))

      joined2 <- joined2%>%group_by(event)%>%dplyr::slice_head()


    }else{
  #reordering columns, getting rid of unnecessary columns
  colskeep <- c("cam_site_id", "season", "occ", "camera_location_seq_no", "start_date", "end_date",
                sort(grep(pattern = "[A-Z]*_AMT", x = colnames(joined1), value = TRUE)), "lat", "lon", "motion_trigger_count",
                "time_lapse_trigger_count", grep(pattern = "class_.*_trigger_count", x = colnames(joined1), value = TRUE), "prop_classified", "days_active")
  joined2 <- joined1[,colskeep]%>%dplyr::arrange(cam_site_id, season, occ)
  }




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
                     across(start_date:end_date, .fns = ~unique(.x)),
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
                       across(start_date:end_date, .fns = ~unique(.x)),
                       dplyr::across(tidyselect::matches("[A-Z]*_AMT", ignore.case = FALSE), ~sum(. > 0, na.rm=TRUE)),
                       across(lat:days_active, .fns = ~unique(.x))) %>%
      dplyr::arrange(cam_site_id, season, occ) %>%
      dplyr::ungroup()
  }

  return(detections)

}
