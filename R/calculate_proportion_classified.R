
#' Title
#'
#' @param effort_by_day
#' @param effort_by_occ
#' @param min_date
#' @param max_date
#' @param min_year
#' @param max_year
#'
#' @return
#' @export
#'
#' @examples

calculate_prop_classified <- function(seasons, effort_by_day, effort_by_occ, min_date, max_date, min_year, max_year) {

  cat('calculating the proportion of triggers classified may take several minutes...\n')

  # camera site by year combinations to retain for proportion classified
  # needed later when filtering the sswi photo table
  cam_loc_seq_no_x_year <-
    effort_by_day %>%
    dplyr::distinct(camera_location_seq_no, cam_site_id, year)

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

  mean_date_occasion_df <-
    day_occasion_df %>%
    group_by(year, occ) %>%
    summarise(
      # not exactly an ordinal date, just average day from the season start date
      mean_date = mean(day_of_season)
    ) %>%
    ungroup()

  # first, lazily query classifications (i.e., don't load into memory yet)
  class_lazy <-
    conn %>%
    # classification table
    dplyr::tbl("SSWI_CLASSIFICATION") %>%
    # get rid of photos with comment status (121; e.g., 'cool' photo comment)
    dplyr::filter(METADATA_GROUP_SEQ_NO != 121) %>%
    # get rid of zooniverse no consensus (48 AND Z)
    dplyr::filter(METADATA_GROUP_SEQ_NO != 48 | USER_GROUP_CODE != 'Z') %>%
    # only need one classification per trigger
    dplyr::distinct(TRIGGER_SEQ_NO) %>%
    # used to tally 'classified' or not later
    dplyr::mutate(classified = 'yes')

  # if seasons span years, add 1 below to end_date when filtering photo table; otherwise == 0
  year_plus_one <- add_year()

  # now query photo table
  # 1) query photo table to get all photos within season date range
  # 2) join classifications to these photos
  # 3) tally number classified, unclassified per camera_location_seq_no and date
  photo_df <-
    conn %>%
    # photo table
    dplyr::tbl("SSWI_PHOTO") %>%
    # only want distinct triggers
    dplyr::distinct(TRIGGER_SEQ_NO, UPLOAD_BATCH_SEQ_NO, CAMERA_LOCATION_SEQ_NO, FINAL_DATE_TIME, PHOTO_TYPE_CODE, DELETE_TRIGGER) %>%
    # only interested in motion triggers that shouldn't be deleted
    # and between min/max year
    dplyr::filter(PHOTO_TYPE_CODE == 'MOTION_TRIGGERED' & DELETE_TRIGGER == 'NO' & year(FINAL_DATE_TIME) >= min_year & year(FINAL_DATE_TIME) <= max_year) %>%
    # filter photos within our season date ranges
    dplyr::filter(
      # this filters season dates for each year
      FINAL_DATE_TIME >= to_date(paste(year(FINAL_DATE_TIME), min_date, sep = ''), "YYYY-MM-DD"),
      # to have inclusive end date add 1 here (and add year if season spans years)
      FINAL_DATE_TIME < to_date(paste(year(FINAL_DATE_TIME) + year_plus_one, max_date, sep = ''), "YYYY-MM-DD") + 1
    ) %>%
    # join classifications to triggers
    dplyr::left_join(., class_lazy, by = 'TRIGGER_SEQ_NO') %>%
    # clean up
    dplyr::select(CAMERA_LOCATION_SEQ_NO, FINAL_DATE_TIME, TRIGGER_SEQ_NO, classified) %>%
    # convert datetime to date (this actually is still a datetime, but any given image on a day gets
    # a date with YYYY-MM-DD 00:00:00). this allows tallying photos by 'date'
    dplyr::mutate(date = to_date(paste(year(FINAL_DATE_TIME), month(FINAL_DATE_TIME), day(FINAL_DATE_TIME), sep = "-"), "YYYY-MM-DD")) %>%
    # tally triggers classified or not (== NA) per camera_location_seq_no and date
    dplyr::group_by(CAMERA_LOCATION_SEQ_NO, date, classified) %>%
    # this creates n, which is equal to number of triggers
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    # organize
    dplyr::arrange(CAMERA_LOCATION_SEQ_NO, date, classified) %>%
    # read data into memory now
    dplyr::collect() %>%
    # fix date, add year
    dplyr::mutate(
      date = as.Date(date),
      year = lubridate::year(date)
    ) %>%
    # rename to join together to detections/effort/locs dfs later
    dplyr::rename(camera_location_seq_no = CAMERA_LOCATION_SEQ_NO)

  # then join in cam_site_ids as we want to calculate prop. classified at a site-level
  # not by camera_location_seq_no
  photos_by_cam_df <-
    photo_df %>%
    # only keep focal cam sites/cam location seq nos/years with inner_join()
    dplyr::inner_join(., cam_loc_seq_no_x_year, by = c("camera_location_seq_no", "year")) %>%
    dplyr::select(cam_site_id, camera_location_seq_no, year, date, classified, n) %>%
    dplyr::arrange(year, cam_site_id, date, classified) %>%

    # for clarity, classified == NA means photos haven't been classified
    # so just change these NAs to 'no' now
    dplyr::mutate(classified = tidyr::replace_na(classified, 'no')) %>%

    # now we need to count classified/unclassified photos by cam_site_id instead of camera_location_seq_no
    # this is because there can be multiple camera_location_seq_nos in the same location in a season
    dplyr::group_by(cam_site_id, year, date, classified) %>%
    # total photos at a cam_site_id per date
    dplyr::summarise(n = sum(n)) %>%
    dplyr::ungroup() %>%

    # now create columns of counts of unclassified (classified_no) and classified (classified_yes) triggers
    # this helps for calculating proportions below
    tidyr::pivot_wider(
      names_from = c(classified),
      names_glue = "{'classified'}_{classified}",
      values_from = n,
      # fill in missing classified_no/classified_yes values with 0
      values_fill = 0
    ) %>%

    # now tally the total number of photos per cam site and date by adding
    # the number of unclassified and classified triggers
    dplyr::mutate(
      total = classified_no + classified_yes
    ) %>%
    # rename classified_yes to classified; just need this and total photos to get proportion
    dplyr::select(cam_site_id, year, date, classified = classified_yes, total) %>%
    dplyr::arrange(cam_site_id, year, date) %>%

    # add in day of season, occasion period for each date so the proportion classified (joined by date in photos)
    # can be calculated over a cam_site_id and sampling occasion

    # assign day to equal interval bin
    dplyr::left_join(., day_occasion_df, by = c("year", "date")) %>%

    # now sum photos over sampling occasion for each camera
    # organize
    dplyr::arrange(cam_site_id, year, date) %>%
    dplyr::group_by(cam_site_id, year, occ) %>%
    dplyr::summarise(
      # number classified in an occasion
      classified = sum(classified),
      # total photos in an occasion
      total = sum(total)
    ) %>%
    dplyr::ungroup() %>%

    # at this point, there will be missing occasions if there are no photos over certain dates
    # zero-fill in any missing occasions; classified and total == 0 for missing data
    dplyr::group_by(cam_site_id, year) %>%
    tidyr::complete(occ = 1:num_occasions, fill = list(classified = 0, total = 0)) %>%
    dplyr::ungroup()

  effort_ppn_df <-
    effort_by_occ %>%
    dplyr::left_join(
      .,
      photos_by_cam_df,
      by = c("cam_site_id", "year", "occ")
    ) %>%
    # filter(is.na(classified)) %>% print(n=Inf)
    # there are some NAs now...seem to be when there is effort but no photos in the photo table
    # or 0 effort (and thus no photos)
    # so turn those into 0s now
    dplyr::mutate(
      classified = tidyr::replace_na(classified, 0),
      total = tidyr::replace_na(total, 0)
    ) %>%
    # calculate proportion classified
    dplyr::mutate(
      ppn_classified = case_when(
        # I think we want ppn to be 1 here as there are no photos to be classified?
        total == 0 ~ 1,
        TRUE ~ classified / total
      )
    ) %>%
    # add in mean date of sampling occasion
    dplyr::left_join(., mean_date_occasion_df, by = c("year", "occ")) %>%
    # organize
    dplyr::select(cam_site_id, year, occ, classified, total, ppn_classified, days_active, mean_date)

  return(effort_ppn_df)

}
