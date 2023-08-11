
#' Title
#'
#' @param effort
#' @param min_date
#' @param max_date
#' @param min_year
#' @param max_year
#'
#' @return
#' @export
#'
#' @examples

calculate_prop_classified <- function(effort, min_date, max_date, min_year, max_year) {

  # camera site by year combinations to retain for proportion classified
  # needed later when filtering the sswi photo table
  cam_loc_seq_no_x_year <-
    effort %>%
    distinct(camera_location_seq_no, cam_site_id, year)

  # first, lazily query classifications (i.e., don't load into memory yet)
  class_lazy <-
    conn %>%
    # classification table
    tbl("SSWI_CLASSIFICATION") %>%
    # get rid of photos with comment status (121; e.g., 'cool' photo comment)
    filter(METADATA_GROUP_SEQ_NO != 121) %>%
    # get rid of zooniverse no consensus (48 AND Z)
    filter(METADATA_GROUP_SEQ_NO != 48 | USER_GROUP_CODE != 'Z') %>%
    # only need one classification per trigger
    distinct(TRIGGER_SEQ_NO) %>%
    # used to tally 'classified' or not later
    mutate(classified = 'yes')

  # if seasons span years, add 1 below to end_date when filtering photo table; otherwise == 0
  year_plus_one <- add_year()

  # now query photo table
  # 1) query photo table to get all photos within season date range
  # 2) join classifications to these photos
  # 3) tally number classified, unclassified per camera_location_seq_no and date
  photo_df <-
    conn %>%
    # photo table
    tbl("SSWI_PHOTO") %>%
    # only want distinct triggers
    distinct(TRIGGER_SEQ_NO, UPLOAD_BATCH_SEQ_NO, CAMERA_LOCATION_SEQ_NO, FINAL_DATE_TIME, PHOTO_TYPE_CODE, DELETE_TRIGGER) %>%
    # only interested in motion triggers that shouldn't be deleted
    # and between min/max year
    filter(PHOTO_TYPE_CODE == 'MOTION_TRIGGERED' & DELETE_TRIGGER == 'NO' & year(FINAL_DATE_TIME) >= min_year & year(FINAL_DATE_TIME) <= max_year) %>%
    # filter photos within our season date ranges
    filter(
      # this filters season dates for each year
      FINAL_DATE_TIME >= to_date(paste(year(FINAL_DATE_TIME), min_date, sep = ''), "YYYY-MM-DD"),
      # to have inclusive end date add 1 here (and add year if season spans years)
      FINAL_DATE_TIME < to_date(paste(year(FINAL_DATE_TIME) + year_plus_one, max_date, sep = ''), "YYYY-MM-DD") + 1
    ) %>%
    # join classifications to triggers
    left_join(., class_lazy, by = 'TRIGGER_SEQ_NO') %>%
    # clean up
    select(CAMERA_LOCATION_SEQ_NO, FINAL_DATE_TIME, TRIGGER_SEQ_NO, classified) %>%
    # convert datetime to date (this actually is still a datetime, but any given image on a day gets
    # a date with YYYY-MM-DD 00:00:00). this allows tallying photos by 'date'
    mutate(date = to_date(paste(year(FINAL_DATE_TIME), month(FINAL_DATE_TIME), day(FINAL_DATE_TIME), sep = "-"), "YYYY-MM-DD")) %>%
    # tally triggers classified or not (== NA) per camera_location_seq_no and date
    group_by(CAMERA_LOCATION_SEQ_NO, date, classified) %>%
    # this creates n, which is equal to number of triggers
    tally() %>%
    ungroup() %>%
    # organize
    arrange(CAMERA_LOCATION_SEQ_NO, date, classified) %>%
    # read data into memory now
    collect() %>%
    # fix date, add year
    mutate(
      date = as.Date(date),
      year = year(date)
    ) %>%
    # rename to join together to detections/effort/locs dfs later
    rename(camera_location_seq_no = CAMERA_LOCATION_SEQ_NO)

  # then join in cam_site_ids as we want to calculate prop. classified at a site-level
  # not by camera_location_seq_no
  photos_by_cam_df <-
    photo_df %>%
    # only keep focal cam sites/cam location seq nos/years with inner_join()
    inner_join(., cam_loc_seq_no_x_year, by = c("camera_location_seq_no", "year")) %>%
    select(cam_site_id, camera_location_seq_no, year, date, classified, n) %>%
    arrange(year, cam_site_id, date, classified) %>%

    # for clarity, classified == NA means photos haven't been classified
    # so just change these NAs to 'no' now
    mutate(classified = replace_na(classified, 'no')) %>%

    # now we need to count classified/unclassified photos by cam_site_id instead of camera_location_seq_no
    # this is because there can be multiple camera_location_seq_nos in the same location in a season
    group_by(cam_site_id, year, date, classified) %>%
    # total photos at a cam_site_id per date
    summarise(n = sum(n)) %>%
    ungroup() %>%

    # now create columns of counts of unclassified (classified_no) and classified (classified_yes) triggers
    # this helps for calculating proportions below
    pivot_wider(
      names_from = c(classified),
      names_glue = "{'classified'}_{classified}",
      values_from = n,
      # fill in missing classified_no/classified_yes values with 0
      values_fill = 0
    ) %>%

    # now tally the total number of photos per cam site and date by adding
    # the number of unclassified and classified triggers
    mutate(
      total = classified_no + classified_yes
    ) %>%
    # rename classified_yes to classified; just need this and total photos to get proportion
    select(cam_site_id, year, date, classified = classified_yes, total) %>%
    arrange(cam_site_id, year, date) %>%

    # add in day of season, occasion period for each date so the proportion classified (joined by date in photos)
    # can be calculated over a cam_site_id and sampling occasion

    # assign day to equal interval bin
    left_join(., day_occasion_df, by = c("year", "date")) %>%

    # now sum photos over sampling occasion for each camera
    # organize
    arrange(cam_site_id, year, date) %>%
    group_by(cam_site_id, year, occ) %>%
    summarise(
      # number classified in an occasion
      classified = sum(classified),
      # total photos in an occasion
      total = sum(total)
    ) %>%
    ungroup() %>%

    # at this point, there will be missing occasions if there are no photos over certain dates
    # zero-fill in any missing occasions; classified and total == 0 for missing data
    group_by(cam_site_id, year) %>%
    complete(occ = 1:num_occasions, fill = list(classified = 0, total = 0)) %>%
    ungroup()

  return(photos_by_cam_df)

}
