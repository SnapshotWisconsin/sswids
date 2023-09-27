
#' Title
#'
#' @param species list of species
#' @param grid
#' @param season
#' @param prec
#' @return
#' @export
#'
#' @examples

query_raw_data <- function(species, grid, season, prec) {

  # create data frame to assign appropriate 'season year' to effort start dates below
  # if Dec 2017-Feb 2018, year is set as 2017; otherwise use regular year
  season_year_df <-
    season %>%
    dplyr::group_by(year) %>%
    tidyr::nest() %>%
    dplyr::mutate(start_date = purrr::map(data, date_sequence)) %>%
    tidyr::unnest(start_date) %>%
    dplyr::ungroup() %>%
    dplyr::select(-data)

  cat('querying detections from database...\n')

  # detections
  detections_df <-
    # take each season
    season %>%
    # iterate sswidb_detections() over each season's date range
    # _dfr produces a data frame as output
    purrr::map2_dfr(
      .x = .$start_date,
      .y = .$end_date,
      .f = ~sswidb::sswidb_detections(
        # set date range
        date_range = c(.x, .y),
        # query one or multiple species
        species_name = species,
        # query one or multiple grid types
        grid = grid,
        # set precision level
        prec = prec,
        conn = conn
      )
    ) %>%
    # clean up names (snakecase), convert to tibble
    tibble::as_tibble(.name_repair = janitor::make_clean_names) %>%
    # add year to detections from detection datetime
    # dplyr::mutate(year = lubridate::year(detection_datetime)) %>%
    dplyr::mutate(start_date = as.Date(detection_datetime)) %>%
    dplyr::left_join(., season_year_df, by = "start_date") %>%
    # keep only some of the detection columns
    dplyr::select(
      year,
      batch_seq_no,
      trigger_id,
      camera_location_seq_no,
      detection_datetime,
      class_method,
      class_key,
      species,
      count
    ) %>%
    # deal with NA counts (these are young or collar present)
    dplyr::mutate(
      count = dplyr::case_when(
        is.na(count) ~ 1,
        TRUE ~ count
      )
    )

  cat('querying effort from database...\n')

  # effort
  # now query database for effort across different years but same season
  effort_df <-
    season %>%
    purrr::map2_dfr(
      .x = .$start_date,
      .y = .$end_date,
      .f = ~sswidb::sswidb_effort(
        # set date range
        date_range = c(.x, .y),
        # query one or multiple grid types
        grid = grid,
        conn = conn
      )
    ) %>%
    # clean up names, convert to tibble
    tibble::as_tibble(.name_repair = janitor::make_clean_names) %>%
    # change start/end datetimes to start/end dates
    dplyr::mutate(dplyr::across(where(lubridate::is.POSIXt), as.Date)) %>%
    # add appropriate year to effort start_date (needed here if season dates span winter)
    dplyr::left_join(., season_year_df, by = 'start_date') %>%
    # not using 'active_days' as supplied by sswidb_effort()
    # instead will calculate camera effort in next script
    dplyr::select(camera_location_seq_no, batch_seq_no, year, start_date, end_date) %>%
    # organize
    dplyr::arrange(year, camera_location_seq_no, start_date)

  cat('querying locations from database...\n')

  # locations
  # get camera location lat/long coordinates
  locs_df <-
    sswidb::sswidb_location(
      # use unique camera locations from effort data frame
      camera_location_seq_no = unique(effort_df$camera_location_seq_no),
      conn = conn
    ) %>%
    # clean up names, convert to tibble
    tibble::as_tibble(.name_repair = janitor::make_clean_names) %>%
    # cam_loc_seq_no and id are the same thing
    dplyr::select(camera_location_seq_no = id, dnr_grid_id, lat, lon)

  return(
    list(
      detections = detections_df,
      effort = effort_df,
      locations = locs_df
    )
  )

}
