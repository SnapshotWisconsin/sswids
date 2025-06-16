#' Pull in detection from Snapshot database
#'
#' @param conn a connection to the sswi database
#' @param species character vector of species of interest.See `sswidb::sswidb_species`
#' @param grid specified as character vector. What camera grids to pull effort
#'             info for (e.g. "SSWI").
#' @param daterange preferably a data frame of start and end dates, with columns
#'                  "start_date" and "end_date". Can accommodate same ranges
#'                  across years. The date range to pull the effort for. If not
#'                  provided defaults to 2019-01-01 to  the current date - 1 year.
#' @param prec precision of classifications. Should be set to 0 or 0.95. 0.95
#'             indicates final data and returns FINAL data while 0 indicates data
#'             that is classified but includes classifications that don't meet
#'             accuracy standards.
#'
#' @return
#' @export
#'
#' @examples



query_detections <- function(conn, species, grid, daterange=NULL, prec) {

  if(is.null(daterange)){
    previousyear <- lubridate::year(Sys.Date()-365)
    start_date <- as.Date(stringr::str_c(previousyear, "-01-01"))
    end_date <- as.Date(stringr::str_c(previousyear, "-12-31"))
    daterange <- data.frame("start_date"=start_date, "end_date"=end_date)
  }

  if(!inherits(daterange, "data.frame")){
    daterange <- data.frame("start_date"=as.Date(min(daterange)), "end_date"=as.Date(max(daterange)))
  }

# detections
detections_df <-
  # take each season
  daterange %>%
  # iterate sswidb_detections() over each season's date range
  # _dfr produces a data frame as output
  purrr::map2(
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
  ) %>% purrr::list_rbind(names_to="season") %>%
  # clean up names (snakecase), convert to tibble
  tibble::as_tibble(.name_repair = janitor::make_clean_names) %>%
  # add year to detections from detection datetime
  # dplyr::mutate(year = lubridate::year(detection_datetime)) %>%
  # add in year from season data frame so it is correct for winter queries
  # keep only some of the detection columns
  dplyr::select(
    season,
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
}
