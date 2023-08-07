
#' Create Seasonal Data Frames to Query Detections and Effort
#'
#' Query effort and detection data from a set of season dates in each year of interest.
#' @param min_date Start date of query
#' @param max_date End date of query
#' @param years Years of data to query
#'
#' @return Data frame containing years and associated start and end dates
#' @export
#'
#' @examples
#' \dontrun{
#' # set min/max years for obtaining data
#' min_year <- 2017
#' max_year <- 2022
#'
#' # season start/end dates (in -MM-DD format)
#' min_date <- '-04-01'
#' max_date <- '-04-30'
#'
#' # what years of data to query?
#' years <- seq(min_year, max_year, 1)
#'
#' # create date ranges for each year;
#' # if spanning two years (i.e., winter) use start date year
#' # otherwise just use the year of the particular season
#' seasons_df <-
#' create_season_dates(
#' min_date,
#' max_date,
#' years
#' )
#' }

create_season_dates <- function(min_date, max_date, years) {

  if(as.numeric(stringr::str_sub(min_date, 2, 3)) > as.numeric(stringr::str_sub(max_date, 2, 3))) {

    cat('creating winter season date ranges (across years)')

    tibble::tibble(
      year = years,
      start_date = as.Date(stringr::str_c(year, min_date)),
      end_date = as.Date(stringr::str_c(year + 1, max_date))
    )

  } else {

    cat('creating season date ranges (within years)')

    tibble::tibble(
      year = years,
      start_date = as.Date(stringr::str_c(year, min_date)),
      end_date = as.Date(stringr::str_c(year, max_date))
    )

  }

}
