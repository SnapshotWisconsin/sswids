
#' Create Seasonal Data Frames to Query Detections and Effort
#'
#' Query effort and detection data from a set of season dates in each year of interest.
#' @param min_date Start date of query
#' @param max_date End date of query
#' @param years Vector of the start and end years desired. `seq()` used within
#' function to create a sequence of years from start to end years. Change to a
#' vector of specific years desired if you want a discontinuous sequence of years.
#' @param discontinuous logical indicating whether you want a discontinuous
#' range of years from which to pull data. Defaults to FALSE. If TRUE, change years
#' argument to specify the years you want (e.g. c(2019, 2021, 2023)) as opposed
#' to the start and end years
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

create_season_dates <- function(min_date, max_date, years, discontinuous=FALSE) {

if (discontinuous ==FALSE){
  if(years[1] <= years[2]){
  years <- seq(years[1], years[2], 1)
  }

  if(years[2] < years[1]){
    years <- seq(years[2], years[1], 1)
  }
}else{
  years <- years
}

  if(as.numeric(stringr::str_sub(min_date, 2, 3)) > as.numeric(stringr::str_sub(max_date, 2, 3))) {

    cat('creating winter season date ranges (season spans years)...\n')

    tibble::tibble(
      year = years,
      start_date = as.Date(stringr::str_c(year, min_date)),
      end_date = as.Date(stringr::str_c(year + 1, max_date))
    )

  } else {

    cat('creating season date ranges (season does not span years)...\n')

    tibble::tibble(
      year = years,
      start_date = as.Date(stringr::str_c(year, min_date)),
      end_date = as.Date(stringr::str_c(year, max_date))
    )

  }

}
