
#' Create Seasonal Data Frames to Query Detections and Effort
#'
#' Query effort and detection data from a set of season dates in each year of interest.
#' @param min_date character, start date of query, in the format "-mm-dd"
#' @param max_date character, end date of query, in the format "-mm-dd"
#' @param years Vector of the start and end years desired. `seq()` used within
#' function to create a sequence of years from start to end years. Change to a
#' vector of specific years desired if you want a discontinuous sequence of years
#' AND use argument discontinuous = TRUE.
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
#' #continuous year range
#' create_season_dates(min_date = '-12-01',
#'                     max_date = '-01-31',
#'                     years = c('2018','2024'))
#'
#'#discontinuous year range
#'dates2 <- create_season_dates(min_date = '-12-01',
#'                              max_date = '-01-31',
#'                              years = c('2018','2022','2024'),
#'                              discontinuous = TRUE)
#'
#'
#' }

create_season_dates <- function(min_date, max_date, years, discontinuous=FALSE) {

if (discontinuous == FALSE){
  if(length(years) == 2){
  if(years[1] <= years[2]){
  years <- seq(years[1], years[2], 1)
    }

  if(years[2] < years[1]){
    years <- seq(years[2], years[1], 1)
    }
  }else{
    years <- as.numeric(years)
  }
}else{
  years <- as.numeric(years)
}

  if(as.numeric(stringr::str_sub(min_date, 2, 3)) > as.numeric(stringr::str_sub(max_date, 2, 3))) {

    cat('creating winter season date ranges (season spans years)...\n')

    tibble::tibble(
      season=seq.int(length(years)),
      start_date = as.Date(stringr::str_c(years, min_date)),
      end_date = as.Date(stringr::str_c(years + 1, max_date))
    )

  } else {

    cat('creating season date ranges (season does not span years)...\n')

    tibble::tibble(
      season=seq.int(length(years)),
      start_date = as.Date(stringr::str_c(years, min_date)),
      end_date = as.Date(stringr::str_c(years, max_date)),
    )

  }

}
