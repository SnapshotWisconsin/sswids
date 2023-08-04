
#' Title
#'
#' @param min_date
#' @param max_date
#' @param years
#'
#' @return
#' @export
#'
#' @examples

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
