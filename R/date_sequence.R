
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

date_sequence <- function(df) {

  seq.Date(as.Date(df$start_date), as.Date(df$end_date), by = 'day')

}
