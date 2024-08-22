
#' Save query to raw data frames for further prep/analysis
#'
#' Saves the query to three separate data frames in the data_raw folder of the
#' working directory, created in [setup_project()]. This also allows us to run
#' the query above just once.Locations dataframe is saved as .rds file to preserve
#' the number of coordinate digits.
#'
#' Filename argument is pasted into string with e.g. "data_raw/detections_df_".
#'
#' @param data result of data query
#' @param filename character string, optional argument to specify part of file name for data frames. Used just in case previous queries are saved in the data_raw folder already to avoid overwriting them.
#'
#' @return
#' @export
#'
#' @examples

write_raw_data <- function(data, filename = NULL) {
if(!is.null(filesnames)){
  # write detections
  data$detections %>%
    # vroom_write() is like write_csv(), but faster
    vroom::vroom_write(here::here(paste0('data_raw/detections_df_', filename, '.csv')), delim = ',')

  # write effort
  data$effort %>%
    vroom::vroom_write(here::here(paste0('data_raw/effort_df_', filename, '.csv')), delim = ',')

  # write locations...need to write as .rds because saving as csv might impact # of coordinate digits
  data$locations %>%
    readr::write_rds(here::here(paste0('data_raw/locations_df_', filename, '.rds')))
} else{
  # write detections
  data$detections %>%
    # vroom_write() is like write_csv(), but faster
    vroom::vroom_write(here::here('data_raw/detections_df_raw.csv'), delim = ',')

  # write effort
  data$effort %>%
    vroom::vroom_write(here::here('data_raw/effort_df_raw.csv'), delim = ',')

  # write locations...need to write as .rds because saving as csv might impact # of coordinate digits
  data$locations %>%
    readr::write_rds(here::here('data_raw/locations_df_raw.rds'))
  }

}
