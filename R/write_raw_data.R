
# Save raw data frames for further prep/analysis
#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples

write_raw_data <- function(data) {

  # write detections
  data$detections %>%
    # vroom_write() is like write_csv(), but faster
    vroom::vroom_write(here::here('data/detections_df_raw.csv'), delim = ',')

  # write effort
  data$effort %>%
    vroom::vroom_write(here::here('data/effort_df_raw.csv'), delim = ',')

  # write locations...need to write as .rds because saving as csv might impact # of coordinate digits
  data$locations %>%
    readr::write_rds(here::here('data/locations_df_raw.rds'))

}
