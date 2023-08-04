
#' Create project directories to store SSWI data and outputs
#'
#' @return
#' @export
#'
#' @examples

setup_project <- function() {

  # write data folder
  fs::dir_create(here::here('data'))

  # also write subfolder for spatial data
  fs::dir_create(here::here('data/spatial_data'))

  # write output folder
  fs::dir_create(here::here('output'))

}
