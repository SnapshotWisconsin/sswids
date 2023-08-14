
#' Create Project Directories
#'
#' Create project directories to store SSWI data and outputs.
#' @return empty data and output folders
#' @export
#'
#' @examples
#' setup_project()

setup_project <- function() {

  # write data folders; raw
  fs::dir_create(here::here('data_raw'))

  # clean
  fs::dir_create(here::here('data_clean'))

  # write output folder
  fs::dir_create(here::here('output'))

}
