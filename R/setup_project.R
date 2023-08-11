
#' Create Project Directories
#'
#' Create project directories to store SSWI data and outputs.
#' @return empty data and output folders
#' @export
#'
#' @examples
#' setup_project()

setup_project <- function() {

  # write data folder
  fs::dir_create(here::here('data'))

  # write output folder
  fs::dir_create(here::here('output'))

}
