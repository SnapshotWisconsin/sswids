
#' Download and unzip Wiscland2 landcover data from DNR GIS website
#' @importFrom utils download.file
#' @return
#' @export
#'
#' @examples

download_wiscland2 <- function() {

  download.file(
    url = 'https://p.widencdn.net/lkfpeb/wiscland2_landcover',
    destfile = here::here('data/spatial_data/wiscland2_landcover.zip')
    )

}
