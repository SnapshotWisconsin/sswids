
#' Download Wiscland2 Landcover Data
#'
#' Download and unzip Wiscland2 landcover data from DNR GIS website.
#' @importFrom utils download.file
#' @return Directory containing Wiscland2 rasters
#' @export
#'
#' @examples
#' \dontrun{
#' download_wiscland2()
#' }

download_wiscland2 <- function() {

  download.file(
    url = 'https://p.widencdn.net/lkfpeb/wiscland2_landcover',
    destfile = here::here('data/spatial_data/wiscland2_landcover.zip')
    )

}
