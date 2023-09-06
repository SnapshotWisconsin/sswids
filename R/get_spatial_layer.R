
#' Title
#'
#' @param layer_name
#'
#' @return
#' @export
#'
#' @examples

get_spatial_layer <- function(layer_name) {

  if (layer_name == 'counties') {

    layer <- sswids_spatial_layers$counties

  }

  return(layer)

}
