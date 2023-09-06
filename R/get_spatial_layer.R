
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

  } else if (layer_name == 'dmus') {

    layer <- sswids_spatial_layers$dmus

  } else if (layer_name == 'ecological_landscapes') {

    layer <- sswids_spatial_layers$ecological_landscapes

  } else if (layer_name == 'furbearer_zones') {

    layer <- sswids_spatial_layers$furbearer_zones

  } else if (layer_name == 'turkey_mgt_zones') {

    layer <- sswids_spatial_layers$turkey_mgt_zones

  } else if (layer_name == 'wolf_zones') {

    layer <- sswids_spatial_layers$wolf_zones

  } else if (layer_name == 'ruffed_grouse_priority_areas') {

    layer <- sswids_spatial_layers$ruffed_grouse_priority_areas

  } else if (layer_name == 'bear_zones') {

    layer <- sswids_spatial_layers$bear_zones

  }

  return(layer)

}
