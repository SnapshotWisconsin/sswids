
#' Title
#'
#' @param layer_name
#'
#' @return
#' @export
#'
#' @examples

get_spatial_data <- function(layer_name = NULL, level = NULL, year = NULL) {

  if (layer_name == 'counties') {

    layer <- sswids_spatial_layers$counties

  } else if (layer_name == 'dmus') {

    layer <- sswids_spatial_layers$dmus %>% st_make_valid()

  } else if (layer_name == 'ecological_landscapes') {

    layer <- sswids_spatial_layers$ecological_landscapes

  } else if (layer_name == 'furbearer_zones') {

    layer <- sswids_spatial_layers$furbearer_zones

  } else if (layer_name == 'turkey_mgt_zones') {

    layer <- sswids_spatial_layers$turkey_mgt_zones

  } else if (layer_name == 'wolf_zones') {

    layer <- sswids_spatial_layers$wolf_zones %>% st_make_valid()

  } else if (layer_name == 'ruffed_grouse_priority_areas') {

    layer <- sswids_spatial_layers$ruffed_grouse_priority_areas

  } else if (layer_name == 'bear_zones') {

    layer <- sswids_spatial_layers$bear_zones

  } else if (layer_name == 'major_roads') {

    layer <- sf::st_read('C:/sswids_gis/shapefiles/major_roads.shp')

  } else if (layer_name == 'county_local_roads') {

    layer <- sf::st_read('C:/sswids_gis/shapefiles/county_local_roads.shp')

  } else if (layer_name == 'streams') {

    layer <- sf::st_read('C:/sswids_gis/shapefiles/24k_Hydro_Flowlines_(Rivers_Streams).shp')

  } else if (layer_name == 'open_water') {

    layer <- sf::st_read('C:/sswids_gis/shapefiles/24k_Hydro_Waterbodies_(Open_Water).shp')

  } else if (layer_name == 'wiscland2') {

    layer <-
      raster::raster(
        stringr::str_c(
          'C:/sswids_gis/wiscland2_landcover/wiscland2/wiscland2_dataset/', 'level', level, '/level', level, '/wiscland2_level', level, '.tif'
        )
      )

  }

  else if (layer_name == 'nlcd') {

    layer <-
      raster::raster(
        fs::dir_ls('C:/sswids_gis/nlcd_landcover', glob = '*.tiff') %>%
          tibble::as_tibble() %>%
          dplyr::filter(stringr::str_detect(value, stringr::str_c('NLCD_', year))) %>%
          dplyr::pull()
      )

  }

  return(layer)

}

