
#' List available spatial layers in sswids
#'
#' @return a table with the layer name, description, and data type of available spatial layers
#' @export
#'
#' @examples

list_spatial_layers <- function() {

  tibble::tribble(
    ~layer_name, ~description, ~type,
    'dmus', 'deer management units and zones', 'shapefile',
    'counties', 'county boundaries', 'shapefile',
    'ecological_landscapes', 'ecological landscapes', 'shapefile',
    'turkey_mgt_zones', 'turkey hunting management zones', 'shapefile',
    'furbearer_zones', 'north/south trapping zones', 'shapefile',
    'wolf_zones', 'wolf hunting management zones', 'shapefile',
    'ruffed_grouse_priority_areas', 'ruffed grouse priority areas', 'shapefile',
    'bear_zones', 'bear hunting management zones', 'shapefile',
    'pheasant_regions', 'core pheasant range', 'shapefile',
    'major_roads', 'major roads', 'shapefile',
    'county_local_roads', 'county and local roads', 'shapefile',
    'streams', 'streams and rivers', 'shapefile',
    'open_water', 'open water bodies', 'shapefile',
    'wiscland2', 'wiscland land cover', 'raster',
    'nlcd', 'national land cover database', 'raster',
    'elk_zones', 'north/central elk management zones', 'shapefile'
  ) %>%
    dplyr::arrange(type, layer_name) %>%
    print(n = Inf)

}
