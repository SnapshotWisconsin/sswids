
#' List available spatial layers in sswids
#'
#' @return a table with the layer name, description, and data type of available spatial layers
#' @export
#'
#' @examples

list_spatial_layers <- function() {

  tibble::tribble(
    ~layer_name, ~description, ~type,
    'bear_zones', 'bear hunting management zones', 'shapefile',
    'beaver_zones', 'beaver management zones', 'shapefile',
    'counties', 'county boundaries', 'shapefile',
    'county_local_roads', 'county and local roads', 'shapefile',
    'dmus', 'deer management units and zones', 'shapefile',
    'ecological_landscapes', 'ecological landscapes', 'shapefile',
    'elk_zones', 'north/central elk management zones', 'shapefile',
    'furbearer_zones', 'north/south trapping zones', 'shapefile',
    'major_roads', 'major roads', 'shapefile',
    'nlcd', 'national land cover database', 'raster',
    'open_water', 'open water bodies', 'shapefile',
    'pheasant_regions', 'core pheasant range', 'shapefile',
    'PLSS_QuarterSections', 'Section township info', 'shapefile',
    'rugged_grouse_hunting_zones', 'ruffed grouse hunting zones', 'shapefile',
    'ruffed_grouse_priority_areas', 'ruffed grouse priority areas', 'shapefile',
    'streams', 'streams and rivers', 'shapefile',
    'turkey_mgt_zones', 'turkey hunting management zones', 'shapefile',
    'wiscland2', 'wiscland land cover', 'raster',
    'wolf_zones', 'wolf hunting management zones', 'shapefile'
  ) %>%
    dplyr::arrange(type, layer_name) %>%
    print(n = Inf)

}
