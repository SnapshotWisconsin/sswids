
#' Title
#'
#' @return
#' @export
#'
#' @examples

list_spatial_layers <- function() {

  tibble::tribble(
    ~layer_name, ~description,
    'dmus', 'deer management units and zones',
    'counties', 'county boundaries',
    'ecological_landscapes', 'ecological landscapes',
    'turkey_mgt_zones', 'turkey hunting management zones',
    'furbearer_zones', 'north/south trapping zones',
    'wolf_zones', 'wolf hunting management zones',
    'ruffed_grouse_priority_areas', 'ruffed grouse priority areas',
    'bear_zones', 'bear hunting management zones'
  ) %>%
    dplyr::arrange(layer_name) %>%
    print(n = Inf)

}
