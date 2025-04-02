
#' Spatially Subset Camera Site Locations
#'
#' Spatially subset camera site locations using an sf polygon object. There may
#' be times where it doesn’t make sense to include data from the entire Snapshot
#' Wisconsin camera network, and we should spatially subset camera locations. For
#' example, we may want to only consider the Northern Forest Zone instead of the
#' entire state of Wisconsin. We can filter locations, detections, and effort
#' based on a spatial layer, like deer management unit.
#'
#' @param locationeffort dataframe containing location and effort information from `effort_query()`
#' @param sf_layer as character string, sf polygon object by which to subset locationeffort data frame
#' @param zone_id as character string, ID column in sf_layer e.g.(bear_mgmt_unit_id)
#' @param filter_statement character string of right hand side of filter statment, to be passed along to subset spatial layer (e.g. "== 'A'")
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' spatial_subset(locs_df, _sf)
#' }
#' @seealso `list_spatial_layers()`

spatial_subset <- function(locationeffort, sf_layer, zone_id, filter_statement=NULL) {

  if(!is.null(filter_statement)){

    # symbol <- str_extract(pattern = "^[[:symbol:]]{1,2}", string = filter_statement)
    # RHS <- str_extract(pattern = "[[:alnum:]]+", string = filter_statement)
    #

    subset_layer <-sswids::get_spatial_data(layer_name = sf_layer) %>%
    # these regions are based off of county boundaries, so we need to
    # remove ones that aren't surveyed before subsetting
    dplyr::filter({{zone_id}}, {{filter_statement}})#sprintf()
  }else{
    subset_layer <-
      sswids::get_spatial_data(layer_name = sf_layer) %>%
      # these regions are based off of county boundaries, so we need to
      # remove ones that aren't surveyed before subsetting
      dplyr::filter(!is.na({{zone_id}}))
  }

  #make data frame of unique camera locations
  colidx <- which(colnames(locationeffort) %in% c("CAMERA_LOCATION_SEQ_NO", "Longitude", "Latitude"))
  camlocs <- locationeffort%>%dplyr::select(all_of(colidx))%>%dplyr::distinct(CAMERA_LOCATION_SEQ_NO, .keep_all = TRUE)

  #turn camera locations dataframe into sf object
  locs_sf = camlocs %>%
    sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)

  # spatial join with spatial subset layer
  locs_sf_zones <-
    sf::st_join(
      locs_sf,
      # only keep furbearer_zones column
      get_spatial_data("counties") %>%
        sf::st_transform(., sf::st_crs(locs_sf)),

      join = sf::st_within
    )

  locs_sf_zones <- locs_sf_zones%>%filter(!is.na({{zone_id}}))

  locationeffort <- locationeffort%>%filter(CAMERA_LOCATION_SEQ_NO %in% locs_sf_zones$CAMERA_LOCATION_SEQ_NO)

  return(locationeffort)

}
