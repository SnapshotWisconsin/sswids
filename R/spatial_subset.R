
#' Spatially Subset Camera Site Locations
#'
#' Spatially subset camera site locations using an sf polygon object.
#' @param df Data frame of camera locations
#' @param polygon_layer sf polygon object
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' spatial_subset(locs_df, _sf)
#' }

spatial_subset <- function(df, polygon_layer) {

  df %>%
    # polygon layer must be in 3071 projection
    sf::st_intersection(., polygon_layer %>% sf::st_transform(., 3071))

}
