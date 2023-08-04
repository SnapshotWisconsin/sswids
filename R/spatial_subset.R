
#' Spatially subset camera site locations using an sf polygon object
#'
#' @param df
#' @param polygon_layer
#'
#' @return
#' @export
#'
#' @examples

spatial_subset <- function(df, polygon_layer) {

  df %>%
    # polygon layer must be in 3071 projection
    sf::st_intersection(., polygon_layer %>% sf::st_transform(., 3071))

}
