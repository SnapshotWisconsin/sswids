
#' Spatially Subset Camera Site Locations
#'
#' Spatially subset camera site locations using an sf polygon object.
#' @param locations Data frame of camera locations
#' @param sf_layer sf polygon object
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' spatial_subset(locs_df, _sf)
#' }

spatial_subset <- function(locations, sf_layer) {

  # find cam_site_ids within intersection
  focal_cams <-
    locations %>%
    st_as_sf(coords = c('lon', 'lat'), crs = 3071) %>%
    st_intersection(., sf_layer %>% st_transform(., 3071)) %>%
    pull(cam_site_id)

  # use these to discard locations we don't need
  locations <-
    locations %>%
    filter(cam_site_id %in% focal_cams)

  return(locations)

}
