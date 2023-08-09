
#' Title
#'
#' @param locations
#' @param effort
#'
#' @return
#' @export
#'
#' @examples

average_camera_coordinates <- function(locations, effort) {

  # take our locations
  locations %>%
    # join in the cam_site_id- and camera_location_seq_no-specific effort
    dplyr::left_join(., effort, by = "camera_location_seq_no") %>%
    # organize
    dplyr::arrange(cam_site_id) %>%
    # convert these locations to meter projection before computing weighted average
    sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    sf::st_transform(., 3071) %>%
    # extract lat/long in separate columns; needed for weighted average calculation
    dplyr::mutate(
      lon = sf::st_coordinates(.)[,1],
      lat = sf::st_coordinates(.)[,2]
    ) %>%
    # this doesn't need to be sf object anymore
    sf::st_drop_geometry() %>%
    dplyr::group_by(cam_site_id) %>%
    # average coordinates of cam sites weighted by number of days active per camera_location_seq_no
    # do this within each cam_site_id
    dplyr::mutate(
      lon = weighted.mean(lon, active_days),
      lat = weighted.mean(lat, active_days)
    ) %>%
    dplyr::ungroup() %>%
    # tidy up
    dplyr::select(cam_site_id, camera_location_seq_no, lon, lat)

}
