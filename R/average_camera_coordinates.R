
#' Title
#'
#' @param locations
#' @param effort_by_day
#'
#' @return
#' @export
#'
#' @examples

average_camera_coordinates <- function(locations, effort_by_day) {

  # use this for weighting lat/long averages below
  # active_days is summed across years if there are multiple years for a site
  effort_by_cam_site_df <-
    effort_by_day_df %>%
    dplyr::group_by(cam_site_id, camera_location_seq_no) %>%
    dplyr::tally(name = 'active_days') %>%
    dplyr::ungroup()

  # take our locations
  locations %>%
    # join in the cam_site_id- and camera_location_seq_no-specific effort
    dplyr::left_join(., effort_by_cam_site_df, by = "camera_location_seq_no") %>%
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
