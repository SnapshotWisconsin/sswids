
#' Merge Nearby Camera Locations
#'
#' Combine nearby camera_location_seq_no's and create new camera site IDs (cam_site_id).
#' @param locs_df Data frame of camera_location_seq_no's and coordinates
#' @param cam_distance Distance (meters) in which nearby camera locations should be combined
#' @param effort_df Data frame of days of effort for each camera_location_seq_no
#'
#' @return
#' @export
#'
#' @examples

merge_nearby_cameras <- function(locs_df, cam_distance, effort_df) {

  locs_sf <-
    locs_df %>%
    # don't need the grid ID
    dplyr::select(-dnr_grid_id) %>%
    sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    # WI transverse mercator
    sf::st_transform(., 3071)

  # buffer each camera_location_seq_no by distance set above / 2
  # (cameras <X m apart with overlapping buffers below = same site)
  buffers_sf <-
    locs_sf %>%
    # buffer
    sf::st_buffer(dist = cam_distance / 2)

  # dissolve any overlapping buffers (i.e., nearby camera_location_seq_no's or identical locations)
  buffers_sf <-
    # dissolve and cast as polygon
    sf::st_cast(sf::st_union(buffers_sf), "POLYGON") %>%
    # re-format
    sf::st_as_sf() %>%
    # just give geometry column its usual sf name
    dplyr::rename(geometry = x) %>%
    # give each buffer a new id...these turn into cam_site_ids later
    dplyr::mutate(buffer_id = dplyr::row_number())

  # now determine which camera_location_seq_no(s) are in each buffer with st_join()
  # if camera_location_seq_nos are in the same buffer they become the same camera site below
  cam_in_buffer <-
    sf::st_join(
      locs_sf,
      buffers_sf,
      join = sf::st_within
    ) %>%
    # organize and clean up
    dplyr::arrange(buffer_id, camera_location_seq_no) %>%
    dplyr::select(buffer_id, camera_location_seq_no) %>%
    # doesn't need to be sf object anymore
    sf::st_drop_geometry()

  # join in buffer id to effort data
  effort_df <-
    effort_df %>%
    dplyr::left_join(., cam_in_buffer) %>%
    # buffer id turns into final cam_site_id: cam_xxxx
    dplyr::mutate(cam_site_id = stringr::str_c('cam_', sprintf("%04d", buffer_id))) %>%
    # clean up
    dplyr::select(cam_site_id, camera_location_seq_no, batch_seq_no, year, start_date, end_date) %>%
    dplyr::arrange(cam_site_id, year, start_date, end_date)

  return(effort_df)

}
