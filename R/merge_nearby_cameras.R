
#' Merge Nearby Camera Locations
#'
#' Combine nearby camera_location_seq_no's and create new camera site IDs (cam_site_id).
#' Often, volunteers will maintain a camera then move it to another location very
#' close by, and it may be advantageous later on to combine these locations into
#' a single camera site. merge_nearby_cameras() does this for us and creates a new
#' camera location variable called cam_site_id that can contain one or more nearby
#' camera_location_seq_nos. Then, cam_site_id is added to effort and detections.
#'
#' Up to this point, locations are kept track of using camera_location_seq_no.
#' The camera_location_seq_no does not indicate a unique camera location. When
#' there is a new camera deployed or a deployment event created, even at the exact
#' same coordinates, a new camera_location_seq_no is created. Also, there are cases
#' when a new camera is deployed, sometimes at the exact same spot, but there are
#' new coordinates associated with that deployment. This step merges the
#' camera_location_seq_nos within 100 m of each other together as the same cam_site_id.
#' A camera_location_seq_no is buffered and given a new ID (cam_site_id) regardless
#' of whether there is a camera nearby or not. In some cases, multiple
#' camera_location_seq_nos may be in the same exact location (for example, a
#' camera is deployed and later replaced with a different camera). The lat lon for
#' cam_site_id are created later once effort is finalized and tranformed to daily
#' effort for the purposes of creating a weighted average location
#' (see [average_camera_coordinates()]). This step does not result in any data loss.
#' It does set up the data structure to look for instances of overlapping effort,
#' next.
#'
#' @param datalist list object containing location, effort, and detection dataframes.
#' @param locations Data frame of camera_location_seq_no's and coordinates
#' @param cam_distance numeric, distance (in meters) in which nearby camera locations should be combined
#' @param effort_df Data frame of days of effort for each camera_location_seq_no
#'
#' @return list object containing location, effort, and detection dataframes with associated cam_site_id.
#' @export
#'
#' @examples

merge_nearby_cameras <- function(datalist, cam_distance) {

  locs_sf <-
    datalist[["locs DF"]] %>%
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
  effort_df_camsiteid <-
    datalist[["effort DF"]] %>%
    dplyr::left_join(., cam_in_buffer, by = "camera_location_seq_no") %>%
    # buffer id turns into final cam_site_id: cam_xxxx
    dplyr::mutate(cam_site_id = stringr::str_c('cam_', sprintf("%04d", buffer_id))) %>%
    # clean up
    dplyr::select(cam_site_id, camera_location_seq_no, batch_seq_no, year, start_date, end_date) %>%
    dplyr::arrange(cam_site_id, year, start_date, end_date)

  # add in cam_site_id to locations and detections. This will make the inner join more straightforward after overlapping effort is removed from effort_df
  locs_df_camsiteid <- datalist[["locs DF"]] %>%
    dplyr::left_join(.,
              effort_df_camsiteid %>% dplyr::select(cam_site_id,camera_location_seq_no) %>% dplyr::distinct(),
              by=(c("camera_location_seq_no")))

  detections_df_camsiteid <- datalist[["detections DF"]] %>%
    dplyr::left_join(.,
              effort_df_camsiteid %>% dplyr::select(cam_site_id,camera_location_seq_no,batch_seq_no) %>% dplyr::distinct(),
              by=(c("camera_location_seq_no","batch_seq_no")))

  list_camsiteid <- list("locs DF"=locs_df_camsiteid, "effort DF"=effort_df_camsiteid,
                                 "detections DF"= detections_df_camsiteid)
  return(list_camsiteid)


}
