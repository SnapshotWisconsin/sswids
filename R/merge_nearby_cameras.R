
#' Merge Nearby Camera Locations
#'
#' Combine nearby camera_location_seq_no's and create new camera site IDs (cam_site_id).
#' Often, volunteers will maintain a camera then move it to another location very
#' close by, and it may be advantageous later on to combine these locations into
#' a single camera site. merge_nearby_cameras() does this for us and creates a new
#' camera location variable called cam_site_id that can contain one or more nearby
#' camera_location_seq_nos.
#'
#' Up to this point, locations are kept track of using camera_location_seq_no.
#' The camera_location_seq_no does not indicate a unique camera location. When
#' there is a new camera deployed or a deployment event created, even at the exact
#' same coordinates, a new camera_location_seq_no is created. Also, there are cases
#' when a new camera is deployed, sometimes at the exact same spot, but there are
#' new coordinates associated with that deployment. This step merges the
#' camera_location_seq_nos within a specified distance of each other together as
#' the same cam_site_id. A camera_location_seq_no is buffered and given a new ID
#' (cam_site_id) regardless of whether there is a camera nearby or not. In some
#' cases, multiple camera_location_seq_nos may be in the same exact location
#' (for example, a camera is deployed and later replaced with a different camera).
#' The lat lon for cam_site_id are created after weighting the location for cam_site_id
#' by effort at each camera location sequence number. This function also: removes
#'  cam_site_ids with overlapping effort of more than 1 day, and removes some columns
#'  to tidy up data frame (grid type, grid id, lat/long coordinates associated
#'  with camera location sequence number).
#'
#' @param locationeffort nested data frame of locations and effort
#' @param cam_distance numeric, distance (in meters) within which nearby camera locations should be combined,
#'                     this represents the distance between 2 camera locations. This number is divided in half
#'                     within the function to create a buffer around each camera (e.g. for cam_distance=100
#'                     makes 50m buffer around each camera, those buffers that intersect are dissolved into 1).
#'                     Default is 100m.
#'
#' @return nested data frame of locations and effort with new cam_site_id
#' @export
#'
#' @examples
#' \dontrun{
#' Q3.test.typical <- merge_nearby_cameras(locationeffort = Q2.test.typical,
#'                                         cam_distance = 100)
#' }

merge_nearby_cameras <- function(locationeffort, cam_distance=100) {

  colidx <- which(colnames(locationeffort) %in% c("camera_location_seq_no", "longitude", "latitude"))
  camlocs <- locationeffort%>%dplyr::select(all_of(colidx))%>%dplyr::distinct()


  locs_sf = camlocs %>%
    sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    # WI transverse mercator
    sf::st_transform(., 3071)


  # (cameras <X m apart with overlapping buffers below = same site)
  buffers_sf <-
    locs_sf %>%
    # buffer, dist represents the radius of circle around the point
    sf::st_buffer(dist = cam_distance/2)

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
    locationeffort %>%
    dplyr::left_join(., cam_in_buffer, by = "camera_location_seq_no") %>%
    # buffer id turns into final cam_site_id: cam_xxxx
    dplyr::mutate(cam_site_id = stringr::str_c('cam_', sprintf("%04d", buffer_id))) %>%
    # clean up
    dplyr::select(-buffer_id) %>%
    dplyr::arrange(cam_site_id, season)


  multiplecamlocseqno <- effort_df_camsiteid%>%dplyr::group_by(cam_site_id, season)%>%filter(dplyr::n()>1) %>% dplyr::summarize(n=dplyr::n())
  cat("cam_site_ids with multiple cam_loc_seq_no...\n")
  print(multiplecamlocseqno, n=nrow(multiplecamlocseqno))
  #tidyr::unnest(Q3, cols = c(effort))%>%tidyr::nest("effort"= any_of(effortvars))%>%dplyr::arrange(cam_site_id)


  #eliminate cam_site_id x seasons with overlapping effort of more than 1 day when camera was switched over, leaving in overlapped day
  #to be removed in later function calculating effort per occ
  #nest dataframe by cam_site_id now, camera_location_seq_no now in nested effort column
  Q4 <- tidyr::unnest(effort_df_camsiteid, cols = c(effort))%>%tidyr::nest(.by = cam_site_id, .key = "effort")%>%dplyr::arrange(cam_site_id)#must sort with code CAN"T SORT MANUALLY or you'll get mismatched effort data to cam_site_id
  #find duplicated dates of effort in each nested effort dataframe for each row of cam_site_id x season
  Q5 <- Q4%>%purrr::map(.x = .$effort, .f = ~.x[duplicated(.x$final_date) | duplicated(.x$final_date, fromLast=TRUE),])
  names(Q5) <- Q4$cam_site_id
  #pull out cam_site_id x seasons with overlapping effort data
  overlap <- Q5[lapply(Q5, function (x) nrow(x) > 2)==TRUE] # 2 because pulled out both duplicated rows in line 100
  badseasons <- lapply(overlap, function (x) unique(x$season))
  #filter rows with overlapping effort more than one day
  Q6 <- tidyr::unnest(Q4, cols = c(effort))%>%filter(!cam_site_id %in% unique(substr(names(overlap), 1, 8)) | !season %in% unique(unlist(badseasons)))%>%
    tidyr::nest(.by = cam_site_id, .key = "effort")
  printoverlap <- tibble::tibble("cam_site_id"=substring(names(overlap), 0,8),
                                 "season"=unlist(lapply(overlap, function(x) paste0(unique(x$season), collapse = ",")), use.names = FALSE),
                                 "camera_location_seq_no"=unlist(lapply(overlap, function(x) paste0(unique(x$camera_location_seq_no), collapse = ",")), use.names = FALSE))
  cat("\ncam sites x seasons removed due to overlapping effort of more than 1 day...\n")
  print(printoverlap, n=nrow(printoverlap))

  #average camera coordinates
  Q7 <- Q6%>%dplyr::mutate("lat"= purrr::map_dbl(.x=.$effort, .f=~mean(as.numeric(.x$latitude))), "lon"= purrr::map_dbl(.x=.$effort, .f=~mean(as.numeric(.x$longitude))))
  #remove columns in list column dataframe of indivual cam _loc_seq_no coordinates, grid type and grid id
  Q8 <- Q7%>%dplyr::mutate("effort"= purrr::map(.x= .$effort, .f= ~select(.x, -c(latitude, longitude, grid_type_code, dnr_grid_id))))
  Q9 <- tidyr::unnest(Q8, cols = c(effort))%>%tidyr::nest(.by = c(cam_site_id, season, lat, lon), .key = "effort")

  return(Q9)




}
