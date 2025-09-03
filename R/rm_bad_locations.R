#' Remove known bad locations
#'
#' Removes bad locations, specifically one camera that is actually in the UP of
#' Michigan and another with an erroneous location in the middle of a lake, as well as
#' imprecise locations as specified by the user. Imprecise locations are determined by
#' the number of digits after the decimal. The default is 4 which corresponds to an
#' accuracy of ~11m. Number of digits after the decimals scales 10x with each
#' added decimal place.Locations are considered precise if either the latitude OR
#' longitude coordinate has 4 decimal places (accounting for truncation of 0 as
#' last decimal place).
#'
#' @param locationeffort data frame of camera location and effort data from `query_effort()`
#' @param coordinate_precision numeric, number of decimal places to filter locations by. Default is 4.
#'
#' @return a data frame with known bad locations removed.
#' @export
#'
#' @examples
#' \dontrun{
#' Q2.test.typical <- rm_bad_locations(locationeffort = Q.test.typical,
#'                                     coordinate_precision = 4)
#' }
#'
#'
rm_bad_locations <- function (locationeffort=locationeffort, coordinate_precision=4){
#spatial_data, zone


colidx <- which(colnames(locationeffort) %in% c("camera_location_seq_no", "longitude", "latitude"))
camlocs <- locationeffort%>%dplyr::select(all_of(colidx))%>%dplyr::distinct(camera_location_seq_no, .keep_all = TRUE)


locs_sf = camlocs %>%
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

# extract north south furbearer zone for each camera site
# point-in-polygon operation
locs_sf_zones <-
  sf::st_join(
    locs_sf,
    # only keep furbearer_zones column
    get_spatial_data("counties") %>%
      sf::st_transform(., sf::st_crs(locs_sf)),

    join = sf::st_within
  )
upmi_loc = locs_sf_zones %>%
  dplyr::filter(is.na(COUNTY_NAM))

if(nrow(upmi_loc) > 0){
  camlocs = camlocs %>%
    dplyr::filter(camera_location_seq_no != upmi_loc$camera_location_seq_no)




} else{print("No UP location in dataset")
}

if(44282 %in% camlocs$camera_location_seq_no){

  print("Camera_location_seq_no 44282 was remvoed from dataset. \nCamera location in a lake, 1000 feet from shore, grid =WASB048, volunteer quit in 2023")
  camlocs <- camlocs%>%
    dplyr::filter(camera_location_seq_no != 44282) # Camera location in a lake, 1000 feet from shore, WASB048, volunteer quit in 2023

}

#remove locations without a high enough precision
camlocs <- camlocs %>% # locations data frame
  # function in next line needs to be done on a row-by-row basis
  rowwise() %>%
  # n_digits() is a custom function that counts digits after decimal
  mutate(
    lat_digits = n_digits(latitude),
    long_digits = n_digits(longitude)
  ) %>%
  ungroup()

  notprecise <- camlocs%>%filter(!(lat_digits >= coordinate_precision | long_digits >= coordinate_precision))
  cat("Removing camera locations without needed precision, cam_loc_seq_no... \n", notprecise$camera_location_seq_no)

  camlocs <- camlocs%>%
  # keep a location if digits for lat OR long >= coordinate_precision
  filter(lat_digits >= coordinate_precision | long_digits >= coordinate_precision) %>%
  # clean up; drop digit count columns
  select(-matches('digits'))

loceffort <- locationeffort%>%filter(camera_location_seq_no %in% camlocs$camera_location_seq_no)
return(loceffort)
}
