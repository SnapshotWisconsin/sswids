#' Remove known bad locations
#'
#' Removes bad locations, specifically one camera that is actually in the UP of
#' Michigan and another with an erroneous location in the middle of a lake.
#'
#' @param locationeffort data frame of camera location and effort data from `effort_query()`
#'
#' @return a data frame with known bad locations removed.
#' @export
#'
#' @examples
rm_bad_locations <- function (locationeffort=locationeffort, coordinate_precision){
#spatial_data, zone


colidx <- which(colnames(locationeffort) %in% c("camera_location_seq_no", "longitude", "latitude"))
camlocs <- locationeffort%>%dplyr::select(all_of(colidx))


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

if(42282 %in% camlocs$camera_location_seq_no){

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
  ungroup() %>%
  # keep a location if digits for lat OR long >= coordinate_precision
  filter(lat_digits >= coordinate_precision | long_digits >= coordinate_precision) %>%
  # clean up; drop digit count columns
  select(-matches('digits'))

loceffort <- locationeffort%>%filter(camera_location_seq_no %in% camlocs$camera_location_seq_no)
return(loceffort)
}
