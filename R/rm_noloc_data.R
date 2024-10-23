
#' Remove data without location information
#'
#' Removes detections without Wisconsin location data. There are camera_location_seq_no's
#' in effort and detections dataframes that have no location data.
#' This is likely because of detection date times outside of the camera_location_seq_no
#' start and end dates.Also, there is 1 location that falls in the UP of Michigan.
#' Remove this location as well. This is an old
#' location from 2020 and earlier, so it cannot be corrected.
#'
#' @param locs_df raw locations dataframe from Snapshot database query
#' @param effort_df raw effort dataframe from Snapshot database query
#' @param detections_df raw detections dataframe from Snapshot database query
#' @param spatial_data character string returned by list_spatial_layers()
#' @param zone column name (unquoted!), specify column of spatial_data that contains zone or management unit id by which to identify locations outside the study area
#' @return a list containing filtered locations dataframe with UP and no location/effort data , filtered effort dataframe with UP and no location/effort data, filtered detection dataframe with UP and no location/effort data
#' @export
#'
#' @examples
#'

rm_noloc_data <- function (locs_df = locs_df_raw, effort_df = effort_df_raw, detections_df = detections_df_raw,
                           spatial_data = "counties", zone = county_name) {
# find the location in the UP of MI and remove it from detection, effort, and locations. IRON024, but only concerns 1 camera_location_seq_no

locs_sf = locs_df %>%
  sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# extract north south furbearer zone for each camera site
# point-in-polygon operation
locs_sf_zones <-
  sf::st_join(
    locs_sf,
    # only keep furbearer_zones column
    get_spatial_data(spatial_data) %>%
      sf::st_transform(., sf::st_crs(locs_sf)),

    join = sf::st_within
  )
upmi_loc = locs_sf_zones %>%
  dplyr::filter(is.na({{zone}}))

if(nrow(upmi_loc) == 0){
locs_df_nolocs = locs_df %>%
  dplyr::filter(camera_location_seq_no != upmi_loc$camera_location_seq_no)

# update effort
effort_df_nolocs = effort_df %>%
  dplyr::filter(camera_location_seq_no %in% locs_df_nolocs$camera_location_seq_no)

# remove detections with no locations/effort. This is for detections that have date times that are outside of camera_location_seq_no date times.
# this will also remove the detections from the UP of MI location

# update detections
# first to remove UP MI
detections_df_nolocs = detections_df %>%
  dplyr::filter(camera_location_seq_no != upmi_loc$camera_location_seq_no)
#filter(camera_location_seq_no %in% locs_df_nolocs$camera_location_seq_no)
} else{print("No UP location in dataset")}
# then to remove detections without effort or location data
detections_df_nolocs = detections_df_nolocs %>%
  dplyr::filter(camera_location_seq_no %in% locs_df_nolocs$camera_location_seq_no)

nolocs_list <- list("locs DF"=locs_df_nolocs, "effort DF"=effort_df_nolocs, "detections DF"= detections_df_nolocs)
return(nolocs_list)
}
