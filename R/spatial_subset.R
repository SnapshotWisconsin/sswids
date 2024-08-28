
#' Spatially Subset Camera Site Locations
#'
#' Spatially subset camera site locations using an sf polygon object. There may
#' be times where it doesn’t make sense to include data from the entire Snapshot
#' Wisconsin camera network, and we should spatially subset camera locations. For
#' example, we may want to only consider the Northern Forest Zone instead of the
#' entire state of Wisconsin. We can filter locations, detections, and effort
#' based on a spatial layer, like deer management unit.
#'
#' @param datalist list object containing location, effort, and detection dataframes.
#' @param sf_layer sf polygon object
#' @param filter_statement character string, to be passed along to `filter()` to subset spatial layer
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' spatial_subset(locs_df, _sf)
#' }

spatial_subset <- function(datalist, sf_layer, filter_statement) {

  subset_layer <-
    sswids::get_spatial_data(layer_name = sf_layer) %>%
    # these regions are based off of county boundaries, so we need to
    # remove ones that aren't surveyed before subsetting
    dplyr::filter(eval(rlang::parse_expr(filter_statement)))

  # find cam_site_ids within intersection
  focal_cams <-
    datalist[["locs DF"]] %>%
    sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% sf::st_transform(., 3071) %>%
    sf::st_intersection(., sf::st_transform(subset_layer, 3071)) %>%
    dplyr::pull(cam_site_id)

  # use these to discard locations we don't need
  locs_df_spatialfilter <-
    datalist[["locs DF"]] %>%
    dplyr::filter(cam_site_id %in% focal_cams)

  # retain these sites in the detections and effort
  detections_df_spatialfilter <-
    datalist[["detections DF"]] %>%
    dplyr::filter(cam_site_id %in% unique(locs_df_spatialfilter$cam_site_id))

  effort_df_spatialfilter <-
    datalist[["effort DF"]] %>%
    dplyr::filter(cam_site_id %in% unique(locs_df_spatialfilter$cam_site_id))

  return(list("locs DF"=locs_df_spatialfilter, "effort DF"=effort_df_spatialfilter, "detections DF"=detections_df_spatialfilter))

}
