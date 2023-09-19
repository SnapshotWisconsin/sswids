
#' Plot detections spatially (by year or across all years)
#'
#' @param detections
#' @param locations
#' @param class_key
#' @param years
#'
#' @return
#' @export
#'
#' @examples

plot_detections <- function(detections, locations, class_key, years) {

  if (years == 'all') {

    detections %>%
      dplyr::filter(class_key == class_key) %>%
      dplyr::group_by(cam_site_id) %>%
      dplyr::summarize(total_detections = sum(count)) %>%
      dplyr::left_join(., locations %>% dplyr::select(-camera_location_seq_no), by = join_by(cam_site_id)) %>%
      sf::st_as_sf(coords = c('lon', 'lat'), crs = 3071) %>%
      mapview::mapview(cex = 'total_detections', layer.name = 'Total detections')

  } else {

    years <- sort(unique(detections$year))

    x <- list()

    for (i in seq_along(years)) {

      map <-
        detections %>%
        dplyr::filter(class_key == class_key & year == years[i]) %>%
        dplyr::group_by(cam_site_id) %>%
        dplyr::summarize(total_detections = sum(count)) %>%
        dplyr::left_join(., locations %>% dplyr::select(-camera_location_seq_no), by = join_by(cam_site_id)) %>%
        sf::st_as_sf(coords = c('lon', 'lat'), crs = 3071) %>%
        mapview::mapview(cex = 'total_detections', layer.name = stringr::str_c('Total detections', ' - ', years[i]))

      x[[i]] <- map

    }

    leafsync::sync(x, no.initial.sync = FALSE)

  }

}
