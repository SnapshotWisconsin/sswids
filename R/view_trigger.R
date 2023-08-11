
#' Look at a set of triggers in .gif format
#'
#' @param trigger_id numeric trigger ID
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' view_trigger(21733907)
#' }


view_trigger <- function(trigger_id) {

  # locate url for each trigger (set of 3 photos per trigger)
  photo_links <-
    sswidb::sswidb_trigger_photo_urls(conn, trigger_id) %>%
    tibble::as_tibble() %>%
    dplyr::select(-PHOTO_TYPE_CODE) %>%
    dplyr::mutate(trigger_name = basename(PHOTO_LINK_URL))

  # end up with a zoomed in image on the viewer
  # can click button to open this to browser though
  magick::image_read(
    (photo_links$PHOTO_LINK_URL)
  )

}
