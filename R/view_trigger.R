
#' Look at a set of triggers in .gif format
#'
#' @param trigger_seq_no trigger sequence number
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' view_trigger(trigger_seq_no = 21733907)
#' }


view_trigger <- function(trigger_seq_no) {

  # locate url for each trigger (set of 3 photos per trigger)
  photo_links <-
    sswidb::sswidb_trigger_photo_urls(conn, trigger_seq_no) %>%
    tibble::as_tibble() %>%
    dplyr::select(-PHOTO_TYPE_CODE) %>%
    dplyr::mutate(trigger_name = basename(PHOTO_LINK_URL))

  # end up with a zoomed in image on the viewer
  # can click button to open this to browser though
  magick::image_read(
    (photo_links$PHOTO_LINK_URL)
  )

}
