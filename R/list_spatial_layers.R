
#' Title
#'
#' @return
#' @export
#'
#' @examples

list_spatial_layers <- function() {

  tibble::tribble(
    ~layer_name, ~description,
    'dmus', 'deer management units and zones',
    'counties', 'county boundaries'
  )

}
