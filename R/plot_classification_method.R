
#' Title
#'
#' @param detections_df
#'
#' @return
#' @export
#'
#' @examples

plot_classification_method <- function(detections_df) {

  detections_df %>%
    dplyr::group_by(year, class_method) %>%
    dplyr::tally() %>%
    dplyr::mutate(
      freq = n / sum(n)
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(year, freq, color = class_method), size = 1) +
    ggplot2::geom_point(ggplot2::aes(year, freq, size = n, color = class_method), alpha = 7/10) +
    ggplot2::labs(
      x = 'Year',
      y = 'Proportion of triggers classified by method',
      size = 'Triggers',
      color = 'Method'
    )

}
