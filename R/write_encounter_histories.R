
#' Write Encounter Histories for Abundance Analysis
#'
#' @param df
#'
#' @return csv files of encounter histories by class (e.g., age and sex)
#' @export
#'
#' @examples
#' \dontrun{
#' write_encounter_histories(df)
#' }

write_encounter_histories <- function(df) {

  # grab class keys from effort data
  class_keys <-
    colnames(
      df %>%
        dplyr::select(dplyr::matches("[A-Z]", ignore.case = FALSE))
    )

  # loop through each class key, filter effort, create encounter histories,
  # save by class key name
  for (i in seq_along(class_keys)) {

    cat('writing encounter histories for', class_keys[i], 'to csv...')

    cat("\n")

    # save each class key encounter history to csv
    df %>%
      dplyr::select(year, cam_site_id, occ, class_keys[i]) %>%
      tidyr::pivot_wider(
        names_from = c(occ),
        # occ_1, occ_2...etc
        names_glue = "{'occ'}_{occ}",
        values_from = class_keys[i]
      ) %>%
      vroom::vroom_write(here::here('data', stringr::str_c('eh_df_', class_keys[i], '.csv')), delim = ',', progress = FALSE)

  }

}
