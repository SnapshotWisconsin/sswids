
#' Title
#'
#' @param effort
#' @param detections
#'
#' @return
#' @export
#'
#' @examples

join_detections_effort <- function(effort_by_occ, detections, discard_na_rows = TRUE) {

  # then we can join detections (species counts) to particular observation periods
  # in the effort data frame
  effort <-
    effort %>%
    dplyr::left_join(., detections, by = c("cam_site_id", "year", "occ")) %>%
    # and fix counts while we're at it
    # NAs here are actually 0s; otherwise use count as is
    dplyr::mutate_at(
      dplyr::vars(
        matches("[A-Z]", ignore.case = FALSE)
      ),
      list(
        ~dplyr::case_when(
          is.na(.) ~ 0,
          TRUE ~ .
        )
      )
    )

  # but 0s should be converted back to NA if 1) the camera is not operational,
  # or 2) the proportion of photos classified dips below the desired threshold
  # during a sampling occasion
  effort <-
    effort %>%
    dplyr::mutate_at(
      dplyr::vars(
        matches("[A-Z]", ignore.case = FALSE)
      ),
      list(
        ~dplyr::case_when(
          days_active == 0 | ppn_classified < ppn_class_threshold ~ as.numeric(NA),
          TRUE ~ .
        )
      )
    )

  if (discard_na_rows == TRUE) {

    # find all site x year with all NA rows
    na_effort_df <-
      effort %>%
      # grab first 3 columns here plus any upper case (class keys)
      dplyr::select(year, cam_site_id, occ, matches("[A-Z]", ignore.case = FALSE)) %>%
      # only need 1 class key, so subset to first 4 columns (last is one class key)
      dplyr::select(1:4) %>%
      tidyr::pivot_wider(
        names_from = c(occ),
        # occ_1, occ_2...etc
        names_glue = "{'occ'}_{occ}",
        # use key column to fill
        values_from = 4
      ) %>%
      # create column than indicates all NA sampling occasions or not
      dplyr::mutate(
        all_na = dplyr::case_when(
          dplyr::if_all(contains("occ"), ~is.na(.)) ~ 'yes',
          TRUE ~ 'no'
        )
      ) %>%
      # all NA rows
      dplyr::filter(all_na == 'yes') %>%
      dplyr::select(year, cam_site_id)

    # discard from effort
    effort <-
      effort %>%
      dplyr::anti_join(., na_effort_df, by = c("cam_site_id", "year"))

  }

  return(effort)

}
