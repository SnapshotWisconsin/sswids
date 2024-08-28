
#' Remove overlapping effort
#'
#' Group effort dataframe into site-years and detects overlap in effort with
#' detect_overlap from (R/utils.R). THen filters out those site-years with overlapping
#' effort from detections and locations data frame using camera_Location_seq_no
#' as a key.
#'
#' The cam_site_id and season structure of the data provides the framework for
#' assessing overlapping effort and removing site-years with overlapping effort.
#' Overlapping effort can happen when a batch is associated with the wrong camera
#' location. In these cases there may be batches that have the same cam_site_id
#' and start and end active dates that overlap. Another common reason for
#' overlapping effort is if the camera has been programmed with the wrong date.
#' Even just 1 day different can result in batches with the same cam_site_id and
#' overlapping start and end active dates. Batches at the same cam_site_id and year
#' (site-year) are assessed for overlap in their start and end active dates. If
#' overlap is detected, ALL THE DATA FOR THAT SITE-YEAR IS REMOVED, the function
#' is greedy like that.There is potentially more manual work that could be done
#' to ‘save’ some effort. But, these fixes are manual and complicated with no
#' clear blanket solution. For now, the best decision is to detect and remove
#' site-years with overlapping effort. If there is no overlap detection at that
#' cam_site_id over the other years, then those data are retained.
#'
#' @param datalist list object containing location, effort, and detection dataframes.
#'
#' @return list object containing location, effort, and detection dataframes with overlapping effort of site-years removed.
#' @export
#'
#' @examples

detect_remove_overlap <- function(datalist) {

  # warnings indicate instances of no overlap
  suppressWarnings(
    effort_df_removeoverlap <-
      datalist[["effort DF"]] %>%
      # look for overlap at the level of a cam_site_id and year
      dplyr::group_by(cam_site_id, year) %>%
      tidyr::nest() %>% #makes list-column of dataframes with column name `data` fed in to map_dbl below
      # detect_batch_overlap() can take a while
      # overlap = number of days overlapping between two batches
      dplyr::mutate(overlap = purrr::map_dbl(data, detect_overlap)) %>% #detect_overlap in utils.R script
      tidyr::unnest(data) %>%
      dplyr::ungroup() %>%
      # if 0/-Inf (only one batch or no overlap in batches); retain these
      dplyr::filter(overlap %in% c(-Inf, 0)) %>%
      # now discard column
      dplyr::select(-overlap)
  )

  # only keep camera_location_seq_nos now found in cleaned effort in the locations data frame
  # KEEP BASED ON CAMERA_LOCATION_SEQ_NO INSTEAD OF CAM_SITE_ID. IF USING CAM_SITE_ID, YOU MAY KEEP CAMERA_LOCATION_SEQ_NOS THAT HAVE DROPPED OUT DUE TO EFFORT OVERLAP. THERE CAN BE MANY CAMERA_LOCATION_SEQ_NOS PER CAM_SITE_ID
  locs_df_removeoverlap <-
    datalist[["locs DF"]] %>%
    dplyr::filter(camera_location_seq_no %in% unique(effort_df_removeoverlap$camera_location_seq_no))

  # and retain appropriate site x year combinations in detection data frame while we're at it
  # UPDATED THIS FUNCTION TO JOIN WITH CAM_SITE_ID AND YEAR
  detections_df_removeoverlap <-
    datalist[["detections DF"]] %>%
    dplyr::inner_join(
      .,
      effort_df_removeoverlap %>% dplyr::distinct(cam_site_id, year),
      by = c("year", "cam_site_id")
    ) %>%
    # and clean up
    dplyr::select(cam_site_id, camera_location_seq_no, batch_seq_no, trigger_id, year, detection_datetime:count) %>%
    dplyr::arrange(cam_site_id, detection_datetime)

  list_removeoverlap <- list("locs DF"=locs_df_removeoverlap, "effort DF"=effort_df_removeoverlap,
                                  "detections DF"= detections_df_removeoverlap)
  return(list_removeoverlap)

}
