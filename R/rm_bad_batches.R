
#' Removes impossible dates outside the start and end active dates of a batch
#'
#' At least some of these are errors because of the location start date not
#' overlapping with the batch start and end dates. The batch START_ACTIVE_DATE
#' and END_ACTIVE_DATE and the location START_DATE and END_DATE are both assessed
#' when determining start_date and end_date for effort from the sswidb_effort
#' function.Find all batches that have detections outside of batch start and end
#' active dates and remove those batches and all associated detections and locations.
#'
#' @param listobj
#'
#' @return
#' @export
#'
#' @examples

rm_bad_batches <- function(data=list_nolocs){
  batches_baddates = data[["locs DF"]] %>%
  left_join(data[["locs DF"]]) %>%
  mutate(date = as.Date(detection_datetime),
         outside = ifelse(date < start_date | date > end_date,1,0)) %>%
  filter(outside==1) %>%
  select(batch_seq_no) %>%
  distinct()

# test = detections_df_nolocs %>%
#   left_join(effort_df_nolocs) %>%
#   mutate(date = as.Date(detection_datetime),
#          outside = ifelse(date < start_date | date > end_date,1,0)) %>%
#   filter(outside==1)

detections_df_outsideactivedates = detections_df_nolocs %>%
  filter(!batch_seq_no %in% batches_baddates$batch_seq_no)

effort_df_outsideactivedates = effort_df_nolocs %>%
  filter(!batch_seq_no %in% batches_baddates$batch_seq_no)

locs_df_outsideactivedates = locs_df_nolocs %>%
  filter(camera_location_seq_no %in% effort_df_outsideactivedates$camera_location_seq_no)
}
