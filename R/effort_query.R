#' Pulls in effort data from database
#'
#' Querys database for information about effort at camera sites,
#' including classification effort. Combines data from DS_LOCATION_EFFORT,
#' SSWI_GRID_REF, and SSWI_CAMERA_LOCATION based on specified arguments.
#'
#' @param conn connection to database returned by `sswids::connect_to_sswidb()`
#' @param prec precision of classifications. Should be set to 0 or 0.95. 0.95
#'             indicates final data and returns FINAL data while 0 indicates data
#'             that is classified but includes classifications that don't meet
#'             accuracy standards.
#' @param grid specified as character vector. What camera grids to pull effort
#'             info for (e.g. "SSWI").
#' @param daterange preferably a data frame of start and end dates, with columns
#'                  "start_date" and "end_date". Can accommodate same ranges
#'                  across years. The date range to pull the effort for. If not
#'                  provided defaults to 2019-01-01 to  the current date - 1 year.
#' @param remove0Timelapse Logical, should rows with 0 timelapse photos be removed
#'                         from the table. These rows indicate camera site/days
#'                         where no timelapse photo was taken despite programming.
#'
#' @return a data frame with daily camera effort information as well as location and grid information.
#' @export
#'
#' @examples

effort_query <- function(conn, prec, grid, daterange=NULL, remove0Timelapse=TRUE){


if(prec == 0.95){
  effortcolumn <- "G83100.DS_LOCATION_EFFORT.CLASS_FINAL_TRIGGER_COUNT"
} else{
  effortcolumn <- "G83100.DS_LOCATION_EFFORT.CLASS_EFFORT_TRIGGER_COUNT"
}

if(is.null(daterange)){
  daterange <- data.frame("start_date"=as.Date('2019-01-01'), "end_date"=Sys.Date()-365)
}

if(!inherits(daterange, "data.frame")){
  daterange <- data.frame("start_date"=as.Date(min(daterange)), "end_date"=as.Date(max(daterange)))
}

query <- daterange%>%
  purrr::map2_chr(
    .x = .$start_date,
    .y = .$end_date,
    .f = ~ trimws(sprintf("SELECT G83100.DS_LOCATION_EFFORT.CAMERA_LOCATION_SEQ_NO,
                            G83100.DS_LOCATION_EFFORT.FINAL_DATE,
                            G83100.DS_LOCATION_EFFORT.MOTION_TRIGGER_COUNT,
                            G83100.DS_LOCATION_EFFORT.TIME_LAPSE_TRIGGER_COUNT,
                            %s,
                            G83100.SSWI_CAMERA_LOCATION.ORIG_HRZ_X_COORD_AMT,
                            G83100.SSWI_CAMERA_LOCATION.ORIG_HRZ_Y_COORD_AMT,
                            G83100.SSWI_GRID_REF.DNR_GRID_ID,
                            G83100.SSWI_GRID_REF.GRID_TYPE_CODE
                          FROM G83100.DS_LOCATION_EFFORT
                          INNER JOIN G83100.SSWI_CAMERA_LOCATION
                          ON G83100.SSWI_CAMERA_LOCATION.CAMERA_LOCATION_SEQ_NO = G83100.DS_LOCATION_EFFORT.CAMERA_LOCATION_SEQ_NO
                          INNER JOIN G83100.SSWI_GRID_REF
                          ON G83100.SSWI_GRID_REF.GRID_SEQ_NO       = G83100.SSWI_CAMERA_LOCATION.GRID_SEQ_NO
                          WHERE G83100.SSWI_GRID_REF.GRID_TYPE_CODE IN ('%s')
                          AND G83100.DS_LOCATION_EFFORT.FINAL_DATE >= TO_DATE('%s', 'YYYY-MM-DD')
                          AND G83100.DS_LOCATION_EFFORT.FINAL_DATE <= TO_DATE('%s', 'YYYY-MM-DD');",
                        effortcolumn,
                        paste0(grid, collapse = "', '"),
                        format(.x, '%Y-%m-%d'),
                        format(.y, '%Y-%m-%d'))))



#if(nrow(daterange) > 1){
df <-
  query %>%
  purrr::map(
    .x = .,
    .f = ~DBI::dbGetQuery(conn, .x))%>% purrr::list_rbind(names_to="season")
#}else{
#df <- DBI::dbGetQuery(conn, query)
#}
if (remove0Timelapse==TRUE){
df <- df[df$TIME_LAPSE_TRIGGER_COUNT > 0,]
}
colnames(df)[7:8] <- c("Longitude", "Latitude")

class_col <- grep(pattern="CLASS_.*_TRIGGER_COUNT",x=colnames(df), value=TRUE)
df$prop_classified <- df[,class_col]/df$MOTION_TRIGGER_COUNT
df <- df[,c(2:6,11,7:8,1,9:10)]
# clean up names (snakecase), convert to tibble
names(df) <- janitor::make_clean_names(names(df))
effortvars <- c("final_date", "motion_trigger_count", "time_lapse_trigger_count", "class_final_trigger_count", "class_effort_trigger_count", "prop_classified")
df <- df%>%tidyr::nest(effort=any_of(effortvars))%>%dplyr::arrange(camera_location_seq_no)

return(df)
}




