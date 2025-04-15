#' Pulls in effort data from database
#'
#' Queries database for information about effort at camera sites,
#' including classification effort. Combines data from DS_LOCATION_EFFORT,
#' SSWI_GRID_REF, and SSWI_CAMERA_LOCATION based on specified arguments.
#'
#' A column called season is created in the output indicating the time period
#' repeated across years from which data is pulled, is often synonymous with year
#' but possibly different if the time period of interest spans years (e.g. over
#' winter). It is created based on the number of rows in your daterange input.
#'
#' Output is a nested table with camera-level information such as location sequence
#' number, lat, lon, and grid ID displayed across columns and effort data nested
#' within the effort list-column. See `tidyr::nest()` for more info.
#'
#' @param conn connection to database returned by `sswids::connect_to_sswidb()`
#' @param prec precision of classifications. Should be set to 0 or 0.95. 0.95
#'             indicates final data and returns class_final_trigger_count column
#'             in output data while 0 indicates data that is classified but includes
#'             classifications that don't meet accuracy standard, resulting in
#'             class_effort_trigger_count column being included in output.
#' @param grid specified as character vector. What camera grids to pull effort
#'             info for (e.g. "SSWI").
#' @param daterange preferably a data frame of start and end dates, with columns
#'                  "start_date" and "end_date" as created by `sswids::create_season_dates()`.
#'                  Can accommodate same ranges across years. The date range to
#'                  pull the effort for. If not provided defaults to January 1st
#'                  to December 31st of the previous year.
#' @param remove0Timelapse Logical, should rows with 0 timelapse photos be removed
#'                         from the table. These rows indicate camera site/days
#'                         where no timelapse photo was taken despite programming.
#'
#' @return a nested data frame with daily camera effort information as well as
#' location and grid information.See details for more info.
#' @export
#'
#' @examples
#' \dontrun{
#' #set parameters for query_effort
#' sswids::connect_to_sswidb(db_version = 'PROD')
#' grid <- "SSWI"
#' prec <- 0
#' species <- c("Bear","Raccoon")
#' daterange <- create_season_dates(min_date = "-06-05",
#'                                  max_date = "-07-02",
#'                                  years = c(2019,2022))
#' #run query_effort
#' Q.test.typical <- query_effort(conn = conn, prec = prec,
#'                                grid = grid, daterange = daterange,
#'                                remove0Timelapse = TRUE)
#' }
#' @seealso `create_season_dates()` `nest()`

query_effort <- function(conn, prec, grid, daterange=NULL, remove0Timelapse=TRUE){


if(prec == 0.95){
  effortcolumn <- "G83100.DS_LOCATION_EFFORT.CLASS_FINAL_TRIGGER_COUNT"
} else{
  effortcolumn <- "G83100.DS_LOCATION_EFFORT.CLASS_EFFORT_TRIGGER_COUNT"
}

if(is.null(daterange)){
  previousyear <- lubridate::year(Sys.Date()-365)
  start_date <- as.Date(stringr::str_c(previousyear, "-01-01"), tz="America/Chicago")
  end_date <- as.Date(stringr::str_c(previousyear, "-12-31"), tz="America/Chicago")
  daterange <- data.frame("start_date"=start_date, "end_date"=end_date)
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

df <- df%>%dplyr::arrange(camera_location_seq_no, final_date)%>%tidyr::nest(effort=any_of(effortvars))

return(df)
}




