
#' Print Snapshot Wisconsin Database Tables
#'
#' Print a list of Snapshot Wisconsin tables from the larger WI DNR Oracle database
#' @return list of Snapshot Wisconsin tables
#' @export
#'
#' @examples
#' \dontrun{
#' # first connect to Snapshot Wisconsin database
#' connect_to_sswidb(db_version = 'PROD')
#' # print Snapshot Wisconsin tables
#' sswidb_tables()
#' }

sswidb_tables <- function() {

  cat('printing list of sswidb tables may take ~30s...\n\n')

  # unique sswidb tables from larger oracle db
  unique(stringr::str_subset(DBI::dbListTables(conn), 'SSWI'))

}
