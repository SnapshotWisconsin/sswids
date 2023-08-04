
#' Print a list of SSWI database tables from larger DNR database
#'
#' @return list of SSWI tables
#' @export
#'
#' @examples sswidb_tables()

sswidb_tables <- function() {

  cat('printing list of sswidb tables may take ~30s...\n\n')

  # unique sswidb tables from larger oracle db
  unique(stringr::str_subset(DBI::dbListTables(conn), 'SSWI'))

}
