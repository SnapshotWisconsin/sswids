
#' Title
#'
#' @return
#' @export
#'
#' @examples

sswidb_tables <- function() {

  cat('printing list of sswidb tables may take ~30s...\n\n')

  # unique sswidb tables from larger oracle db
  unique(stringr::str_subset(DBI::dbListTables(conn), 'SSWI'))

}
