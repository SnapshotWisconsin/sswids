
#' Title
#'
#' @param db_version
#' @importFrom keyring key_set
#' @importFrom sswidb sswidb_connect_manually
#'
#' @return
#' @export
#'
#' @examples

connect_to_sswidb <- function(db_version) {

  if (db_version == 'PROD') {

    conn <<-
      sswidb_connect_manually(
        server = NULL,
        dsn = key_get('snapshot_db_dsn'),
        uid = key_get('snapshot_db_uid'),
        pwd = key_get('snapshot_db_pwd')
      )

  } else if (db_version == 'UAT') {

    conn <<-
      sswidb_connect_manually(
        server = NULL,
        dsn = key_get('snapshot_uat_db_dsn'),
        uid = key_get('snapshot_uat_db_uid'),
        pwd = key_get('snapshot_uat_db_pwd')
      )

  } else {

    stop('must use either "PROD" or "UAT" for db_version')

  }

}
