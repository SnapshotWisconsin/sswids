
#' Title
#'
#' @param db_version
#'
#' @return
#' @export
#'
#' @examples


establish_sswidb_credentials <- function(db_version) {

  if (db_version == 'PROD') {

    # set dsn
    keyring::key_set('snapshot_db_dsn')
    # set uid
    keyring::key_set('snapshot_db_uid')
    # set pwd
    keyring::key_set('snapshot_db_pwd')

  } else if (db_version == 'UAT') {

    # set dsn
    keyring::key_set('snapshot_uat_db_dsn')
    # set uid
    keyring::key_set('snapshot_uat_db_uid')
    # set pwd
    keyring::key_set('snapshot_uat_db_pwd')

  } else {

    stop('must use either "PROD" or "UAT" for db_version')

  }

}
