
#' Set Snapshot Wisconsin database credentials just one time for future database connections
#'
#' Use \code{\link{keyring}} package to store Snapshot Wisconsin database credentials for
#' future database connections.
#' @param db_version Production ("PROD") or testing ("UAT") versions of the database.
#' @importFrom keyring key_set
#'
#' @return Confidential information saved in the credential store.
#' @export
#'
#' @examples
#' \dontrun{
#' # store credentials for production version of the Snapshot Wisconsin database
#' # this will create 3 pop-ups that require entry of 1) dsn, 2) uid, and 3) pwd
#' establish_sswidb_credentials(db_version = 'PROD')
#' }
#' #' @seealso \code{\link{connect_to_sswidb}}


establish_sswidb_credentials <- function(db_version) {

  if (db_version == 'PROD') {

    # set dsn
    key_set('snapshot_db_dsn')
    # set uid
    key_set('snapshot_db_uid')
    # set pwd
    key_set('snapshot_db_pwd')

  } else if (db_version == 'UAT') {

    # set dsn
    key_set('snapshot_uat_db_dsn')
    # set uid
    key_set('snapshot_uat_db_uid')
    # set pwd
    key_set('snapshot_uat_db_pwd')

  } else {

    stop('must use either "PROD" or "UAT" for db_version')

  }

}
