
#' Connect to Snapshot Wisconsin Database
#'
#' A wrapper around \code{sswidb::sswidb_connect_manually()} that allows Snapshot Wisconsin database connection
#' without a locally-stored yaml file containing credentials.
#' @param db_version Production ("PROD") or testing ("UAT") versions of the database.
#' @importFrom keyring key_get
#' @importFrom sswidb sswidb_connect_manually
#'
#' @return a \code{\link{dbPool}} object.
#' @export
#'
#' @examples
#' \dontrun{
#' # must set credentials first before connecting to database
#' # that only has to be done once before using connect_to_sswidb()
#' establish_sswidb_credentials(db_version = 'PROD')
#' connect_to_sswidb(db_version = 'PROD')
#' }
#' @seealso \code{\link{establish_sswidb_credentials}}

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
