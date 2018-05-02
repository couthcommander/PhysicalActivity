#' Query ActiGraph File
#'
#' This function executes a SELECT query on an ActiGraph agd file.
#'
#' agd files are actually SQLite databases.  This function requires the
#' \code{RSQLite} package.  The user is encouraged to directly interface with
#' the database by creating a connection with the \code{DBI} package.  This has
#' been tested with agd files produced with ActiLife v6.11.
#'
#' @param datfile An agd file.
#' @param qry An SQL SELECT statement.
#'
#' @return data.frame with query results.
#'
#' @templateVar author cole
#' @template auth
#'
#' @seealso \code{\link{readActigraph}}
#'
#' @examples
#' \dontrun{
#' dat <- queryActigraph("actfile.agd", "SELECT * FROM data LIMIT 5")
#'
#' queryActigraph("actfile.agd", "SELECT * FROM settings")
#'
#' ## directly interface using DBI package
#' con <- DBI::dbConnect(RSQLite::SQLite(), "actfile.agd")
#' DBI::dbListTables(con)
#' DBI::dbDisconnect(con)
#' }
#' @export

queryActigraph <- function(datfile, qry) {
    if(!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("queryActigraph requires the RSQLite package, please install it.",
             call. = FALSE)
    }
    if(!requireNamespace("DBI", quietly = TRUE)) {
        stop("queryActigraph requires the DBI package, please install it.",
             call. = FALSE)
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), datfile)
    res <- DBI::dbGetQuery(con, qry)
    DBI::dbDisconnect(con)
    res
}
