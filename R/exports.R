#' Exports
#'
#' @export
#' @param conn a connection object. see [ChmpClient]
#' @param id
#' @param status
#' @param segment
#' @param since
#' @param hashed
#' @template all
#' @examples \dontrun{
#' (conn <- ChmpClient$new())
#' exports(id = "<id>")
#' }
exports <- function(conn, id, status = NULL, segment = NULL, 
  since = NULL, hashed = NULL, key = NULL, ...) {

  args <- ct(list(id = id, status = status, segment = segment, 
    since = since, hashed = hashed, apikey = conn$key %||% key))
  chmp_POST(conn$dc, "export/1.0/list", body = args, ...)
}
