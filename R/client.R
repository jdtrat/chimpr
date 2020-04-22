#' chimpr client
#'
#' @export
#' @param dc (character) a data center. default: us7. required.
#' @param key (character) API key. Can either pass in here,
#' or set with `Sys.setenv()` or pass in to each function call.
#' optional if already set as an environment variable
#'
#' @format NULL
#' @usage NULL
#'
#' @details
#' **Methods**
#'
#' - `ping(...)`
#'    - ... Curl options passed to [crul::HttpClient]
#'
#' - `root(fields = NULL, exclude_fields = NULL, parse = TRUE)`
#'    - fields (list/vector) A comma-separated list of fields to return.
#'      Reference parameters of sub-objects with dot notation.
#'    - exclude_fields (list/vector) A comma-separated list of fields to
#'      exclude. Reference parameters of sub-objects with dot notation.
#'    - parse (logical) Whether to parse to list (`FALSE`) or
#'      data.frame (`TRUE`). Default: `TRUE`
#'    - ... Curl options passed to [crul::HttpClient]
#'
#' @examples \dontrun{
#' (x <- ChmpClient$new(dc = "us7", key = "<your key>"))
#' (x <- ChmpClient$new())
#' x$dc
#' x$key
#' x$ping()
#' x$root()
#' }
ChmpClient <- R6::R6Class(
  "ChmpClient",
  public = list(
    dc = NULL,
    key = NULL,

    initialize = function(dc = "us7", key = NULL) {
      assert_is(dc, "character")
      self$dc <- dc
      self$key <- check_key(key)
    },

    print = function(x, ...) {
      cat("<ChmpClient>", sep = "\n")
      cat(paste0("  data center: ", self$dc %||% ""), sep = "\n")
      cat(paste0("  API key: ",
        if (nzchar(self$key)) "<private>" else "not set!"), sep = "\n")
    },

    ping = function(...) {
      chmp_parse(chmp_GET(self$dc, "ping", self$key, query = list(), ...), TRUE)
    },

    root = function(fields = NULL, exclude_fields = NULL, parse = TRUE, ...) {
      assert_is(parse, "logical")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      chmp_parse(chmp_GET(self$dc, "", self$key, query = args, ...), parse)
    }
  )
)
