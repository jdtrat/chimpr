#' chimpr client
#'
#' @export
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
    #' @field dc (character) a data center
    dc = NULL,
    #' @field key (character) an API key
    key = NULL,

    #' @description Create a new `ChmpClient` object
    #' @param dc (character) a data center. default: us7. required.
    #' @param key (character) API key. Can either pass in here,
    #' or set with `Sys.setenv()` or pass in to each function call.
    #' optional if already set as an environment variable
    initialize = function(dc = "us7", key = NULL) {
      assert_is(dc, "character")
      self$dc <- dc
      self$key <- check_key(key)
    },

    #' @description print method for `ChmpClient` objects
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat("<ChmpClient>", sep = "\n")
      cat(paste0("  data center: ", self$dc %||% ""), sep = "\n")
      cat(paste0("  API key: ",
        if (nzchar(self$key)) "<private>" else "not set!"), sep = "\n")
    },

    #' @description A health check endpoint for the Mailchimp API
    #' @param ... Curl options passed to [crul::verb-GET]
    ping = function(...) {
      chmp_parse(chmp_GET(self$dc, "ping", self$key, query = list(), ...), TRUE)
    },

    #' @description The API root resource links to all other resources
    #' available in the API. Calling the root directory also returns details
    #' about the Mailchimp user account.
    #' @param fields (list/vector) A comma-separated list of fields to return.
    #' Reference parameters of sub-objects with dot notation.
    #' @param exclude_fields (list/vector) A comma-separated list of fields to
    #' exclude. Reference parameters of sub-objects with dot notation.
    #' @param parse (logical) Whether to parse to list (`FALSE`) or
    #' data.frame (`TRUE`). Default: `TRUE`
    #' @param ... Curl options passed to [crul::verb-GET]
    root = function(fields = NULL, exclude_fields = NULL, parse = TRUE, ...) {
      assert_is(parse, "logical")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      chmp_parse(chmp_GET(self$dc, "", self$key, query = args, ...), parse)
    }
  )
)
