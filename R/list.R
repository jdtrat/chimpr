#' Lists class
#'
#' @export
#' @param fields (list/vector) A comma-separated list of fields to return.
#' Reference parameters of sub-objects with dot notation.
#' @param exclude_fields (list/vector) A comma-separated list of fields to
#' exclude. Reference parameters of sub-objects with dot notation.
#' @param subscriber_hash (character) a subscriber hash identifying a subscriber
#' @param count (integer/numeric) The number of records to return.
#' Default value is 10.
#' @param offset (integer/numeric) The number of records from a collection
#' to skip. Iterating over large collections with this parameter can be slow.
#' Default value is 0.
#' @template all
#' @note not all routes are implemented thus far
#' @examples \dontrun{
#' (conn <- ChmpClient$new())
#' (x <- ChmpList$new(conn, id = "<id>"))
#' x$info()
#' x$info(parse = FALSE)
#' x$info(fields = "stats")
#' x$info(fields = "stats.open_rate")
#' x$info(fields = "campaign_defaults")
#' x$activity()
#' x$abuse_reports()
#' x$clients()
#' x$growth_history()
#' x$growth_history("2018-02")
#' x$interest_categories()
#' x$locations()
#' x$members()
#' # x$member("<id>")
#' # x$member_activity("<id>")
#' # x$member_goals("<id>")
#' # x$member_notes("<id>")
#' }
ChmpList <- R6::R6Class(
  "ChmpLists",
  public = list(
    #' @field conn (character) a connection object, see [ChmpClient]
    conn = NULL,
    #' @field id (character) an id
    id = NULL,

    #' @description Create a new `ChmpList` object
    #' @param conn a connection object. see [ChmpClient]
    #' @param id (character) a mailchimp list id. required
    #' @return A new `ChmpList` object
    initialize = function(conn, id) {
      self$conn <- conn
      assert_is(id, "character")
      self$id <- id
    },

    #' @description print method for `ChmpList` objects
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat("<lists>", sep = "\n")
      cat(paste0("  id: ", self$id %||% ""), sep = "\n")
    },

    #' @description Get list info
    info = function(fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("lists", self$id), args, parse, key, ...)
    },

    #' @description List activity
    activity = function(fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("lists", self$id, "activity"), args, parse,
        key, ...)
    },

    #' @description List abuse reports
    #' @param report (character) a report id
    abuse_reports = function(report = NULL, fields = NULL,
      exclude_fields = NULL, key = NULL, parse = TRUE, ...) {
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      part <- if (is.null(report)) "abuse-reports"
        else file.path("abuse-reports", report)
      path <- file.path("lists", self$id, part)
      private$get(path, args, parse, key, ...)
    },

    #' @description List clients
    clients = function(fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("lists", self$id, "clients"), args, parse,
        key, ...)
    },

    #' @description Growth history
    #' @param month (character) a year/month character string of the year/month
    #' you want. optional
    growth_history = function(month = NULL, fields = NULL,
      exclude_fields = NULL, count = 10, offset = 0, key = NULL,
      parse = TRUE, ...) {

      assert_is(month, "character")
      assert_is(count, c("numeric", "integer"))
      assert_is(offset, c("numeric", "integer"))
      args <- ct(list(fields = fields, exclude_fields = exclude_fields,
        count = count, offset = offset))
      part <- if (is.null(month)) "growth-history"
        else file.path("growth-history", month)
      path <- file.path("lists", self$id, part)
      private$get(path, args, parse, key, ...)
    },

    #' @description Interest categories
    #' @param category_id (character) a category id 
    #' @param type (character) a type
    interest_categories = function(category_id = NULL, fields = NULL,
      exclude_fields = NULL, count = 10, offset = 0, type = NULL, key = NULL,
      parse = TRUE, ...) {

      assert_is(count, c("numeric", "integer"))
      assert_is(offset, c("numeric", "integer"))
      assert_is(type, "character")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields,
        count = count, offset = offset, type = type))
      part <- if (is.null(category_id)) "interest-categories"
        else file.path("interest-categories", category_id)
      path <- file.path("lists", self$id, part)
      private$get(path, args, parse, key, ...)
    },

    #' @description Locations
    locations = function(fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("lists", self$id, "locations"), args, parse,
        key, ...)
    },

    #' @description Info for a specific list member
    member = function(subscriber_hash, fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("lists", self$id, "members", subscriber_hash),
        args, parse, key, ...)
    },

    #' @description Activity for a specific list member
    member_activity = function(subscriber_hash, fields = NULL,
      exclude_fields = NULL, key = NULL, parse = TRUE, ...) {
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("lists", self$id, "members", subscriber_hash,
        "activity"), args, parse, key, ...)
    },

    #' @description Goals for a specific list member
    member_goals = function(subscriber_hash, fields = NULL,
      exclude_fields = NULL, key = NULL, parse = TRUE, ...) {
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("lists", self$id, "members", subscriber_hash,
        "goals"), args, parse, key, ...)
    },

    #' @description Notes for a specific list member
    member_notes = function(subscriber_hash, fields = NULL,
      exclude_fields = NULL, key = NULL, parse = TRUE, ...) {
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("lists", self$id, "members", subscriber_hash,
        "notes"), args, parse, key, ...)
    },

    #' @description Members info
    #' @param email_type an email type
    #' @param status a status value
    #' @param since_timestamp_opt time stamp since
    #' @param before_timestamp_opt time stamp before
    #' @param since_last_changed since last changed
    #' @param before_last_changed before last changed
    #' @param unique_email_id a unique email id
    #' @param vip_only vip only?
    #' @param interest_category_id an interest category id
    #' @param interest_ids interest ids
    #' @param interest_match an interest match
    members = function(fields = NULL, exclude_fields = NULL, count = 10,
      offset = 0, email_type = NULL, status = NULL, since_timestamp_opt = NULL,
      before_timestamp_opt = NULL, since_last_changed = NULL,
      before_last_changed = NULL, unique_email_id = NULL, vip_only = NULL,
      interest_category_id = NULL, interest_ids = NULL, interest_match = NULL,
      key = NULL, parse = TRUE, ...) {

      args <- ct(list(fields = fields, exclude_fields = exclude_fields,
        count = count, offset = offset, email_type = email_type,
        status = status, since_timestamp_opt = since_timestamp_opt,
        before_timestamp_opt = before_timestamp_opt,
        since_last_changed = since_last_changed,
        before_last_changed = before_last_changed,
        unique_email_id = unique_email_id,
        vip_only = vip_only, interest_category_id = interest_category_id,
        interest_ids = interest_ids, interest_match = interest_match))
      private$get(file.path("lists", self$id, "members"), args, parse, key, ...)
    }
  ),

  private = list(
    get = function(path, args, parse, key, ...) {
      assert_is(parse, "logical")
      assert_is(key, "character")
      chmp_parse(chmp_GET(self$conn$dc, path,
        self$conn$key %||% key, query = args, ...), parse)
    }
  )
)
