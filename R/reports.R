#' Reports class
#'
#' @export
#' @param id a report id. optional, but required for most methods
#' @param fields (list/vector) A comma-separated list of fields to return. 
#' Reference parameters of sub-objects with dot notation.
#' @param exclude_fields (list/vector) A comma-separated list of fields to 
#' exclude. Reference parameters of sub-objects with dot notation.
#' @format NULL
#' @usage NULL
#' @details xxx
#' @note not all routes are implemented thus far
#' @examples \dontrun{
#' (x <- ChmpReports$new())
#' x$all()
#' x$all(count = 2)
#' x$all(count = 2, offset = 2)
#' 
#' (x <- ChmpReports$new(id = "<id>"))
#' x$info()
#' x$info(parse = FALSE)
#' x$info(fields = "clicks")
#' x$abuse_reports()
#' x$advice()
#' x$open_details()
#' x$click_details()
#' # x$click_details("<id>")
#' # x$click_details("<id>", members = TRUE)
#' # x$click_details("<id>", members = TRUE, subscriber_hash = "<hash>")
#' x$domain_performance()
#' x$eepurl()
#' x$email_activity()
#' x$locations()
#' x$sent_to()
#' x$sub_reports()
#' x$unsubscribed()
#' }
ChmpReports <- R6::R6Class(
  "ChmpReports",
  public = list(
    id = NULL,

    initialize = function(id) {
      if (!missing(id)) {
        assert_is(id, "character")
        self$id <- id
      }
    },

    print = function(x, ...) {
      cat("<reports>", sep = "\n")
      cat(paste0("  id: ", self$id %||% "none"), sep = "\n")
    },

    all = function(fields = NULL, exclude_fields = NULL, count = 10, 
      offset = 0, type = NULL, before_send_time = NULL, 
      since_send_time = NULL, key = NULL, parse = TRUE, ...) {

      args <- ct(list(fields = fields, exclude_fields = exclude_fields, count = count,
        offset = offset, type = type, before_send_time = before_send_time, 
        since_send_time = since_send_time))
      private$get("reports", args, parse, key, ...)
    },

    info = function(fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("reports", self$id), args, parse, key, ...)
    },

    abuse_reports = function(report = NULL, fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      part <- if (is.null(report)) "abuse-reports" else file.path("abuse-reports", report)
      path <- file.path("reports", self$id, part)
      private$get(path, args, parse, key, ...)
    },

    advice = function(fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("reports", self$id, "advice"), args, parse, key, ...)
    },

    open_details = function(fields = NULL, exclude_fields = NULL, count = 10, offset = 0,
        since = NULL, key = NULL, parse = TRUE, ...) {
      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields, 
        count = count, offset = offset, since = since))
      private$get(file.path("reports", self$id, "open-details"), args, parse, key, ...)
    },

    click_details = function(link_id = NULL, members = FALSE, subscriber_hash = NULL, 
      fields = NULL, exclude_fields = NULL, count = 10, offset = 0, key = NULL, 
      parse = TRUE, ...) {
      
      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields, 
        count = count, offset = offset))
      part <- if (is.null(link_id)) "click-details" else file.path("click-details", link_id)
      path <- file.path("reports", self$id, part)
      if (members) {
        path <- file.path(path, "members")
        if (!is.null(subscriber_hash)) path <- file.path(path, subscriber_hash)
      }
      private$get(path, args, parse, key, ...)
    },

    domain_performance = function(fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("reports", self$id, "domain-performance"), args, parse, key, ...)
    },

    eepurl = function(fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("reports", self$id, "eepurl"), args, parse, key, ...)
    },

    email_activity = function(subscriber_hash = NULL, fields = NULL, exclude_fields = NULL,
      count = 10, offset = 0, since = NULL, key = NULL, parse = TRUE, ...) {

      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields, 
        count = count, offset = offset, since = since))
      part <- if (is.null(subscriber_hash)) "email-activity" else file.path("email-activity", subscriber_hash)
      path <- file.path("reports", self$id, part)
      private$get(path, args, parse, key, ...)
    },

    locations = function(fields = NULL, exclude_fields = NULL, count = 10, offset = 0,
        key = NULL, parse = TRUE, ...) {
      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields, 
        count = count, offset = offset))
      private$get(file.path("reports", self$id, "locations"), args, parse, key, ...)
    },

    sent_to = function(subscriber_hash = NULL, fields = NULL, exclude_fields = NULL,
      count = 10, offset = 0, key = NULL, parse = TRUE, ...) {

      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields, 
        count = count, offset = offset))
      part <- if (is.null(subscriber_hash)) "sent-to" else file.path("sent-to", subscriber_hash)
      path <- file.path("reports", self$id, part)
      private$get(path, args, parse, key, ...)
    },

    sub_reports = function(fields = NULL, exclude_fields = NULL, key = NULL, parse = TRUE, ...) {
      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("reports", self$id, "sub-reports"), args, parse, key, ...)
    },

    unsubscribed = function(subscriber_hash = NULL, fields = NULL, exclude_fields = NULL,
      count = 10, offset = 0, key = NULL, parse = TRUE, ...) {

      if (is.null(self$id)) stop("must initialize the class with 'id' for this method")
      args <- ct(list(fields = fields, exclude_fields = exclude_fields, 
        count = count, offset = offset))
      part <- if (is.null(subscriber_hash)) "unsubscribed" else file.path("unsubscribed", subscriber_hash)
      path <- file.path("reports", self$id, part)
      private$get(path, args, parse, key, ...)
    }
  ),

  private = list(
    get = function(path, args, parse, key, ...) {
      assert_is(parse, 'logical')
      assert_is(key, 'character')
      chmp_parse(chmp_GET(path, key, query = args, ...), parse) 
    }
  )
)
