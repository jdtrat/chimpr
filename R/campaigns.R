#' Campaigns class
#'
#' @export
#' @param conn a connection object. see [ChmpClient]
#' @param id a campgaign id. optional, but required for most methods
#' @param fields (list/vector) A comma-separated list of fields to return.
#' Reference parameters of sub-objects with dot notation.
#' @param exclude_fields (list/vector) A comma-separated list of fields to
#' exclude. Reference parameters of sub-objects with dot notation.
#' @format NULL
#' @usage NULL
#' @details xxx
#' @note not all routes are implemented thus far
#' @examples \dontrun{
#' (conn <- ChmpClient$new())
#' (x <- ChmpCampaigns$new(conn))
#' x$all()
#' x$all(count = 2)
#' x$all(count = 2, offset = 2)
#'
#' (x <- ChmpCampaigns$new(conn, id = "<id>"))
#' x$info()
#' x$info(parse = FALSE)
#' x$info(fields = "report_summary")
#' x$content()
#' x$feedback()
#' x$send_checklist()
#' }
ChmpCampaigns <- R6::R6Class(
  "ChmpCampaigns",
  public = list(
    conn = NULL,
    id = NULL,

    initialize = function(conn, id) {
      self$conn <- conn
      if (!missing(id)) {
        assert_is(id, "character")
        self$id <- id
      }
    },

    print = function(x, ...) {
      cat("<campaigns>", sep = "\n")
      cat(paste0("  id: ", self$id %||% "none"), sep = "\n")
    },

    all = function(fields = NULL, exclude_fields = NULL, count = 10,
      offset = 0, type = NULL, status = NULL, before_send_time = NULL,
      since_send_time = NULL, before_create_time = NULL,
      since_create_time = NULL, list_id = NULL, folder_id = NULL,
      sort_field = NULL, sort_dir = NULL, key = NULL, parse = TRUE, ...) {

      args <- ct(list(fields = fields, exclude_fields = exclude_fields,
        count = count, offset = offset, type = type, status = status,
        before_send_time = before_send_time, since_send_time = since_send_time,
        before_create_time = before_create_time,
        since_create_time = since_create_time, list_id = list_id,
        folder_id = folder_id, sort_field = sort_field, sort_dir = sort_dir))
      private$get("campaigns", args, parse, key, ...)
    },


    info = function(report = NULL, fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      private$check_id()
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("campaigns", self$id), args, parse, key, ...)
    },

    content = function(fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      private$check_id()
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("campaigns", self$id, "content"),
        args, parse, key, ...)
    },

    feedback = function(feedback_id = NULL, fields = NULL,
      exclude_fields = NULL, key = NULL, parse = TRUE, ...) {
      private$check_id()
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      part <- if (is.null(feedback_id)) "feedback"
        else file.path("feedback", feedback_id)
      path <- file.path("campaigns", self$id, part)
      private$get(path, args, parse, key, ...)
    },

    send_checklist = function(fields = NULL, exclude_fields = NULL,
        key = NULL, parse = TRUE, ...) {
      private$check_id()
      args <- ct(list(fields = fields, exclude_fields = exclude_fields))
      private$get(file.path("campaigns", self$id, "send-checklist"),
        args, parse, key, ...)
    }
  ),

  private = list(
    get = function(path, args, parse, key, ...) {
      assert_is(parse, "logical")
      assert_is(key, "character")
      chmp_parse(chmp_GET(self$conn$dc, path, self$conn$key %||% key,
        query = args, ...), parse)
    },

    check_id = function() {
      if (is.null(self$id)) {
        stop("must initialize the class with 'id' for this method")
      }
    }
  )
)
