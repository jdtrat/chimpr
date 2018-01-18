#' Get a list
#'
#' @export
#' @param id (integer/numeric) a list id. required.
#' @param month (character) a year/month character string of the year/month
#' you want. optional
#' @param fields (list/vector) A comma-separated list of fields to return. 
#' Reference parameters of sub-objects with dot notation.
#' @param exclude_fields (list/vector) A comma-separated list of fields to 
#' exclude. Reference parameters of sub-objects with dot notation.
#' @param count (integer/numeric) The number of records to return. 
#' Default value is 10.
#' @param offset (integer/numeric) The number of records from a collection 
#' to skip. Iterating over large collections with this parameter can be slow. 
#' Default value is 0.
#' @template all
#' @examples \dontrun{
#' ch_list_growth_history('208fb01591')$history
#' ch_list_growth_history('208fb01591', parse = FALSE)$history
#' ch_list_growth_history('208fb01591', month = "2017-06")
#' }
ch_list_growth_history <- function(id, month = NULL, fields = NULL, 
    exclude_fields = NULL, count = 10, offset = 0, key = NULL, 
    parse = TRUE, ...) {

  assert_is(parse, 'logical')
  ch_parse(
    ch_list_growth_history_(id, month, fields, exclude_fields, count, 
        offset, key, ...), 
    parse 
  )
}

ch_list_growth_history_ <- function(id, month = NULL, fields = NULL, 
    exclude_fields = NULL, count = 10, offset = 0, key = NULL, ...) {

  assert_is(key, 'character')
  args <- ct(list(fields = fields, exclude_fields = exclude_fields))
  path <- file.path("lists", id, "growth-history")
  path <- if (is.null(month)) path else file.path(path, month)
  ch_GET(path, key, query = args, ...)
}
