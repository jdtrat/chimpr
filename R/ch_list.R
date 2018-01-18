#' Get a list
#'
#' @export
#' @param id (integer/numeric) a list id. required.
#' @param fields (list/vector) A comma-separated list of fields to return. 
#' Reference parameters of sub-objects with dot notation.
#' @param exclude_fields (list/vector) A comma-separated list of fields to 
#' exclude. Reference parameters of sub-objects with dot notation.
#' @template all
#' @examples \dontrun{
#' ch_list_('208fb01591')
#' ch_list('208fb01591')
#' ch_list('208fb01591', parse = FALSE)
#' 
#' ch_list('208fb01591', fields = "stats")
#' ch_list('208fb01591', fields = "stats.open_rate")
#' ch_list('208fb01591', fields = "campaign_defaults")
#' }
ch_list <- function(id, fields = NULL, exclude_fields = NULL,
    key = NULL, parse = TRUE, ...) {

  assert_is(parse, 'logical')
  ch_parse(ch_list_(id, fields, exclude_fields, key, ...), parse)
}

#' @export
#' @rdname ch_list
ch_list_ <- function(id, fields = NULL, exclude_fields = NULL, 
    key = NULL, ...) {

  assert_is(key, 'character')
  args <- ct(list(fields = fields, exclude_fields = exclude_fields))
  ch_GET(file.path("lists", id), key, query = args, ...)
}
