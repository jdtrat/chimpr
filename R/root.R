#' call the root route
#'
#' @export
#' @param fields (list/vector) A comma-separated list of fields to return. 
#' Reference parameters of sub-objects with dot notation.
#' @param exclude_fields (list/vector) A comma-separated list of fields to 
#' exclude. Reference parameters of sub-objects with dot notation.
#' @template all
#' @examples \dontrun{
#' chmp_root()
#' chmp_root(parse = FALSE)
#' }
chmp_root <- function(fields = NULL, exclude_fields = NULL, key = NULL, 
  parse = TRUE, ...) {

  assert_is(parse, 'logical')
  assert_is(key, 'character')
  args <- ct(list(fields = fields, exclude_fields = exclude_fields))
  chmp_parse(chmp_GET("", key, query = args, ...), parse)
}
