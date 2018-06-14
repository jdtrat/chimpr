#' Get lists
#'
#' @export
#' @param fields (list/vector) A comma-separated list of fields to return. 
#' Reference parameters of sub-objects with dot notation.
#' @param exclude_fields (list/vector) A comma-separated list of fields to 
#' exclude. Reference parameters of sub-objects with dot notation.
#' @param count (integer/numeric) The number of records to return. 
#' Default value is 10.
#' @param offset (integer/numeric) The number of records from a collection 
#' to skip. Iterating over large collections with this parameter can be slow. 
#' Default value is 0.
#' @param before_date_created (character) Restrict response to lists created 
#' before the set date.
#' @param since_date_created (character) Restrict results to lists created 
#' after the set date.
#' @param before_campaign_last_sent (character) Restrict results to lists 
#' created before the last campaign send date.
#' @param since_campaign_last_sent (character) Restrict results to lists 
#' created after the last campaign send date.
#' @param email (character) Restrict results to lists that include a specific 
#' subscriber's email address.
#' @param sort_field (character) Returns files sorted by the specified field. 
#' Possible Values: date_created
#' @param sort_dir (character) Determines the order direction for sorted 
#' results. Possible Values: ASC, DESC
#' @template all
#' @examples \dontrun{
#' chmp_lists_()
#' chmp_lists()
#' chmp_lists(parse = FALSE)
#' }
chmp_lists <- function(fields = NULL, exclude_fields = NULL, 
    count = NULL, offset = NULL, before_date_created = NULL,
    since_date_created = NULL, before_campaign_last_sent = NULL, 
    since_campaign_last_sent = NULL, email = NULL, sort_field = NULL, 
    sort_dir = NULL, key = NULL, parse = TRUE, ...) {

  assert_is(parse, 'logical')
  chmp_parse(chmp_lists_(fields, exclude_fields, count, offset, 
    before_date_created, since_date_created, before_campaign_last_sent, 
    since_campaign_last_sent, email, sort_field, sort_dir, key, 
    args, ...), parse)
}

#' @export
#' @rdname chmp_lists
chmp_lists_ <- function(fields = NULL, exclude_fields = NULL, 
    count = NULL, offset = NULL, before_date_created = NULL,
    since_date_created = NULL, before_campaign_last_sent = NULL, 
    since_campaign_last_sent = NULL, email = NULL, sort_field = NULL, 
    sort_dir = NULL, key = NULL, ...) {

  assert_is(key, 'character')
  args <- ct(list(fields = fields, exclude_fields = exclude_fields, 
    count = count, offset = offset, before_date_created = before_date_created,
    since_date_created = since_date_created, 
    before_campaign_last_sent = before_campaign_last_sent, 
    since_campaign_last_sent = since_campaign_last_sent, 
    email = email, sort_field = sort_field, sort_dir = sort_dir))
  chmp_GET("lists", key, query = args, ...)
}
