#' Get lists
#'
#' @export
#' @param conn a connection object. see [ChmpClient]
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
#' (conn <- ChmpClient$new())
#' chmp_lists_(conn)
#' chmp_lists(conn)
#' chmp_lists(conn, parse = FALSE)
#' }
chmp_lists <- function(conn, fields = NULL, exclude_fields = NULL,
    count = NULL, offset = NULL, before_date_created = NULL,
    since_date_created = NULL, before_campaign_last_sent = NULL,
    since_campaign_last_sent = NULL, email = NULL, sort_field = NULL,
    sort_dir = NULL, key = NULL, parse = TRUE, ...) {

  assert_is(parse, "logical")
  chmp_parse(chmp_lists_(conn, fields, exclude_fields, count, offset,
    before_date_created, since_date_created, before_campaign_last_sent,
    since_campaign_last_sent, email, sort_field, sort_dir, key,
    args, ...), parse)
}

#' @export
#' @rdname chmp_lists
chmp_lists_ <- function(conn, fields = NULL, exclude_fields = NULL,
    count = NULL, offset = NULL, before_date_created = NULL,
    since_date_created = NULL, before_campaign_last_sent = NULL,
    since_campaign_last_sent = NULL, email = NULL, sort_field = NULL,
    sort_dir = NULL, key = NULL, ...) {

  assert_is(key, "character")
  args <- ct(list(fields = fields, exclude_fields = exclude_fields,
    count = count, offset = offset, before_date_created = before_date_created,
    since_date_created = since_date_created,
    before_campaign_last_sent = before_campaign_last_sent,
    since_campaign_last_sent = since_campaign_last_sent,
    email = email, sort_field = sort_field, sort_dir = sort_dir))
  chmp_GET(conn$dc, "lists", conn$key %||% key, query = args, ...)
}

#' Add Members to a List
#'
#' @param conn a connection object. see [ChmpClient]
#' @param list_id **Required:** The unique ID for the list
#' @param email_address **Required:** Email address for a subscriber.
#' @param status **Required:** Subscriber's current status. Possible values:
#'   "subscribed", "unsubscribed", "cleaned", "pending", or "transactional".
#' @param skip_merge_validation If skip_merge_validation is true, member data
#'   will be accepted without merge field values, even if the merge field is
#'   usually required. The Mailchimp API defaults this to FALSE, but chimpr
#'   authors prefer a more permissive default of TRUE.
#' @param email_type Type of email this member asked to get ('html' or 'text').
#' @param merge_fields A dictionary of merge fields [audience
#'   fields](https://mailchimp.com/help/getting-started-with-merge-tags/) where
#'   the keys are the merge tags. For example, {"FNAME":"Freddie"}
#' @param interests The key of this object's properties is the ID of the
#'   interest in question.
#' @param language If set/detected, the [subscriber's
#'   language](https://mailchimp.com/help/view-and-edit-contact-languages/)
#' @param vip [VIP
#'   status](https://mailchimp.com/help/view-and-edit-contact-languages/) for
#'   subscriber.
#' @param location Subscriber location information. See API for details.
#' @param marketing_permissions The marketing permissions for the subscriber.
#'   See API for details.
#' @param ip_signup IP address the subscriber signed up from.
#' @param timestamp_signup The date and time the subscriber signed up for the
#'   list in ISO 8601 format.
#' @param ip_opt The IP address the subscriber used to confirm their opt-in
#'   status.
#' @param timestamp_opt The date and time the subcscriber confirmed their opt-in
#'   status in ISO 8601 format.
#' @param tags The tags that are associated with a member
#' @param invisibly Logical: `TRUE` by default. If TRUE, the HTTP request
#'   will be returned invisibly; `FALSE` and it will be returned explicitly.
#'
#' @template all
#' @export
#'
#' @examples \dontrun{
#' (conn <- ChmpClient$new())
#'
#' chmp_post_list(conn,
#'                list_id = "your-list-id",
#'                email_address = "person@website.com",
#'                status = "subscribed")
#'
#' }
#'
chmp_post_list <- function(conn, list_id = NULL, email_address = NULL, status = NULL,
                           skip_merge_validation = TRUE, email_type = NULL, merge_fields = NULL,
                           interests = NULL, language = NULL, vip = NULL, location = NULL,
                           marketing_permissions = NULL, ip_signup = NULL, timestamp_signup = NULL,
                           ip_opt = NULL, timestamp_opt = NULL, tags = NULL, key = NULL,
                           parse = TRUE, invisibly = TRUE, ...) {

  if (!all(vapply(list(list_id, email_address, status), is.character, logical(1)))) {
    stop("`list_id`, `email_address`, and `status` are required arguments.")
  }

  out <- chmp_parse(chmp_post_list_(conn, list_id = list_id, email_address = email_address, status = status,
                                    skip_merge_validation = skip_merge_validation, email_type = email_type,
                                    merge_fields = merge_fields, interests = interests, language = language,
                                    vip = vip, location = location, marketing_permissions = marketing_permissions,
                                    ip_signup = ip_signup, timestamp_signup = timestamp_signup, ip_opt = ip_opt,
                                    timestamp_opt = timestamp_opt, tags = tags, key = key,
                                    ...), parse)

  if (invisibly) invisible(out) else return(out)
}


#' @export
#' @rdname chmp_post_list
chmp_post_list_ <- function(conn, list_id = NULL, email_address = NULL, status = NULL,
                           skip_merge_validation = TRUE, email_type = NULL, merge_fields = NULL,
                           interests = NULL, language = NULL, vip = NULL, location = NULL,
                           marketing_permissions = NULL, ip_signup = NULL, timestamp_signup = NULL,
                           ip_opt = NULL, timestamp_opt = NULL, tags = NULL, key = NULL, ...) {

  query_args <- ct(list(skip_merge_validation = skip_merge_validation))

  body_args <- ct(list(email_address = email_address, status = status, email_type = email_type,
                       merge_fields = merge_fields, interests = interests, language = language,
                       vip = vip, location = location, marketing_permissions = marketing_permissions,
                       ip_signup = ip_signup, timestamp_signup = timestamp_signup,
                       ip_opt = ip_opt, timestamp_opt = timestamp_opt, tags = tags)
                  )

  chmp_POST(dc = conn$dc,
            path = file.path("lists", list_id, "members"),
            key = conn$key %||% key,
            query = query_args,
            body = body_args,
            ...)

}


