#' call the ping route
#' @export
#' @param key A Mailchimp API key. See
#' https://developer.mailchimp.com/documentation/mailchimp/
#' to get a key
#' @param ... Curl options passed to [crul::HttpClient]
#' @examples \dontrun{
#' chmp_ping()
#' }
chmp_ping <- function(key = NULL, ...) {
  assert_is(key, 'character')
  chmp_parse(chmp_GET("ping", key, query = list(), ...), TRUE)
}
