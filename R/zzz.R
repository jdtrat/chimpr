ct <- function(l) Filter(Negate(is.null), l)

chimpr_ua <- function() {
  versions <- c(
    paste0("r-curl/", utils::packageVersion("curl")),
    paste0("crul/", utils::packageVersion("crul")),
    sprintf("rOpenSci(chimpr/%s)", utils::packageVersion("chimpr"))
  )
  paste0(versions, collapse = " ")
}

ch_GET <- function(path, key, query = list(), ...){
  cli <- crul::HttpClient$new(
    url = ch_base(),
    opts = c(list(useragent = chimpr_ua()), ...),
    auth = crul::auth(user = "anystring", pwd = check_key(key))
  )
  temp <- cli$get(
    path = file.path("3.0", path), 
    query = query)
  temp$raise_for_status()
  x <- temp$parse("UTF-8")
  err_catcher(x)
  return(x)
}

err_catcher <- function(x) {
  xx <- jsonlite::fromJSON(x)
  if (any(vapply(c("message", "error"), function(z) z %in% names(xx),
                 logical(1)))) {
    stop(xx[[1]], call. = FALSE)
  }
}

ch_parse <- function(x, parse) {
  jsonlite::fromJSON(x, parse)
}

check_key <- function(x){
  tmp <- if (is.null(x)) Sys.getenv("MAILCHIMP_KEY", "") else x
  if (tmp == "") {
    stop("need an API key for the Mailchimp API", call. = FALSE)
  } else {
    tmp
  }
}

ch_base <- function() "https://us7.api.mailchimp.com"

space <- function(x) gsub("\\s", "%20", x)

assert_is <- function(x, y) {
  if (!is.null(x)) {
    if (!class(x) %in% y) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

assert_n <- function(x, n) {
  if (!is.null(x)) {
    if (!length(x) == n) {
      stop(deparse(substitute(x)), " must be length ", n, call. = FALSE)
    }
  }
}
