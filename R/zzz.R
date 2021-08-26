ct <- function(l) Filter(Negate(is.null), l)

chimpr_ua <- function() {
  versions <- c(
    paste0("r-curl/", utils::packageVersion("curl")),
    paste0("crul/", utils::packageVersion("crul")),
    sprintf("chimpr/%s", utils::packageVersion("chimpr"))
  )
  paste0(versions, collapse = " ")
}

chmp_GET <- function(dc = "us7", path, key, query = list(), ...){
  cli <- crul::HttpClient$new(
    url = chmp_base(dc),
    opts = c(list(useragent = chimpr_ua()), ...),
    auth = crul::auth(user = "anystring", pwd = key)
    # auth = crul::auth(user = "anystring", pwd = check_key(key))
  )
  temp <- cli$get(
    path = file.path("3.0", path),
    query = query)
  err_catcher(temp)
  x <- temp$parse("UTF-8")
  return(x)
}

chmp_POST <- function(dc = "us7", path, key, query = list(), body = NULL,
                      disk = NULL, stream = NULL, encode = "json", ...) {

  cli <- crul::HttpClient$new(
    url = chmp_base(dc),
    opts = c(list(useragent = chimpr_ua()), ...),
    auth = crul::auth(user = "anystring", pwd = key)
    # auth = crul::auth(user = "anystring", pwd = check_key(key))
  )

  temp <- cli$post(
    path = file.path("3.0", path),
    query = query,
    body = body,
    disk = disk,
    stream = stream,
    encode = encode)

  err_catcher(temp)
  x <- temp$parse("UTF-8")
  return(x)
}


err_catcher <- function(x) {
  if (x$status_code > 201) {
    if (x$response_headers$`content-type` ==
        "application/problem+json; charset=utf-8") {

      xx <- jsonlite::fromJSON(x$parse("UTF-8"))
      xx <- paste0("\n  ", paste(names(xx), unname(xx), sep = ": ",
                                 collapse = "\n  "))
      stop(xx, call. = FALSE)
    } else {
      x$raise_for_status()
    }
  }
}

chmp_parse <- function(x, parse) {
  tmp <- jsonlite::fromJSON(x, parse)
  if ("_links" %in% names(tmp)) {
    names(tmp)[names(tmp) %in% "_links"] <- "links"
  }
  return (tmp)
}

check_key <- function(x){
  tmp <- if (is.null(x)) Sys.getenv("MAILCHIMP_KEY", "") else x
  if (tmp == "") {
    stop("need an API key for the Mailchimp API", call. = FALSE)
  } else {
    tmp
  }
}

chmp_base <- function(x="us7") sprintf("https://%s.api.mailchimp.com", x)

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

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
