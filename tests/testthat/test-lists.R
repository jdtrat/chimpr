context("chmp_lists")

x <- ChmpClient$new()

test_that("chmp_lists with vcr", {
  skip_on_cran()
  skip_on_travis()

  vcr::use_cassette("chmp_lists", {
    aa <- chmp_lists(x)
  })

  expect_is(aa, "list")
  expect_named(aa, c('lists', 'total_items', 'constraints', 'links'))
  expect_is(aa$lists, "data.frame")
  expect_type(aa$total_items, "integer")
  expect_is(aa$links, "data.frame")

})

test_that("chmp_lists curl options work", {
  skip_on_cran()

  expect_error(chmp_lists(x, timeout_ms = 1), "Timeout was reached")
})

x5 <- ChmpClient$new(dc = "us5") # Current Location

test_that("chmp_post_lists with vcr", {
  skip_on_cran()
  skip_on_travis()

  rand_string <- function() {
    paste(sample(c(letters, 1:9), 9), collapse = "")
  }

  vcr::use_cassette("chmp_post_lists", {

    aa <- chmp_post_list(x5,
                         list_id = Sys.getenv("MAILCHIMP_LISTID"),
                         email_address = paste0(rand_string(), "@", rand_string(), ".com"),
                         status = "subscribed")

  })

    expect_is(aa, "list")
    expect_named(aa, c("id", "email_address", "unique_email_id",
                       "full_name", "web_id", "email_type", "status",
                       "consents_to_one_to_one_messaging", "merge_fields",
                       "stats", "ip_signup", "timestamp_signup", "ip_opt",
                       "timestamp_opt", "member_rating", "last_changed",
                       "language", "vip", "email_client", "location",
                       "source", "tags_count", "tags", "list_id", "links"))

    expect_is(aa$links, "data.frame")
    expect_is(aa$email_address, "character")
    expect_is(aa$id, "character")
    expect_is(aa$email_address, "character")
    expect_is(aa$status, "character")
    expect_is(aa$location, "list")

    })
