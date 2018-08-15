context("chmp_root")

x <- ChmpClient$new()

test_that("chmp_root with vcr", {
  skip_on_cran()
  skip_on_travis()

  vcr::use_cassette("chmp_root", {
    aa <- x$root()

    expect_is(aa, "list")
    expect_is(aa$'_links', 'data.frame')
    expect_equal(aa$account_name, "rOpenSci")
  })
})

test_that("chmp_root curl options work", {
  skip_on_cran()

  expect_error(x$root(timeout_ms = 1), "Timeout was reached")
})
