context("chmp_lists")

x <- ChmpClient$new()

test_that("chmp_lists with vcr", {
  skip_on_cran()
  skip_on_travis()

  vcr::use_cassette("chmp_lists", {
    aa <- chmp_lists(x)

    expect_is(aa, "list")
    expect_named(aa, c('lists', 'total_items', 'links'))
    expect_is(aa$lists, "data.frame")
    expect_type(aa$total_items, "integer")
    expect_is(aa$links, "data.frame")
  })
})

test_that("chmp_lists curl options work", {
  skip_on_cran()

  expect_error(chmp_lists(x, timeout_ms = 1), "Timeout was reached")
})
