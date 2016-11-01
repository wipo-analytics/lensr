#tests for lens iterate
context("lens_iterate function")

test_that("single url works", {
  lita <- lens_iterate(one_url)
  expect_is(lita, "data.frame")
})

test_that("three urls works", {
  litb <- lens_iterate(three_urls)
  expect_is(litb, "data.frame")
})