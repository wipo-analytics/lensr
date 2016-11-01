#tests for lens_inventors
context("lens_inventors function")

test_that("single inventor works correctly", {
  lia <- lens_inventor("Venter Craig")
  expect_identical(lia, "https://www.lens.org/lens/search?q=inventor%3A%22Venter+Craig%22~2")
})

test_that("two inventors works with NULL boolean with OR as default", {
  lib <- lens_inventor(auth)
  expect_identical(lib, "https://www.lens.org/lens/search?q=inventor%3A%22Venter+Craig%22~2+%7C%7C+inventor%3A%22Smith+Hamilton%22~2")
})

test_that("two inventor works correctly with OR specified", {
  lic <- lens_inventor(auth, inventor_boolean = "OR")
  expect_identical(lic, "https://www.lens.org/lens/search?q=inventor%3A%22Venter+Craig%22~2+%7C%7C+inventor%3A%22Smith+Hamilton%22~2")
})

test_that("two inventor works correctly with AND specified", {
  lid <- lens_inventor(auth, inventor_boolean = "AND")
  expect_identical(lid, "https://www.lens.org/lens/search?q=inventor%3A%22Venter+Craig%22~2+%26%26+inventor%3A%22Smith+Hamilton%22~2")
})
