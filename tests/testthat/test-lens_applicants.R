#tests for lens_applicants
context("lens_applicants function")

test_that("single applicant works correctly",{
  laa <- lens_applicants("Synthetic Genomics")
  expect_identical(laa, "https://www.lens.org/lens/search?q=applicant%3A%28%22Synthetic+Genomics%22%29")
})

test_that("multiple applicants on AND works correctly", {
  lab <- lens_applicants(c("Synthetic Genomics", "Venter Craig"), applicant_boolean = "AND")
  expect_identical(lab, "https://www.lens.org/lens/search?q=applicant%3A%28%22Synthetic+Genomics%22%29+%26%26+applicant%3A%28%22Venter+Craig%22%29")
})

test_that("multiple applicants on OR works correctly", {
  lac <- lens_applicants(c("Synthetic Genomics", "Venter Craig"), applicant_boolean = "OR")
  expect_identical(lac, "https://www.lens.org/lens/search?q=applicant%3A%28%22Synthetic+Genomics%22%29+%7C%7C+applicant%3A%28%22Venter+Craig%22%29")
})

test_that("three applicants on OR works correctly", {
  lad <- lens_applicants(three_applicants, applicant_boolean = "OR")
  expect_identical(lad, "https://www.lens.org/lens/search?q=applicant%3A%28%22Synthetic+Genomics%22%29+%7C%7C+applicant%3A%28%22Venter+Craig%22%29+%7C%7C+applicant%3A%28%22Gibson+Daniel%22%29")
})

test_that("three applicants on AND works correctly", {
  lae <- lens_applicants(three_applicants, applicant_boolean = "AND")
  expect_identical(lae, "https://www.lens.org/lens/search?q=applicant%3A%28%22Synthetic+Genomics%22%29+%26%26+applicant%3A%28%22Venter+Craig%22%29+%26%26+applicant%3A%28%22Gibson+Daniel%22%29")
})