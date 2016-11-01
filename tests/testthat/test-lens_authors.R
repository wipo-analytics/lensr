#tests for lens_authors
context("lens_authors function")

test_that("single author works correctly", {
  laa <- lens_authors("Venter Craig")
  expect_identical(laa, "https://www.lens.org/lens/search?q=author%3A%28Venter+Craig%29")
})

test_that("OR multiple authors works correctly", {
  lab <- lens_authors(auth, author_boolean = "OR")
  expect_identical(lab, "https://www.lens.org/lens/search?q=author%3A%28Venter+Craig%29+%7C%7C+author%3A%28Smith+Hamilton%29")
})

test_that("AND multiple authors works correctly", {
  lac <- lens_authors(auth, author_boolean = "AND")
  expect_identical(lac, "https://www.lens.org/lens/search?q=author%3A%28Venter+Craig%29+%26%26+author%3A%28Smith+Hamilton%29")
})

test_that("three author names format correctly with OR", {
  lad <- lens_authors(three_authors, author_boolean = "OR")
  expect_identical(lad, "https://www.lens.org/lens/search?q=author%3A%28Venter+Craig%29+%7C%7C+author%3A%28Hamilton+Smith%29+%7C%7C+author%3A%28Beeblebrox+Zaphod%29")
})

# test_that("single author type works", {
#   lae <- lens_authors("Kirk James", author_type = "crossref")
#   expect_identical(lad, "https://www.lens.org/lens/search?q=author%3A%28Venter+Craig%29+%7C%7C+author%3A%28Hamilton+Smith%29+%7C%7C+author%3A%28Beeblebrox+Zaphod%29")
# })