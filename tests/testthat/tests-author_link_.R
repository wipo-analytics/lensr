#tests for author_link_
context("author_link_ function")

test_that("multi author default works correctly", {
  al <- author_link_(auth, author_boolean = "OR", author_type = "default" )
  expect_identical(al, "https://www.lens.org/lens/search?q=author%3A%28Venter Craig%29+%7C%7C+author%3A%28Smith Hamilton%29")
})

test_that("multi author crossref works correctly", {
  ala <- author_link_(auth, author_boolean = "OR", author_type = "crossref" )
  expect_identical(ala, "https://www.lens.org/lens/search?q=crossref_author%3A%28Venter Craig%29+%7C%7C+crossref_author%3A%28Smith Hamilton%29")
})

test_that("multi author pubmed works correctly", {
  alb <- author_link_(auth, author_boolean = "OR", author_type = "pubmed" )
  expect_identical(alb, "https://www.lens.org/lens/search?q=pubmed_author%3A%28Venter Craig%29+%7C%7C+pubmed_author%3A%28Smith Hamilton%29")
})

test_that("multi author default AND works correctly", {
  alc <- author_link_(auth, author_boolean = "AND", author_type = "default" )
  expect_identical(alc, "https://www.lens.org/lens/search?q=author%3A%28Venter Craig%29+%26%26+author%3A%28Smith Hamilton%29")
})

test_that("multi author crossref AND works correctly", {
  ald <- author_link_(auth, author_boolean = "AND", author_type = "crossref" )
  expect_identical(ald, "https://www.lens.org/lens/search?q=crossref_author%3A%28Venter Craig%29+%26%26+crossref_author%3A%28Smith Hamilton%29")
})

test_that("multi author pubmed AND works correctly", {
  ale <- author_link_(auth, author_boolean = "AND", author_type = "pubmed" )
  expect_identical(ale, "https://www.lens.org/lens/search?q=pubmed_author%3A%28Venter Craig%29+%26%26+pubmed_author%3A%28Smith Hamilton%29")
})