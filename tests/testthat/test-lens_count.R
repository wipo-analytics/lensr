context("lens_count function")
#note to change to data.frame or tibble rather than a list. At present only passes on list and should be tibble.
test_that("count families TRUE works", {
  f <- lens_count("drones", families = TRUE)
  expect_type(f, "list")
  expect_length(f, 3)
  expect_lt(f$families, f$publications) #expect that families count is less than publication count
}
  )

test_that("count families FALSE works", {
  t <- lens_count("drones", families = FALSE)
  expect_type(t, "list")
  expect_length(t, 3)
  expect_equal(t$publications, t$families) # expect that families and publications are equal. Note that is publication results at present.
}
)