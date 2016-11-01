#tests for parsing lens results
context("lens_parse function")
library(dplyr)

test_that("returns df with 50 rows", {
  lpd <- lens_urls(synbio, boolean = "OR") %>% lens_parse()
  expect_is(lpd, "data.frame")
  #expectnrows to add
})

test_that("blank applicant cases are working", {
  lsb <- lens_urls("synthetic biology", type = "tac", families = FALSE) %>% lens_parse()
  expect_is(lsb, "data.frame")
  #stringr::str_detect(lsb$applicants, "-") # could add some kind of true false test for the marker here.
})

test_that("full text with no family works", {
  ls <- lens_urls("drones", families = FALSE) %>% lens_parse()
  expect_is(ls, "data.frame")
  #stringr::str_detect(lsb$applicants, "-") # could add some kind of true false test for the marker here.
})
