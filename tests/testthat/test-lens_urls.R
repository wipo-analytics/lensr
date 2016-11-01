#tests for lens_urls
context("lens_urls function")

test_that("results argument works correctly", {
  lua <- lens_urls("synthetic biology", families = TRUE, results = 20)
  expect_identical(lua, "https://www.lens.org/lens/search?q=%22synthetic+biology%22&n=20&f=true")
})

test_that("NULL in results returns 50", {
  lub <- lens_urls("synthetic biology", type = "tac", families = TRUE)
  expect_identical(lub, "https://www.lens.org/lens/search?q=%28title%3A%28%22synthetic+biology%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22%29%29&n=50&f=true")
})

test_that("pub_date_start and end work",{
  luc <- lens_urls("synthetic biology", pub_date_start = 19900101, pub_date_end = 20150101, families = TRUE)
  expect_identical(luc, "https://www.lens.org/lens/search?q=%22synthetic+biology%22&dates=%2Bpub_date%3A19900101-20150101&n=50&f=true")
})

test_that("filing_date_start and end work",{
  lud <- lens_urls("synthetic biology", filing_date_start = 19900101, filing_date_end = 20150101, families = TRUE)
  expect_identical(lud, "https://www.lens.org/lens/search?q=%22synthetic+biology%22&dates=%2Bfiling_date%3A19900101-20150101&n=50&f=true")
})

# test_that("short pub year to date conversion works", {
#   lue <- lens_urls("synthetic biology", pub_date_start = 1990, pub_date_end = 2015)
#   expect_identical(lue, "https://www.lens.org/lens/search?q=%22synthetic+biology%22&dates=%2Bpub_date%3A19900101-20151231&n=50&f=true")
#
# })