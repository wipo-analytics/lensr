#tests for lens searches
context("lens_search function")

test_that("inventor and query combination works", {
  lsa <- lens_search(inventor = "donald", query = "duck", timer = 5)
  expect_is(lsa, "data.frame")
})

test_that("applicant and query combination works", {
  lsb <- lens_search(applicant = "Synthetic Genomics", query = "synthetic genomics", timer = 5)
  expect_is(lsb, "data.frame") # look at url and output here
})

test_that("query alone works", {
  lsc <- lens_search(query = "duck", timer = 5)
  expect_is(lsc, "data.frame")
})

# test_that("query applicant inventor works with tac and families", {
#   lsc <- lens_search(synbio, boolean = "OR", type = "tac", inventor = "Venter Craig", applicant = "Synthetic Genomics", families = TRUE, timer = 5)
#   expect_is(lsc, "data.frame")
# })
#
# "https://www.lens.org/lens/search?q=inventor%3A%22Venter+Craig%22~2+%26%26+applicant%3A%28%22Synthetic+Genomics%22%29+%26%26+%28title%3A%28%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%7C%7C+%22synthetic+genome%22+%7C%7C+%22synthetic+genomes%22+%7C%7C+%22biological+parts%22+%7C%7C+%22genetic+circuit%22+%7C%7C+%22genetic+circuits%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%7C%7C+%22synthetic+genome%22+%7C%7C+%22synthetic+genomes%22+%7C%7C+%22biological+parts%22+%7C%7C+%22genetic+circuit%22+%7C%7C+%22genetic+circuits%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%7C%7C+%22synthetic+genome%22+%7C%7C+%22synthetic+genomes%22+%7C%7C+%22biological+parts%22+%7C%7C+%22genetic+circuit%22+%7C%7C+%22genetic+circuits%22%29%29&n=50&f=true"




# test_that("boolean OR works", {
#   lsb <- lens_search(synbio, boolean = "OR")
#   expect_identical(lsb, "https://www.lens.org/lens/search?q=%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%7C%7C+%22synthetic+genome%22+%7C%7C+%22synthetic+genomes%22+%7C%7C+%22biological+parts%22+%7C%7C+%22genetic+circuit%22+%7C%7C+%22genetic+circuits%22&n=50&f=true")
# })
#
# test_that("boolean AND works", {
#   lsc <- lens_search(synbio, boolean = "AND")
#   expect_identical(lsc, "https://www.lens.org/lens/search?q=%22synthetic+biology%22+%26%26+%22synthetic+genomics%22+%26%26+%22synthetic+genome%22+%26%26+%22synthetic+genomes%22+%26%26+%22biological+parts%22+%26%26+%22genetic+circuit%22+%26%26+%22genetic+circuits%22&n=50&f=true")
# })
#
# test_that("title search works", {
#   lsd <- lens_search("synthetic biology", type = "title")
#   expect_identical(lsd, "https://www.lens.org/lens/search?q=title%3A%28%22synthetic+biology%22%29&n=50&f=true")
# })
#
# test_that("abstract search works", {
#   lse <- lens_search("synthetic biology", type = "abstract")
#   expect_identical(lse, "https://www.lens.org/lens/search?q=abstract%3A%28%22synthetic+biology%22%29&n=50&f=true")
# })
#
# test_that("claims search works", {
#   lsf <- lens_search("synthetic biology", type = "claims")
#   expect_identical(lsf, "https://www.lens.org/lens/search?q=claims%3A%28%22synthetic+biology%22%29&n=50&f=true")
# })
#
# test_that("tac search works", {
#   lsg <- lens_search(synbio, boolean = "OR", type = "tac")
#   expect_identical(lsg, "https://www.lens.org/lens/search?q=%28title%3A%28%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%7C%7C+%22synthetic+genome%22+%7C%7C+%22synthetic+genomes%22+%7C%7C+%22biological+parts%22+%7C%7C+%22genetic+circuit%22+%7C%7C+%22genetic+circuits%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%7C%7C+%22synthetic+genome%22+%7C%7C+%22synthetic+genomes%22+%7C%7C+%22biological+parts%22+%7C%7C+%22genetic+circuit%22+%7C%7C+%22genetic+circuits%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%7C%7C+%22synthetic+genome%22+%7C%7C+%22synthetic+genomes%22+%7C%7C+%22biological+parts%22+%7C%7C+%22genetic+circuit%22+%7C%7C+%22genetic+circuits%22%29%29&n=50&f=true")
# })
#
# test_that("rank citing works", {
#   lsh <- lens_search("synthetic biology", type = "claims", rank_citing = TRUE)
#   expect_identical(lsh, "https://www.lens.org/lens/search?q=claims%3A%28%22synthetic+biology%22%29&n=50&s=citing_pub_key_count&d=-&f=true")
# })
#
# test_that("rank families works", {
#   lsi <- lens_search("synthetic biology", type = "tac", rank_family = TRUE)
#   expect_identical(lsi, "https://www.lens.org/lens/search?q=%28title%3A%28%22synthetic+biology%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22%29%29&n=50&s=simple_family_size&d=-&f=true")
# })
#
# test_that("rank sequences works", {
#   lsj <- lens_search("synthetic biology", type = "tac", rank_sequences = TRUE)
#   expect_identical(lsj, "https://www.lens.org/lens/search?q=%28title%3A%28%22synthetic+biology%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22%29%29&n=50&s=sequence_count&d=-&f=true")
# })
#
# test_that("rank latest publication works", {
#   lsk <- lens_search("synthetic biology", type = "tac", latest_publication = TRUE)
#   expect_identical(lsk, "https://www.lens.org/lens/search?q=%28title%3A%28%22synthetic+biology%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22%29%29&n=50&s=pub_date&d=-&f=true")
# })
#
# test_that("rank earliest publication works", {
#   lsl <- lens_search("synthetic biology", type = "tac", earliest_publication = TRUE)
#   expect_identical(lsl, "https://www.lens.org/lens/search?q=%28title%3A%28%22synthetic+biology%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22%29%29&n=50&s=pub_date&d=%2B&f=true")
# })
#
# test_that("rank earliest filing works", {
#   lsm <- lens_search("synthetic biology", type = "tac", earliest_filing = TRUE)
#   expect_identical(lsm, "https://www.lens.org/lens/search?q=%28title%3A%28%22synthetic+biology%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22%29%29&n=50&s=filing_date&d=%2B&f=true")
# })
#
# test_that("rank latest filing works", {
#   lsn <- lens_search("synthetic biology", type = "tac", latest_filing = TRUE)
#   expect_identical(lsn, "https://www.lens.org/lens/search?q=%28title%3A%28%22synthetic+biology%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22%29%29&n=50&s=filing_date&d=-&f=true")
# })
#
# test_that("families false works", {
#   lso <- lens_search("synthetic biology", type = "tac", families = FALSE)
#   expect_identical(lso, "https://www.lens.org/lens/search?q=%28title%3A%28%22synthetic+biology%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22%29%29&n=50")
# })
#
# #If function shifts to lens_urls then this test should move elsewhere to keep the tests focused.
# test_that("retrieve 50 results works", {
#   lsp <- lens_search(synbio, boolean = "OR", type = "tac", rank_citing = TRUE, results = 50) %>% lens_parse()
#   expect_is(lsp, "data.frame")
#   expect_length(lsp, 8) # 8 columns. Note that this may change
# })

# test_that("retrieve 500 results works", {
#   lsq <- lens_search(synbio, boolean = "OR", type = "tac", rank_citing = TRUE, results = 500) %>% lens_iterate()
#   expect_is(lsq, "data.frame")
#   expect_length(lsq, 8) # 8 columns. Note that this may change
# }) # needs a short sequence for testing as will take a long time.