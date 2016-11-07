#tests for lens_ipcs
context("lens_ipcs function")

test_that("single ipc works correctly", {
  lipa <- lens_ipcs("A61K31/00")
  expect_identical(lipa, "https://www.lens.org/lens/search?q=classification_ipcr%3AA61K31%5C%2F00")
})

test_that("multiple ipcs work on AND", {
  lipb <- lens_ipcs(c("A61K31/00", "C12N15/82"), ipc_boolean = "AND")
  expect_identical(lipb, "https://www.lens.org/lens/search?q=classification_ipcr%3AA61K31%5C%2F00+%26%26+classification_ipcr%3AC12N15%5C%2F82")
})

test_that("multiple ipcs work on AND", {
  lipc <- lens_ipcs(c("A61K31/00", "C12N15/82"), ipc_boolean = "OR")
  expect_identical(lipc, "https://www.lens.org/lens/search?q=classification_ipcr%3AA61K31%5C%2F00+%7C%7C+classification_ipcr%3AC12N15%5C%2F82")
})

test_that("multiple ipcs work on subclass OR", {
  lipb <- lens_ipcs(c("A61K", "C12N"), ipc_boolean = "OR")
  expect_identical(lipb, "https://www.lens.org/lens/search?q=classification_ipcr%3AA61K+%7C%7C+classification_ipcr%3AC12N")
})

#Add test for error message on absence of forward slash below

