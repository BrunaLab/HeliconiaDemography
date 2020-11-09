library(here)
source(here("R", "utils.R"))
test_that("matrix iteration works", {
  A <- matrix(1:9, nrow = 3)
  expect_equal(A %*% A, A %^% 2)
  expect_equal(A %*% A %*% A, A %^% 3)
})


