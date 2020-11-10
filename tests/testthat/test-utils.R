# library(here)
# source(here("R", "utils.R"))
library(dplyr)
test_that("matrix iteration works", {
  A <- matrix(1:9, nrow = 3)
  expect_equal(A %*% A, A %^% 2)
  expect_equal(A %*% A %*% A, A %^% 3)
})

test_that("eu_dist() works", {
  x = c(0,0)
  y = c(3,3)
  expect_equal(eu_dist(x, y), sqrt(3^2 + 3^2))
})

test_that("nearest() helper works", {
  x <- seq(1, 10, 0.3)
  expect_equal(which(nearest(x, 5)), 14)
  expect_equal(x[nearest(x, 5)], 4.9)
  expect_s3_class(tibble(x) %>% filter(nearest(x, 5)), "tbl_df")
})