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

test_that("as_living() util works", {
  alive <- c(1,2,NA,3,5,NA)
  dead <- c(1,2,NA,3,NA,NA)
  expect_equal(as_living(alive), rep(1, 6))
  expect_equal(as_living(dead), c(1,1,1,1,0,0))
})

test_that("as_living() can handle short numbers of observations", {
  alive <- c(1,2)
  nope <- c(NA, NA)
  expect_type(as_living(alive, n = 3), "integer")
  expect_equal(as_living(nope), c(NA_integer_, NA_integer_))
})