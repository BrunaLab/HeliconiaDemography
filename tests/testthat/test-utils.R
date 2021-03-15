library(here)
source(here("packages.R"))
source(here("R", "utils.R"))

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

#extremely simple test that just checks for errors, not correctness.
test_that("annotate_spei() doesn't error", {
  df <- tibble(x = 1:20, y = runif(20, -2.5, 2.5))
  p <- ggplot(df, aes(x, y)) + geom_line()  
  expect_s3_class(annotate_spei(p), "ggplot")
})