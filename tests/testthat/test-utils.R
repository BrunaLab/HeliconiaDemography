test_that("matrix iteration works", {
  A <- matrix(1:9, nrow = 3)
  expect_equal(A %*% A, A %^% 2)
  expect_equal(A %*% A %*% A, A %^% 3)
})

test_that("rectangle masks are unique", {
  x <- mat_rect(D = 3, nrow = 10, ncol = 10)
  l <- vector()
  for(i in 1:length(x)) {
    for(j in 1:length(x)) {
      o <- all(x[[i]] == x[[j]]) & i!=j
      l <- c(l, o)
    }
  }
  expect_false(any(l))
}) 

test_that("diamond masks are unique", {
  x <- mat_diamond(D = 3, nrow = 10, ncol = 10)
  l <- vector()
  for(i in 1:length(x)) {
    for(j in 1:length(x)) {
      o <- all(x[[i]] == x[[j]]) & i!=j
      l <- c(l, o)
    }
  }
  expect_false(any(l))
}) 
