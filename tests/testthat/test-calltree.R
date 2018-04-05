context("test-calltree.R")

# Trimming ----------------------------------------------------------------

test_that("trace_simplify() extracts last branch", {
  e <- environment()
  j <- function(i) k(i)
  k <- function(i) l(i)
  l <- function(i) eval(quote(calltrace(e)), parent.frame(i))

  x1 <- j(1)
  expect_length(x1, 6)
  expect_length(trace_simplify(x1), 3)

  x2 <- j(2)
  expect_length(x2, 6)
  expect_length(trace_simplify(x2), 2)

  x3 <- j(3)
  expect_length(x2, 6)
  expect_length(trace_simplify(x3), 1)
})
