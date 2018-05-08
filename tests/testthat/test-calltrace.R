context("calltrace.R")

# This test must come first because print method includes srcrefs
test_that("tree printing only changes deliberately", {
  skip_on_os("windows")

  dir <- normalizePath(test_path(".."))

  e <- environment()
  i <- function(i) j(i)
  j <- function(i) { k(i) }
  k <- function(i) {
    NULL
    l(i)
  }
  l <- function(i) trace_back(e)

  x <- i()

  expect_known_output({
    print(x, dir = dir)
    cat("\n")
    print(x[0L], dir = dir)
  }, test_path("calltrace-print.txt"))
})

test_that("trace_simplify() extracts last branch", {
  e <- environment()
  j <- function(i) k(i)
  k <- function(i) l(i)
  l <- function(i) eval(quote(m()), parent.frame(i))
  m <- function() trace_back(e)

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
