context("trace.R")

# These tests must come first because print method includes srcrefs
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
  trace <- i()

  expect_known_output(file = test_path("test-trace-print.txt"), {
    print(trace, dir = dir)
    cat("\n")
    print(trace[0L], dir = dir)
  })
})

test_that("can print tree with collapsed branches", {
  skip_on_os("windows")

  dir <- normalizePath(test_path(".."))
  e <- environment()

  f <- function() { g() }
  g <- function() { tryCatch(h(), foo = identity, bar = identity) }
  h <- function() { tryCatch(i(), baz = identity) }
  i <- function() { tryCatch(trace_back(e)) }
  trace <- eval(quote(f()))

  expect_known_output(file = test_path("test-trace-collapsed.txt"), {
    cat("Full:\n")
    print(trace, siblings = "full", dir = dir)
    cat("\nCollapsed:\n")
    print(trace, siblings = "collapsed", dir = dir)
  })
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
