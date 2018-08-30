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
  skip_if(getRversion() < "3.5.0", "Old R versions have different eval() backtraces")

  dir <- normalizePath(test_path(".."))
  e <- environment()

  f <- function() { g() }
  g <- function() { tryCatch(h(), foo = identity, bar = identity) }
  h <- function() { tryCatch(i(), baz = identity) }
  i <- function() { tryCatch(trace_back(e)) }
  trace <- eval(quote(f()))

  expect_known_output(file = test_path("test-trace-collapsed1.txt"), {
    cat("Full:\n")
    print(trace, simplify = "none", dir = dir)
    cat("\nCollapsed:\n")
    print(trace, simplify = "collapse", dir = dir)
    cat("\nTrail:\n")
    print(trace, simplify = "trail", dir = dir)
  })

  # With multiple siblings
  f <- function() eval(quote(eval(quote(g()))))
  g <- function() tryCatch(eval(quote(h())), foo = identity, bar = identity)
  h <- function() trace_back(e)
  trace <- eval(quote(f()))

  expect_known_output(file = test_path("test-trace-collapsed2.txt"), {
    cat("Full:\n")
    print(trace, simplify = "none", dir = dir)
    cat("\nCollapsed:\n")
    print(trace, simplify = "collapse", dir = dir)
    cat("\nTrail:\n")
    print(trace, simplify = "trail", dir = dir)
  })
})

test_that("trace_simplify_trail() extracts last branch", {
  e <- environment()
  j <- function(i) k(i)
  k <- function(i) l(i)
  l <- function(i) eval(quote(m()), parent.frame(i))
  m <- function() trace_back(e)

  x1 <- j(1)
  expect_length(x1, 6)
  expect_length(trace_simplify_trail(x1), 3)

  x2 <- j(2)
  expect_length(x2, 6)
  expect_length(trace_simplify_trail(x2), 2)

  x3 <- j(3)
  expect_length(x2, 6)
  expect_length(trace_simplify_trail(x3), 1)
})

test_that("integerish indices are allowed", {
  trace <- trace_back()
  expect_identical(trace[0], trace[0L])
})

test_that("cli_branch() handles edge case", {
  e <- environment()
  f <- function() trace_back(e)
  trace <- f()

  call <- paste0(" ", cli_style$h, "f()")
  tree <- trace_as_tree(trace, srcrefs = FALSE)
  expect_identical(cli_branch(tree$call[-1]), call)
})

test_that("trace formatting picks up `rlang_trace_format_srcrefs`", {
  e <- environment()
  f <- function() trace_back(e)
  trace <- f()

  with_options(
    rlang_trace_format_srcrefs = FALSE,
    expect_false(any(grepl("testthat", format(trace))))
  )
  with_options(
    rlang_trace_format_srcrefs = TRUE,
    expect_true(any(!!grepl("test-trace\\.R", format(trace))))
  )
})

test_that("trace picks up option `rlang_trace_top_env` for trimming trace", {
  e <- current_env()
  f1 <- function() trace_back()
  f2 <- function() trace_back(e)
  with_options(rlang_trace_top_env = current_env(),
    expect_identical(length(f1()), length(f2()))
  )
})

test_that("collapsed formatting doesn't collapse single frame siblings", {
  e <- current_env()
  f <- function() eval_bare(quote(g()))
  g <- function() trace_back(e)
  trace <- f()

  full <- capture.output(print(trace, simplify = "none", srcrefs = FALSE))[[3]]
  full <- substr(full, 5, nchar(full))

  collapsed <- capture.output(print(trace, simplify = "collapse", srcrefs = FALSE))[[3]]
  collapsed <- substr(collapsed, 5, nchar(collapsed))

  expect_identical(full, "eval_bare(quote(g()))")
  expect_identical(collapsed, "[ eval_bare(...) ]")
})

test_that("recursive frames are rewired to the global env", {
  skip_on_os("windows")

  dir <- normalizePath(test_path(".."))
  e <- environment()

  f <- function() g()
  g <- function() trace_back(e)
  trace <- eval_tidy(quo(f()))

  expect_known_output(file = test_path("test-trace-recursive.txt"), {
    cat("Full:\n")
    print(trace, simplify = "none", dir = dir, srcrefs = FALSE)
    cat("\nCollapsed:\n")
    print(trace, simplify = "collapse", dir = dir, srcrefs = FALSE)
    cat("\nTrail:\n")
    print(trace, simplify = "trail", dir = dir, srcrefs = FALSE)
  })
})

test_that("long backtrails are truncated", {
  skip_on_os("windows")

  e <- current_env()
  f <- function(n) {
    if (n) {
      return(f(n - 1))
    }
    trace_back(e)
  }
  trace <- f(10)

  expect_known_output(file = test_path("test-trace-truncate-trail.txt"), {
    cat("Full:\n")
    print(trace, simplify = "trail", srcrefs = FALSE)
    cat("\n5 frames:\n")
    print(trace, simplify = "trail", max_frames = 5, srcrefs = FALSE)
    cat("\n2 frames:\n")
    print(trace, simplify = "trail", max_frames = 2, srcrefs = FALSE)
    cat("\n1 frame:\n")
    print(trace, simplify = "trail", max_frames = 1, srcrefs = FALSE)
  })

  expect_error(print(trace, simplify = "none", max_frames = 5), "currently only supported with")
  expect_error(print(trace, simplify = "collapse", max_frames = 5), "currently only supported with")
})
