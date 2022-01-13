local_unexport_signal_abort()

test_that("try_fetch() catches or declines values", {
  f <- function() g()
  g <- function() h()
  h <- function() abort("foo")

  expect_error(try_fetch(f(), warning = function(cnd) NULL), "foo")
  expect_error(try_fetch(f(), error = function(cnd) zap()), "foo")
  expect_null(try_fetch(f(), error = function(cnd) NULL))

  fns <- list(error = function(cnd) NULL)
  expect_null(try_fetch(f(), !!!fns))
})

test_that("try_fetch() checks inputs", {
  expect_snapshot({
    (expect_error(try_fetch(NULL, function(...) NULL)))
  })
  expect_true(try_fetch(TRUE))
})

test_that("can rethrow from `try_fetch()`", {
  local_options(
    rlang_trace_top_env = current_env(),
    rlang_trace_format_srcrefs = FALSE
  )
  f <- function() g()
  g <- function() h()
  h <- function() abort("foo")

  high1 <- function(...) high2(...)
  high2 <- function(...) high3(...)
  high3 <- function(..., chain) {
    if (chain) {
      try_fetch(f(), error = function(cnd) abort("bar", parent = cnd))
    } else {
      try_fetch(f(), error = function(cnd) abort("bar", error = cnd))
    }
  }

  expect_snapshot({
    err <- catch_error(
      try_fetch(f(), error = function(cnd) abort("bar", parent = cnd))
    )
    print(err)
    print(err, simplify = "none")

    err <- catch_error(high1(chain = TRUE))
    print(err)
    print(err, simplify = "none")

    err <- catch_error(high1(chain = FALSE))
    print(err)
    print(err, simplify = "none")
  })
})

test_that("can catch condition of specific classes", {
  expect_null(catch_cnd(signal("", "bar"), "foo"))
  expect_s3_class(catch_cnd(signal("", "bar"), "bar"), "bar")
  expect_s3_class(catch_cnd(stop(""), "error"), "error")

  expect_s3_class(catch_cnd(stop("tilt")), "error")
  expect_error(catch_cnd(stop("tilt"), "foo"), "tilt")

  classes <- c("foo", "bar")
  expect_s3_class(catch_cnd(signal("", "bar"), classes), "bar")
  expect_s3_class(catch_cnd(signal("", "foo"), classes), "foo")
})

test_that("cnd_muffle() returns FALSE if the condition is not mufflable", {
  value <- NULL
  expect_error(withCallingHandlers(
    stop("foo"),
    error = function(cnd) value <<- cnd_muffle(cnd)
  ))
  expect_false(value)
})

test_that("drop_global_handlers() works and is idempotent", {
  skip_if_not_installed("base", "4.0")

  code <- '{
    library(testthat)

    globalCallingHandlers(NULL)

    handler <- function(...) "foo"
    globalCallingHandlers(foo = handler)

    rlang:::drop_global_handlers(bar = handler)
    expect_equal(globalCallingHandlers(), list(foo = handler))

    rlang:::drop_global_handlers(foo = handler, bar = function() "bar")
    expect_equal(globalCallingHandlers(), list())

    rlang:::drop_global_handlers(foo = handler, bar = function() "bar")
    expect_equal(globalCallingHandlers(), list())
  }'

  out <- Rscript(shQuote(c("--vanilla", "-e", code)))
  expect_equal(out$out, chr())
})

test_that("stackOverflowError are caught", {
  overflow <- function() signal("", "stackOverflowError")

  handled <- FALSE
  try_fetch(
    overflow(),
    error = function(cnd) handled <<- TRUE
  )
  expect_true(handled)

  handled <- FALSE
  try_fetch(
    overflow(),
    warning = identity,
    error = function(cnd) handled <<- TRUE
  )
  expect_true(handled)

  handled <- NULL
  try_fetch(
    overflow(),
    error = function(cnd) {
      handled <<- c(handled, 1)
      cnd_signal(cnd)
    },
    warning = identity,
    error = function(cnd) handled <<- c(handled, 2)
  )
  expect_equal(handled, c(1, 2))
})
