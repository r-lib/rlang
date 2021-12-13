test_that("try_call() catches or declines values", {
  f <- function() g()
  g <- function() h()
  h <- function() abort("foo")

  expect_error(try_call(f(), warning = function(cnd) NULL), "foo")
  expect_error(try_call(f(), error = function(cnd) zap()), "foo")
  expect_null(try_call(f(), error = function(cnd) NULL))

  fns <- list(error = function(cnd) NULL)
  expect_null(try_call(f(), !!!fns))
})

test_that("try_call() checks inputs", {
  expect_snapshot({
    (expect_error(try_call(NULL, function(...) NULL)))
  })
  expect_true(try_call(TRUE))
})

test_that("can rethrow from `try_call()`", {
  local_options(
    rlang_trace_top_env = current_env(),
    rlang_trace_format_srcrefs = FALSE
  )
  f <- function() g()
  g <- function() h()
  h <- function() abort("foo")

  expect_snapshot({
    err <- catch_error(
      try_call(f(), error = function(cnd) abort("bar", parent = cnd))
    )

    print(err)
    print(err, simplify = "none")
  })
})

test_that("with_handlers() establishes inplace and exiting handlers", {
  handlers <- list(
    error = function(c) "caught error",
    message = function(c) "caught message",
    warning = calling(function(c) "warning"),
    foobar = calling(function(c) cat("foobar"))
  )

  expect_equal(with_handlers(identity(letters), !!!handlers), identity(letters))
  expect_equal(with_handlers(stop(letters), !!!handlers), "caught error")
  expect_equal(with_handlers(message(letters), !!!handlers), "caught message")
  expect_warning(expect_equal(with_handlers({ warning("warn!"); letters }, !!!handlers), identity(letters)), "warn!")
  expect_output(expect_equal(with_handlers({ signal("", "foobar"); letters }, !!!handlers), identity(letters)), "foobar")
})

test_that("bare functions are treated as exiting handlers", {
  expect_identical(with_handlers(abort("foo"), error = function(cnd) "caught"), "caught")
})

test_that("with_handlers() supports formula shortcut for lambdas", {
  err <- with_handlers(abort("foo", "bar"), error = ~.x)
  expect_true(inherits(err, "bar"))
})

test_that("can muffle conditions", {
  expect_no_message(
    expect_identical(with_handlers({ message(""); "foo" }, message = calling(cnd_muffle)), "foo")
  )
  expect_no_warning(
    expect_identical(with_handlers({ warning(""); "foo" }, warning = calling(cnd_muffle)), "foo")
  )
  cnd_expect_muffle <- calling(function(cnd) {
    expect_s3_class(findRestart("rlang_muffle"), "restart")
    cnd_muffle(cnd)
  })
  expect_identical(with_handlers({ signal("", "cnd"); "foo" }, cnd = cnd_expect_muffle), "foo")
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

test_that("with_handlers() registers calling handlers first (#718)", {
  out <- with_restarts(
    RESTART = ~ "good",
    with_handlers(
      CND = calling(~ rst_jump("RESTART")),
      CND = ~ "bad",
      signal("", "CND")
    )
  )
  expect_identical(out, "good")

  out <- with_restarts(
    RESTART = ~ "good",
    with_handlers(
      CND = ~ "bad",
      CND = calling(~ rst_jump("RESTART")),
      signal("", "CND")
    )
  )
  expect_identical(out, "good")
})

test_that("cnd_muffle() returns FALSE if the condition is not mufflable", {
  value <- NULL
  expect_error(withCallingHandlers(
    stop("foo"),
    error = function(cnd) value <<- cnd_muffle(cnd)
  ))
  expect_false(value)
})

test_that("with_handlers() propagates visibility", {
  expect_visible(with_handlers(list(invisible(1))))
  expect_invisible(with_handlers(invisible(1)))
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
  try_call(
    overflow(),
    error = function(cnd) handled <<- TRUE
  )
  expect_true(handled)

  handled <- FALSE
  try_call(
    overflow(),
    warning = identity,
    error = function(cnd) handled <<- TRUE
  )
  expect_true(handled)

  handled <- NULL
  try_call(
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
