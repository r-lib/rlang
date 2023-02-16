test_that("error if dots not used", {
  f <- function(x, y, ...) {
    check_dots_used()
    x + y
  }

  expect_error(f(1, 2), NA)
  expect_error(f(1, 2, 3), class = "rlib_error_dots_unused")
})

test_that("error if dots not used by another function", {
  g <- function(a = 1, b = 1, ...) {
    a + b
  }
  f <- function(x = 1, ...) {
    check_dots_used()
    x * g(...)
  }

  expect_error(f(x = 10, a = 1), NA)

  expect_snapshot({
    (expect_error(f(x = 10, c = 3), class = "rlib_error_dots_unused"))
  })
})

test_that("error if dots named", {
  f <- function(..., xyz = 1) {
    check_dots_unnamed()
  }

  expect_null(f(1))

  expect_error(f(xyz = 1), NA)
  expect_error(f(1, 2, 3), NA)
  expect_error(f(1, 2, 3, xyz = 4), NA)
  expect_error(f(1, 2, 3, xy = 4), class = "rlib_error_dots_named")

  expect_snapshot({
    (expect_error(f(1, 2, 3, xy = 4, x = 5), class = "rlib_error_dots_named"))
  })
})

test_that("error if if dots not empty", {
  f <- function(..., xyz = 1) {
    check_dots_empty()
  }
  f0 <- function(..., xyz = 1) {
    check_dots_empty0(...)
  }

  expect_error(f(xyz = 1), NA)
  expect_error(f0(xyz = 1), NA)

  expect_snapshot({
    (expect_error(f(xy = 4), class = "rlib_error_dots_nonempty"))
    (expect_error(f0(xy = 4), class = "rlib_error_dots_nonempty"))
  })
})

test_that("can control the action (deprecated)", {
  f <- function(action, check, ..., xyz = 1) {
    check(action = action)
  }

  expect_error(f(abort, check_dots_used, xy = 4), class = "rlib_error_dots_unused")
  expect_warning(f(warn, check_dots_used, xy = 4), class = "rlib_error_dots_unused")
  expect_message(f(inform, check_dots_used, xy = 4), class = "rlib_error_dots_unused")

  expect_error(f(abort, check_dots_unnamed, xy = 4), class = "rlib_error_dots_named")
  expect_warning(f(warn, check_dots_unnamed, xy = 4), class = "rlib_error_dots_named")
  expect_message(f(inform, check_dots_unnamed, xy = 4), class = "rlib_error_dots_named")

  expect_error(f(abort, check_dots_empty, xy = 4), class = "rlib_error_dots_nonempty")
  expect_warning(f(warn, check_dots_empty, xy = 4), class = "rlib_error_dots_nonempty")
  expect_message(f(inform, check_dots_empty, xy = 4), class = "rlib_error_dots_nonempty")
})

test_that("warn if unused dots", {
  safe_median <- function(x, ...) {
    check_dots_used()
    UseMethod("safe_median")
  }
  safe_median.numeric <- function(x, ..., na.rm = TRUE) {
    stats::median(x, na.rm = na.rm)
  }
  expect_error(safe_median(1:10), NA)
  expect_error(safe_median(1:10, na.rm = TRUE), NA)
  expect_error(safe_median(1:10, y = 1), class = "rlib_error_dots_unused")
})

test_that("can supply `error` handler", {
  hnd <- function(cnd) warning(cnd)

  f <- function(...) check_dots_empty(error = hnd)
  expect_silent(f())
  expect_warning(f(foo), class = "rlib_error_dots_nonempty")

  f <- function(...) check_dots_used(error = hnd)
  expect_silent(f())
  expect_warning(f(foo), class = "rlib_error_dots_unused")


  f <- function(...) check_dots_unnamed(error = hnd)
  expect_silent(f(foo))
  expect_warning(f(foo = foo), class = "rlib_error_dots_named")
})

test_that("expression contents are mentioned", {
  f <- function(...) check_dots_empty()

  expect_snapshot(error = TRUE, {
    f("foo")
    f(foo)
    inject(f(!!letters))
    f(a = { 1; 2 })
    f(a = toupper(letters))
  })
})

test_that("empty dots error mentions info bullets if any unnamed element", {
  f <- function(...) check_dots_empty()
  expect_snapshot(error = TRUE, {
    f(1)
    f(a = 1)
    f(a = 1, 2)
  })
})

test_that("check_dots_empty() allows trailing missing arg (#1390)", {
  fn <- function(..., a = NULL) check_dots_empty()

  expect_null(fn(a = 1, ))

  expect_snapshot({
    (expect_error(fn(a = 1, b = )))
  })
})
