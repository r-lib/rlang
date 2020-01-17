context("cnd-signal")

test_that("cnd_signal() creates muffle restarts", {
  withCallingHandlers(cnd_signal(cnd("foo")),
    foo = function(c) {
      expect_true(rst_exists("rlang_muffle"))
    }
  )
})

test_that("signallers support character vectors as `message` parameter", {
  expect_message(inform(c("foo", "bar")), "foo\n* bar", fixed = TRUE)
  expect_warning(warn(c("foo", "bar")), "foo\n* bar", fixed = TRUE)
  expect_error(abort(c("foo", "bar")), "foo\n* bar", fixed = TRUE)
  expect_condition(signal(c("foo", "bar"), "quux"), "quux", regex = "foo\n\\* bar")
})

test_that("cnd_signal() and signal() returns NULL invisibly", {
  expect_identical(withVisible(cnd_signal(cnd("foo"))), withVisible(invisible(NULL)))
  expect_identical(withVisible(signal("", "foo")), withVisible(invisible(NULL)))
})

test_that("signal() accepts character vectors of classes (#195)", {
  expect <- calling(function(cnd) {
    expect_identical(class(cnd), c("foo", "bar", "condition"))
  })
  with_handlers(signal("", c("foo", "bar")), foo = expect)
})

test_that("can pass condition metadata", {
  msg <- catch_cnd(inform("type", foo = "bar"))
  expect_identical(msg$foo, "bar")

  wng <- catch_cnd(warn("type", foo = "bar"))
  expect_identical(wng$foo, "bar")

  err <- catch_cnd(abort("type", foo = "bar"))
  expect_identical(err$foo, "bar")
})

test_that("can signal and catch interrupts", {
  expect_is(catch_cnd(interrupt()), "interrupt")
})

test_that("can signal interrupts with cnd_signal()", {
  intr <- catch_cnd(interrupt())
  with_handlers(cnd_signal(intr),
    condition = function(cnd) expect_is(cnd, "interrupt")
  )
})

test_that("conditions have correct subclasses", {
  expect_true(inherits_all(catch_cnd(signal("", "foo")), c("foo", "condition", "condition")))
  expect_true(inherits_all(catch_cnd(inform("", "foo")), c("foo", "message", "condition")))
  expect_true(inherits_all(catch_cnd(warn("", "foo")), c("foo", "warning", "condition")))
  expect_true(inherits_all(catch_cnd(abort("", "foo")), c("foo", "rlang_error", "error", "condition")))
})

test_that("cnd_signal() creates a backtrace if needed", {
  skip_unless_utf8()

  local_options(
    rlang_trace_top_env = current_env(),
    rlang_trace_format_srcrefs = FALSE
  )

  err <- error_cnd("rlang_error_foobar", trace = NULL)
  f <- function() g()
  g <- function() h()
  h <- function() cnd_signal(err)

  err <- catch_cnd(f())
  expect_known_output(file = test_path("test-cnd-signal-trace.txt"), print(err))
})

test_that("`inform()` appends newline to message", {
  expect_identical(
    catch_cnd(inform("foo"))$message,
    "foo\n"
  )
})

test_that("condition signallers can be called without arguments", {
  # For pragmatic reasons we don't require a class because we now use
  # `inform()` in places where `cat()` would be more appropriate
  expect_message(inform(), "", fixed = TRUE)
  expect_warning(warn(class = "foo"), "", fixed = TRUE)
  expect_error(abort(class = "foo"), "", fixed = TRUE, class = "foo")
})

test_that("`inform()` returns invisibly", {
  expect_invisible(inform("foo"))
})


# Lifecycle ----------------------------------------------------------

test_that("deprecated arguments of cnd_signal() still work", {
  local_lifecycle_silence()

  observed <- catch_cnd(cnd_signal("foo"))
  expected <- catch_cnd(signal("", "foo"))
  expect_identical(observed, expected)

  with_handlers(cnd_signal(cnd("foo"), .mufflable = TRUE),
    foo = calling(function(cnd) expect_true(rst_exists("rlang_muffle")))
  )
})

test_that("can still use `.subclass`", {
  expect_error(abort("foo", .subclass = "baz"), class = "baz")
  expect_warning(warn("foo", .subclass = "baz"), class = "baz")
  expect_message(inform("foo", .subclass = "baz"), class = "baz")
  expect_is(cnd(.subclass = "baz"), "baz")
  expect_is(error_cnd(.subclass = "baz"), "baz")
  expect_is(warning_cnd(.subclass = "baz"), "baz")
  expect_is(message_cnd(.subclass = "baz"), "baz")
})
