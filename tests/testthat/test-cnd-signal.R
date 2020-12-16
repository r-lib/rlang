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
  expect_s3_class(catch_cnd(interrupt()), "interrupt")
})

test_that("can signal interrupts with cnd_signal()", {
  intr <- catch_cnd(interrupt())
  with_handlers(cnd_signal(intr),
    condition = function(cnd) expect_s3_class(cnd, "interrupt")
  )
})

test_that("conditions have correct subclasses", {
  expect_true(inherits_all(catch_cnd(signal("", "foo")), c("foo", "condition", "condition")))
  expect_true(inherits_all(catch_cnd(inform("", "foo")), c("foo", "message", "condition")))
  expect_true(inherits_all(catch_cnd(warn("", "foo")), c("foo", "warning", "condition")))
  expect_true(inherits_all(catch_cnd(abort("", "foo")), c("foo", "rlang_error", "error", "condition")))
})

test_that("cnd_signal() creates a backtrace if needed", {
  local_options(
    rlang_trace_top_env = current_env(),
    rlang_trace_format_srcrefs = FALSE
  )

  err <- error_cnd("rlang_error_foobar", trace = NULL)
  f <- function() g()
  g <- function() h()
  h <- function() cnd_signal(err)

  err <- catch_cnd(f())
  expect_snapshot(print(err))
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

test_that("warn() respects frequency", {
  local_options(`rlang:::message_always` = FALSE)

  expect_warning(
    warn("foo", .frequency = "always", .frequency_id = "warn_always"),
    "^foo$"
  )
  expect_warning(
    warn("foo", .frequency = "always", .frequency_id = "warn_always"),
    "^foo$"
  )

  expect_warning(
    warn("foo", .frequency = "once", .frequency_id = "warn_once"),
    "^foo\n.*warning is displayed once per session"
  )
  expect_no_warning(
    warn("foo", .frequency = "once", .frequency_id = "warn_once")
  )

  expect_warning(
    warn("foo", .frequency = "regularly", .frequency_id = "warn_regularly"),
    "foo\n.*warning is displayed once every 8 hours"
  )
  expect_no_warning(
    warn("foo", .frequency = "regularly", .frequency_id = "warn_regularly")
  )
})

test_that("inform() respects frequency", {
  local_options(`rlang:::message_always` = FALSE)

  expect_message(
    inform("foo", .frequency = "always", .frequency_id = "inform_always"),
    "^foo\n$"
  )
  expect_message(
    inform("foo", .frequency = "always", .frequency_id = "inform_always"),
    "^foo\n$"
  )

  expect_message(
    inform("foo", .frequency = "once", .frequency_id = "inform_once"),
    "^foo\n.*message is displayed once per session"
  )
  expect_no_message(
    inform("foo", .frequency = "once", .frequency_id = "inform_once")
  )

  expect_message(
    inform("foo", .frequency = "regularly", .frequency_id = "inform_regularly"),
    "foo\n.*message is displayed once every 8 hours"
  )
  expect_no_message(
    inform("foo", .frequency = "regularly", .frequency_id = "inform_regularly")
  )
})

test_that("warn() and inform() use different periodicity environments", {
  local_options(`rlang:::message_always` = FALSE)

  expect_message(
    inform("foo", .frequency = "once", .frequency_id = "warn_inform_different_envs"),
    "foo"
  )
  expect_warning(
    warn("foo", .frequency = "once", .frequency_id = "warn_inform_different_envs"),
    "foo"
  )
})

test_that("periodic messages can be forced", {
  local_options(`rlang:::message_always` = TRUE)
  expect_warning(
    warn("foo", .frequency = "once", .frequency_id = "warn_forced"),
    "foo"
  )
  expect_warning(
    warn("foo", .frequency = "once", .frequency_id = "warn_forced"),
    "foo"
  )
})

test_that("`.frequency_id` is mandatory", {
  expect_error(warn("foo", .frequency = "once"), "frequency_id")
})

test_that("cnd_signal() is a no-op with `NULL`", {
  expect_null(catch_cnd(cnd_signal(NULL)))
})

test_that("`inform()` behaves consistently in interactive and non-interactive sessions (#1037)", {
  # Default behaviour
  out1 <- Rscript(shQuote(c("--vanilla", "-e", "rlang::inform('foo')")))
  out2 <- Rscript(shQuote(c("--vanilla", "-e", "rlang::with_interactive(rlang::inform('foo'))")))
  expect_equal(out1$out, "foo")
  expect_equal(out1$out, out2$out)

  # Sinked behaviour
  out1 <- Rscript(shQuote(c("--vanilla", "-e", "capture.output(rlang::inform('foo'))")))
  out2 <- Rscript(shQuote(c("--vanilla", "-e", "rlang::with_interactive(capture.output(rlang::inform('foo')))")))
  expect_equal(out1$out, c("foo", "character(0)"))
  expect_equal(out1$out, out2$out)
})

test_that("`inform()` and `warn()` with recurrent footer handle newlines correctly", {
  expect_snapshot({
    inform("foo", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    inform("bar", .frequency = "regularly", .frequency_id = as.character(runif(1)))

    warn("foo", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    warn("bar", .frequency = "regularly", .frequency_id = as.character(runif(1)))
  })
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

test_that("error_cnd() still accepts `.subclass`", {
  expect_equal(
    error_cnd(.subclass = "foo"),
    error_cnd("foo")
  )
})
