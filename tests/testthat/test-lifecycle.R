context("lifecycle")

test_that("can disable lifecycle warnings", {
  scoped_lifecycle_silence()
  scoped_options(
    lifecycle_verbose_soft_deprecation = TRUE,
    lifecycle_repeat_warnings = TRUE
  )

  expect_no_warning(signal_soft_deprecated("foo"))
  expect_no_warning(warn_deprecated("foo"))
})

test_that("can promote lifecycle warnings to errors", {
  scoped_lifecycle_errors()
  expect_error(signal_soft_deprecated("foo"), "foo")
  expect_error(warn_deprecated("foo"), "foo")
})

test_that("can enable warnings and errors with `with_` helpers", {
  expect_warning(with_lifecycle_warnings(signal_soft_deprecated("foo")), "foo")
  expect_error(with_lifecycle_errors(signal_soft_deprecated("foo")), "foo")
  expect_no_warning(with_lifecycle_warnings(with_lifecycle_silence(warn_deprecated("foo"))))
})

test_that("soft-deprecation warnings are issued when called from child of global env as well", {
  fn <- function() signal_soft_deprecated("called from child of global env")
  expect_warning(eval_bare(call2(fn), env(global_env())), "child of global env")
})

test_that("once-per-session note is not displayed on repeated warnings", {
  wrn <- catch_cnd(warn_deprecated("foo", "once-per-session-note"))
  expect_true(grepl("once per session", wrn$message))

  scoped_options(lifecycle_repeat_warnings = TRUE)

  wrn <- catch_cnd(warn_deprecated("foo", "once-per-session-no-note"))
  expect_false(grepl("once per session", wrn$message))
})

test_that("inputs are type checked", {
  expect_error(signal_soft_deprecated(1), "is_string")
  expect_error(signal_soft_deprecated("foo", 1), "is_string")
  expect_error(signal_soft_deprecated("foo", "bar", 1), "is_environment")
  expect_error(warn_deprecated(1), "is_string")
  expect_error(warn_deprecated("foo", 1), "is_string")
  expect_error(stop_defunct(1), "is_string")
})

test_that("the topenv of the empty env is not the global env", {
  expect_silent(signal_soft_deprecated("topenv empty env", env = empty_env()))
})
