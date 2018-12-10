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
