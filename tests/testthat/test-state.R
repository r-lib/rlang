context("state")

test_that("can't add an exit event at top-level", {
  expect_error(scoped_exit(1, global_env()), "Can't add an exit event at top-level")
})

test_that("options are set temporarily", {
  scoped_options(foo = "foo")
  expect_identical(with_options(foo = "bar", peek_option("foo")), "bar")
  expect_identical(peek_option("foo"), "foo")
})

test_that("peek_options() returns a named list", {
  scoped_options(foo = "FOO", bar = "BAR")
  expect_identical(peek_options("foo", "bar"), list(foo = "FOO", bar = "BAR"))
})
