context("cnd-message")

test_that("format_bullets() formats bullets depending on names", {
  scoped_options(
    crayon.enabled = FALSE,
    cli.unicode = FALSE
  )
  expect_identical(format_bullets(c("foo", "bar")), "* foo\n* bar")
  expect_identical(format_bullets(c(i = "foo", "baz", x = "bar")), "i foo\n* baz\nx bar")
  expect_error(format_bullets(c(i = "foo", u = "bar")))
  expect_identical(format_bullets(chr()), chr())
})

test_that("default conditionMessage() method for rlang errors calls cnd_message()", {
  # Fallback
  out <- conditionMessage(error_cnd("rlang_foobar", message = "embedded"))
  expect_identical(out, "embedded")

  # Only `cnd_issue()` method
  out <- with_bindings(
    .env = global_env(),
    cnd_issue.rlang_foobar = function(cnd, ...) "dispatched!",
    conditionMessage(error_cnd("rlang_foobar", message = "embedded"))
  )
  expect_identical(out, "dispatched!")

  # Both `cnd_issue()` and `cnd_bullets()` methods
  out <- with_bindings(
    .env = global_env(),
    cnd_issue.rlang_foobar = function(cnd, ...) "dispatched!",
    cnd_bullets.rlang_foobar = function(cnd, ...) c("one", x = "two", i = "three"),
    conditionMessage(error_cnd("rlang_foobar", message = "embedded"))
  )
  exp <- paste0("dispatched!\n", format_bullets(c("one", x = "two", i = "three")))
  expect_identical(out, exp)
})

test_that("default cnd_bullets() method calls lazy method if present", {
  err <- error_cnd(
    "rlang_foobar",
    message = "Issue.",
    data = "foo",
    cnd_bullets = function(cnd, ...) {
      c(x = cnd$data, i = "bar")
    }
  )
  err_formula_bullets <- error_cnd(
    "rlang_foobar",
    message = "Issue.",
    data = "foo",
    cnd_bullets = ~ .$data
  )

  # Should not have precedence
  local_methods(
    cnd_bullets.rlang_foobar = function(cnd, ...) "wrong!"
  )

  # Needs bugfix in dev version
  skip_if_not_installed("testthat", "2.2.1.9000")

  verify_output(test_path("test-cnd-bullets-lazy.txt"), {
    cnd_signal(err)
    cnd_signal(err_formula_bullets)
  })
})
