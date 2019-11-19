context("cnd-message")

test_that("format_bullets() formats bullets depending on names", {
  local_options(
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

  # Only `cnd_header()` method
  out <- with_methods(
    cnd_header.rlang_foobar = function(cnd, ...) "dispatched!",
    conditionMessage(error_cnd("rlang_foobar", message = "embedded"))
  )
  expect_identical(out, "dispatched!")

  # Both `cnd_header()` and `cnd_body()` methods
  out <- with_methods(
    cnd_header.rlang_foobar = function(cnd, ...) "dispatched!",
    cnd_body.rlang_foobar = function(cnd, ...) c("one", "two", "three"),
    conditionMessage(error_cnd("rlang_foobar", message = "embedded"))
  )
  exp <- paste0("dispatched!\n", paste_line(c("one", "two", "three")))
  expect_identical(out, exp)

  # All three methods defined
  out <- with_methods(
    cnd_header.rlang_foobar = function(cnd, ...) "dispatched!",
    cnd_body.rlang_foobar = function(cnd, ...) c("one", "two", "three"),
    cnd_footer.rlang_foobar = function(cnd, ...) c("foo", "bar"),
    conditionMessage(error_cnd("rlang_foobar", message = "embedded"))
  )
  exp <- paste0(exp, "\nfoo\nbar")
  expect_identical(out, exp)
})

test_that("default cnd_body() method calls OO bullets() method if present", {
  err <- error_cnd(
    "rlang_foobar",
    message = "Issue.",
    data = "foo",
    bullets = function(cnd, ...) {
      c(x = cnd$data, i = "bar")
    }
  )
  err_formula_bullets <- error_cnd(
    "rlang_foobar",
    message = "Issue.",
    data = "foo",
    bullets = ~ .$data
  )

  # Should not have precedence
  local_methods(
    cnd_body.rlang_foobar = function(cnd, ...) "wrong!"
  )

  verify_output(test_path("test-cnd-bullets-lazy.txt"), {
    cnd_signal(err)
    cnd_signal(err_formula_bullets)
  })
})

test_that("default cnd_body() method formats bullets if bullets field is TRUE", {
  err <- error_cnd(
    "rlang_foobar",
    message = "Issue.",
    data = "foo",
    bullets = TRUE
  )

  # Should not have precedence
  local_methods(
    cnd_body.rlang_foobar = function(cnd, ...) c("foo", i = "bar")
  )

  verify_output(test_path("test-cnd-bullets-true.txt"), {
    cnd_signal(err)
  })
})
