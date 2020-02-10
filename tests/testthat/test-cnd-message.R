context("cnd-message")

test_that("format_error_bullets() formats bullets depending on names", {
  local_options(
    crayon.enabled = FALSE,
    cli.unicode = FALSE
  )
  expect_identical(format_error_bullets(c("foo", "bar")), "* foo\n* bar")
  expect_identical(format_error_bullets(c(i = "foo", "baz", x = "bar")), "i foo\n* baz\nx bar")
  expect_error(format_error_bullets(c(i = "foo", u = "bar")))
  expect_identical(format_error_bullets(chr()), chr())
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

test_that("can override body method with `body` fields", {
  local_methods(cnd_body.rlang_foobar = function(...) "wrong")

  expect_error(
    stop(error_cnd("rlang_foobar", message = "header", body = "body")),
    "header\nbody",
    class = "rlang_foobar"
  )
  expect_error(
    stop(error_cnd("rlang_foobar", message = "header", body = ~ "body")),
    "header\nbody",
    class = "rlang_foobar"
  )
  expect_error(
    stop(error_cnd("rlang_foobar", message = "header", body = function(...) "body")),
    "header\nbody",
    class = "rlang_foobar"
  )

  expect_error(
    stop(error_cnd("rlang_foobar", message = "header", body = ~ format_error_bullets("body"))),
    "header\n* body",
    fixed = TRUE,
    class = "rlang_foobar"
  )
})

test_that("`body` must be a string or a function", {
  expect_error(
    stop(error_cnd("foo", body = letters)),
    "must be a string or a function"
  )
})
