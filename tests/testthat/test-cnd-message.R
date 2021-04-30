test_that("format_bullets() formats bullets depending on names", {
  expect_identical(format_bullets(c("foo", "bar")), "* foo\n* bar")
  expect_identical(format_bullets(c(i = "foo", "*" = "baz", x = "bar", v = "bam")), "i foo\n* baz\nx bar\nv bam")
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
    stop(error_cnd("rlang_foobar", message = "header", body = ~ format_bullets("body"))),
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

test_that("can request a line break in error bullets (#1130)", {
  expect_snapshot({
    (expect_error(abort(c(
      "Main header.",
      "Header 1",
      x = "Bullet 1",
      x = "Bullet 2",
      "Header 2",
      x = "Bullet 3",
      x = "Bullet 4"
    ))))

    (expect_error(abort(c(
      "Main header.",
      "Header 1",
      "x" = "Bullet 1",
      " " = "Break line",
      "x" = "Bullet 2",
      "",
      "Header 2",
      "x" = "Bullet 3",
      " " = "Break line",
      "x" = "Bullet 4"
    ))))
  })
})

test_that("fully unnamed bullet vectors are treated as bullets", {
  expect_equal(
    format_bullets("foo"),
    "* foo"
  )
  expect_equal(
    format_bullets(c("foo", "bar")),
    "* foo\n* bar"
  )

  non_bullets <- set_names(c("foo", "bar"), c("", ""))
  expect_equal(
    format_bullets(non_bullets),
    "foo\nbar"
  )
})

test_that("empty names in partially named bullet vectors are treated as line breaks", {
  expect_equal(
    format_bullets(c("foo", i = "bar", "baz")),
    "foo\ni bar\nbaz"
  )
  expect_equal(
    format_bullets(c(i = "bar", "baz")),
    "i bar\nbaz"
  )
})

test_that("! and > symbols create warning and alert bullets", {
  expect_equal(
    format_bullets(c("Header", "!" = "Attention", ">" = "Alert")),
    "Header\n! Attention\n> Alert"
  )
})

test_that("cli is not used when message is escaped with `I()`", {
  .rlang_use_cli_format <- TRUE
  x <- "foo"

  expect_equal(
    conditionMessage(expect_error(abort("{x}"))),
    "foo"
  )

  expect_equal(
    conditionMessage(expect_error(abort(I("{x}")))),
    "{x}"
  )
})
