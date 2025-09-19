test_that("format_error_bullets() formats bullets depending on names", {
  expect_equal(format_error_bullets(c("foo", "bar")), "* foo\n* bar")
  expect_equal(
    format_error_bullets(c(i = "foo", "*" = "baz", x = "bar", v = "bam")),
    "i foo\n* baz\nx bar\nv bam"
  )
  expect_equal(format_error_bullets(c(i = "foo", u = "bar")), "i foo\nbar")
  expect_equal(format_error_bullets(chr()), chr())
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

test_that("can override header, body, and footer methods with fields", {
  local_methods(cnd_body.rlang_foobar = function(...) "wrong")

  expect_error(
    stop(error_cnd(
      "rlang_foobar",
      message = "header",
      body = "body"
    )),
    "header\nbody",
    class = "rlang_foobar"
  )
  expect_error(
    stop(error_cnd(
      "rlang_foobar",
      header = "header",
      body = "body",
      footer = "footer"
    )),
    "header\nbody\nfooter",
    class = "rlang_foobar"
  )

  # Also messages and warnings
  expect_message(
    message(message_cnd(
      "rlang_foobar",
      header = ~"header",
      body = ~"body",
      footer = ~"footer"
    )),
    "header\nbody\nfooter",
    class = "rlang_foobar"
  )
  expect_warning(
    warning(warning_cnd(
      "rlang_foobar",
      header = function(...) "header",
      body = function(...) "body",
      footer = function(...) "footer"
    )),
    "header\nbody\nfooter",
    class = "rlang_foobar"
  )

  expect_error(
    stop(error_cnd(
      "rlang_foobar",
      message = "header",
      body = ~ format_error_bullets("body")
    )),
    "header\n* body",
    fixed = TRUE,
    class = "rlang_foobar"
  )
})

test_that("`body` must be a character vector or a function", {
  expect_snapshot({
    (expect_error(
      stop(error_cnd("foo", body = 1:3)),
      "must be"
    ))
  })
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
    format_error_bullets("foo"),
    "* foo"
  )
  expect_equal(
    format_error_bullets(c("foo", "bar")),
    "* foo\n* bar"
  )

  non_bullets <- set_names(c("foo", "bar"), c("", ""))
  expect_equal(
    format_error_bullets(non_bullets),
    "foo\nbar"
  )
})

test_that("empty names in partially named bullet vectors are treated as line breaks", {
  expect_equal(
    format_error_bullets(c("foo", i = "bar", "baz")),
    "foo\ni bar\nbaz"
  )
  expect_equal(
    format_error_bullets(c(i = "bar", "baz")),
    "i bar\nbaz"
  )
})

test_that("! and > symbols create warning and alert bullets", {
  expect_equal(
    format_error_bullets(c("Header", "!" = "Attention", ">" = "Alert")),
    "Header\n! Attention\n> Alert"
  )
})

test_that("cli is not used when message is escaped with `I()`", {
  local_use_cli(inline = TRUE)

  x <- "foo"

  expect_equal(
    conditionMessage(expect_error(abort("{x}"))),
    "foo"
  )

  return("no longer the case")

  expect_equal(
    conditionMessage(expect_error(abort(I("{x}")))),
    "{x}"
  )
})

test_that(".rlang_cli_str_restore() deals with attributes", {
  msg <- structure("foo", attr = TRUE)

  expect_equal(
    .rlang_cli_str_restore("bar", msg),
    structure("bar", attr = TRUE)
  )

  msg_oo <- structure("foo", attr = TRUE, class = "foo")
  expect_equal(
    .rlang_cli_str_restore("bar", msg_oo),
    "bar"
  )
})

skip_if_not_installed("cli", "2.5.0")
skip_if_not_installed("glue")

cli::test_that_cli("format_error_bullets() generates bullets", {
  expect_snapshot({
    format_error_bullets(c("Header.", i = "Bullet."))
  })
})

cli::test_that_cli(
  configs = c("plain", "fancy"),
  "can use cli syntax in `cnd_message()` methods",
  {
    local_methods(
      cnd_header.rlang_foobar = function(cnd, ...) {
        cli::format_inline("Header: {.emph {cnd$field}}")
      },
      cnd_body.rlang_foobar = function(cnd, ...) {
        c("i" = cli::format_inline("Bullet: {.emph {cnd$field}}"))
      },
      cnd_footer.rlang_foobar = function(cnd, ...) {
        c("_" = cli::format_inline("i" = "Footer: {.emph {cnd$field}}"))
      }
    )
    cnd <- error_cnd(
      "rlang_foobar",
      field = "User { {field}.",
      use_cli_format = TRUE
    )
    expect_snapshot(cnd_message(cnd))
  }
)

test_that("prefix takes call into account", {
  expect_snapshot({
    err <- error_cnd(message = "msg", call = quote(foo(bar = TRUE)))
    writeLines(cnd_message_format_prefixed(err))

    # Inlined objects disable context deparsing
    err1 <- error_cnd(message = "msg", call = expr(foo(bar = !!(1:3))))
    err2 <- error_cnd(message = "msg", call = quote(foo$bar()))
    err3 <- error_cnd(message = "msg", call = call2(identity))
    writeLines(cnd_message_format_prefixed(err1))
    writeLines(cnd_message_format_prefixed(err2))
    writeLines(cnd_message_format_prefixed(err3))
  })
})

test_that("long prefixes cause a line break", {
  very_very_very_very_very_long_function_name <- function() {
    abort("My somewhat longish and verbose error message.")
  }

  expect_snapshot((expect_error(very_very_very_very_very_long_function_name())))
})

test_that("prefixes include srcrefs", {
  withr::local_envvar("TESTTHAT" = "")

  eval_parse(
    "{
    f <- function() g()
    g <- function() abort('Foo.')
  }"
  )

  src_file <- g %@% srcref %@% srcfile
  src_file$filename <- "/foo/bar/baz/myfile.R"

  expect_snapshot((expect_error(f())))
})

test_that("inform() and warn() use fallback bullets formatting", {
  msg <- c("foo", i = "bar")

  expect_snapshot({
    local_use_cli(format = FALSE)
    warn(msg)
    warn(msg, .frequency = "once", .frequency_id = as.character(runif(1)))
  })

  expect_snapshot({
    local_use_cli(format = TRUE)
    warn(msg)
    warn(msg, .frequency = "once", .frequency_id = as.character(runif(1)))
  })

  expect_snapshot({
    local_use_cli(format = FALSE)
    inform(msg)
    inform(msg, .frequency = "once", .frequency_id = as.character(runif(1)))
  })

  expect_snapshot({
    local_use_cli(format = TRUE)
    inform(msg)
    inform(msg, .frequency = "once", .frequency_id = as.character(runif(1)))
  })
})

test_that("cnd_message() uses `body` and `footer` fields by default", {
  expect_equal(
    cnd_message(cnd("foo", message = "foo", footer = "baz")),
    "foo\nbaz"
  )
  expect_equal(
    cnd_message(cnd("foo", message = "foo", body = "bar", footer = "baz")),
    "foo\nbar\nbaz"
  )
})

test_that("can supply bullet without header", {
  expect_snapshot({
    (catch_cnd(inform(c(i = "foo")), "message"))
    (catch_cnd(warn(c(i = "foo")), "warning"))
  })
})

test_that("parent errors prints with bullets in all cases", {
  f <- function(use_cli = TRUE) {
    local_use_cli(format = use_cli)

    try_fetch(
      abort(c(
        "Header",
        i = "Bullet"
      )),
      error = function(cnd) {
        abort("Wrapper", parent = cnd)
      }
    )
  }

  expect_snapshot(error = TRUE, cnd_class = TRUE, {
    f(TRUE)
    f(FALSE)
  })
})

test_that("qualified calls are included in error prefix (#1315)", {
  expect_equal(
    error_call_as_string(quote(foo::bar())),
    "foo::bar()"
  )
})

test_that("special syntax calls handle edge cases", {
  expect_snapshot({
    error_call_as_string(quote(`+`()))
    error_call_as_string(quote(base::`+`(1, 2)))
  })
})

test_that("can print message with and without prefix", {
  expect_snapshot(cran = TRUE, {
    foo <- error_cnd(
      "foo",
      message = "Parent message.",
      body = c("*" = "Bullet 1.", "*" = "Bullet 2."),
      use_cli_format = TRUE
    )
    bar <- error_cnd(
      "bar",
      message = "Message.",
      body = c("*" = "Bullet A.", "*" = "Bullet B."),
      parent = foo,
      use_cli_format = TRUE
    )

    writeLines(cnd_message(foo, prefix = TRUE))
    writeLines(cnd_message(bar, prefix = TRUE))

    writeLines(cnd_message(foo, prefix = FALSE))
    writeLines(cnd_message(bar, prefix = FALSE))
  })
})

test_that("can print message without inheritance", {
  expect_snapshot(cran = TRUE, {
    foo <- error_cnd(
      "foo",
      message = "Parent message.",
      body = c("*" = "Bullet 1.", "*" = "Bullet 2."),
      use_cli_format = TRUE
    )
    bar <- error_cnd(
      "bar",
      message = "Message.",
      body = c("*" = "Bullet A.", "*" = "Bullet B."),
      parent = foo,
      use_cli_format = TRUE
    )

    writeLines(cnd_message(foo, inherit = FALSE, prefix = TRUE))
    writeLines(cnd_message(bar, inherit = FALSE, prefix = TRUE))

    writeLines(cnd_message(foo, inherit = FALSE, prefix = FALSE))
    writeLines(cnd_message(bar, inherit = FALSE, prefix = FALSE))
  })
})

test_that("ANSI escapes are supported in `conditionMessage()`", {
  skip_if_not_installed("cli")

  foo <- error_cnd(
    "foo",
    message = "Parent message.",
    use_cli_format = TRUE
  )
  bar <- error_cnd(
    "bar",
    message = "Message.",
    parent = foo,
    use_cli_format = TRUE
  )

  testthat::local_reproducible_output(
    unicode = FALSE,
    crayon = FALSE
  )
  out_bare <- conditionMessage(bar)

  testthat::local_reproducible_output(
    unicode = TRUE,
    crayon = TRUE
  )
  out_ansi <- conditionMessage(bar)

  expect_equal(out_bare, cli::ansi_strip(out_ansi))
})

test_that("as.character() and conditionMessage() methods for errors, warnings, and messages", {
  parent_cnd <- error_cnd(
    "foo",
    message = "Parent message.",
    body = c("*" = "Bullet 1.", "*" = "Bullet 2."),
    call = call("foo"),
    use_cli_format = TRUE
  )

  cnd_with <- function(ctor, parent = FALSE) {
    ctor(
      "bar",
      message = "Message.",
      body = c("*" = "Bullet A.", "*" = "Bullet B."),
      call = call("bar"),
      parent = if (parent) parent_cnd,
      use_cli_format = TRUE
    )
  }

  expect_snapshot(cran = TRUE, {
    cat(as.character(cnd_with(error_cnd)))
    cat(as.character(cnd_with(warning_cnd)))
    cat(as.character(cnd_with(message_cnd)))

    cat(as.character(cnd_with(error_cnd, parent = TRUE)))
    cat(as.character(cnd_with(warning_cnd, parent = TRUE)))
    cat(as.character(cnd_with(message_cnd, parent = TRUE)))

    cat(conditionMessage(cnd_with(error_cnd)))
    cat(conditionMessage(cnd_with(warning_cnd)))
    cat(conditionMessage(cnd_with(message_cnd)))

    cat(conditionMessage(cnd_with(error_cnd, parent = TRUE)))
    cat(conditionMessage(cnd_with(warning_cnd, parent = TRUE)))
    cat(conditionMessage(cnd_with(message_cnd, parent = TRUE)))
  })
})

test_that("multiline operator calls are preserved", {
  err <- function(expr) {
    error_cnd(message = "This is the error message.", call = enexpr(expr))
  }

  expect_snapshot_output(err(
    1 +
      ("veeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeery_long" +
        "veeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeery_long")
  ))
  expect_snapshot_output(err(
    {
      1
      2
    } +
      {
        2
        3
      }
  ))
  expect_snapshot_output(err(x[{
    1
    2
  }]))
})

test_that("eval_tidy() is not mentioned in calls", {
  expect_null(format_error_call(quote(eval_tidy(expr))))
})

test_that("header, body, and footer don't partial-match", {
  expect_equal(
    cnd_header(error_cnd("foo", headers = 1)),
    ""
  )
  expect_equal(
    cnd_body(error_cnd("foo", bodyy = 1)),
    chr()
  )
  expect_equal(
    cnd_footer(error_cnd("foo", footers = 1)),
    chr()
  )
})

test_that("can disable srcrefs in call formatting", {
  withr::local_envvar(c("TESTTHAT" = "false"))
  local_options(rlang_call_format_srcrefs = FALSE)

  with_srcref(
    "{
    f <- function() { g() }
    g <- function() abort('foo')
  }"
  )

  expect_snapshot(err(f()))
})

test_that("fallback method supports unknown bullets (#1364)", {
  local_use_cli(format = FALSE)
  expect_snapshot({
    "With fallback"
    (expect_error(abort(c("foo", i2 = "bar"))))
    (expect_error(abort(c(i1 = "foo", i2 = "bar"))))
  })

  local_use_cli(format = TRUE)
  expect_snapshot({
    "With cli"
    (expect_error(abort(c("foo", i2 = "bar"))))
    (expect_error(abort(c(i1 = "foo", i2 = "bar"))))
  })
})

test_that("`cnd_message(prefix = TRUE)` propagates warning style across parent errors (#1387)", {
  local_options(cli.num_colors = 8)

  hnd_message <- function(cnd) cnd_message(cnd, prefix = TRUE)

  msg_warning <- try_fetch(
    error = function(cnd) warn("foo", parent = cnd),
    condition = hnd_message,
    abort("bar")
  )
  msg_error <- try_fetch(
    error = function(cnd) abort("foo", parent = cnd),
    condition = hnd_message,
    abort("bar")
  )

  expect_false(grepl("\033\\[1mCaused by error", msg_warning))
  expect_true(grepl("\033\\[1mCaused by error", msg_error))
})

test_that("arguments are highlighted but code spans are not", {
  local_options("rlang:::trace_test_highlight" = TRUE)

  err <- error_cnd(
    header = function(cnd) {
      sprintf(
        "%s - %s - %s",
        format_arg("arg1"),
        format_code("code"),
        format_arg("arg2")
      )
    }
  )

  expect_snapshot({
    with_error_arg_highlight(
      print(err)
    )
  })
})

test_that("chained errors may have empty messages", {
  parent <- error_cnd(message = "Tilt.")
  child <- error_cnd(parent = parent)
  expect_snapshot({
    print(child)
    cat_line(cnd_message(child, prefix = TRUE))
  })

  # This is the intended usage
  child <- error_cnd(call = call("foo"), parent = parent)
  expect_snapshot({
    print(child)
    cat_line(cnd_message(child, prefix = TRUE))
  })

  # Irrelevant calls are considered as NULL
  child <- error_cnd(call = call("eval"), parent = parent)
  expect_snapshot({
    print(child)
    cat_line(cnd_message(child, prefix = TRUE))
  })
})

test_that("`cnd_message()` returns a single string", {
  local_interactive(TRUE)

  f <- function(do) g(do)
  g <- function(do) h(do)
  h <- function(do) do("foo")

  cnd <- catch_cnd(f(abort))
  cnd <- cnd_set_backtrace_on_error(cnd, "reminder")
  expect_length(cnd_message(cnd), 1)

  class(cnd) <- c("rlang_warning", "warning", "condition")
  expect_length(cnd_message(cnd), 1)
})
