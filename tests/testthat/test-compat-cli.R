skip_if_not_installed("cli")

cli::test_that_cli(configs = c("plain", "ansi"), "can style strings with cli", {
  expect_snapshot({
    mark_emph("foo")
    mark_strong("foo")
    mark_code("foo")
    mark_q("foo")
    mark_pkg("foo")
    mark_fn("foo")
    mark_arg("foo")
    mark_kbd("foo")
    mark_key("foo")
    mark_file("foo")
    mark_path("foo")
    mark_email("foo")
    mark_url("foo")
    mark_var("foo")
    mark_envvar("foo")
    mark_field("foo")
    mark_cls("foo")
    mark_cls(c("foo", "bar"))
  })
})

cli::test_that_cli(configs = c("plain", "ansi"), "can format strings with cli", {
  expect_snapshot({
    format_emph("foo")
    format_strong("foo")
    format_code("foo")
    format_q("foo")
    format_pkg("foo")
    format_fn("foo")
    format_arg("foo")
    format_kbd("foo")
    format_key("foo")
    format_file("foo")
    format_path("foo")
    format_email("foo")
    format_url("foo")
    format_var("foo")
    format_envvar("foo")
    format_field("foo")
    format_cls("foo")
    format_cls(c("foo", "bar"))
  })
})

cli::test_that_cli(configs = c("plain", "ansi"), "styled strings may contain `{` syntax", {
  expect_snapshot({
    mark_emph("{foo {}")
    format_message(mark_emph("{foo {}"))
  })
})

cli::test_that_cli(configs = c("plain", "ansi"), "can apply ANSI styles with cli", {
  expect_snapshot({
    col_red("foo")
    col_blue("foo")
    col_green("foo")
    col_yellow("foo")
    col_magenta("foo")
    col_cyan("foo")
    col_silver("foo")
    style_blurred("foo")
    style_bold("foo")
    style_italic("foo")
    style_underline("foo")
  })
})

cli::test_that_cli("can create symbols with cli", {
  expect_snapshot({
    symbol_info()
    symbol_cross()
    symbol_tick()
    symbol_bullet()
    symbol_arrow()
    symbol_alert()
  })
})

cli::test_that_cli("can create ANSI symbols with cli", {
  expect_snapshot({
    ansi_info()
    ansi_cross()
    ansi_tick()
    ansi_bullet()
    ansi_arrow()
    ansi_alert()
  })
})

cli::test_that_cli("can format messages", {
  expect_snapshot({
    format_error(c("Header", "i" = "Bullet."))
    format_warning(c("Header", "i" = "Bullet."))
    format_message(c("Header", "i" = "Bullet."))
  })
})

cli::test_that_cli("formatters restore strings", {
  expect_true(is_bare_character(format_error("foo")))
  expect_true(is_bare_character(format_warning("foo")))
  expect_true(is_bare_character(format_message("foo")))
})

cli::test_that_cli(configs = c("plain", "ansi"), "cli_escape() conditionally escapes `{`", {
  expect_snapshot({
    format_error(cli_escape("{"))
  })
})
