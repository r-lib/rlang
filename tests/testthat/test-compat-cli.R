skip_if_not_installed("cli")

cli::test_that_cli(configs = c("plain", "ansi"), "can style strings with cli", {
  expect_snapshot({
    style_emph("foo")
    style_strong("foo")
    style_code("foo")
    style_q("foo")
    style_pkg("foo")
    style_fn("foo")
    style_arg("foo")
    style_kbd("foo")
    style_key("foo")
    style_file("foo")
    style_path("foo")
    style_email("foo")
    style_url("foo")
    style_var("foo")
    style_envvar("foo")
    style_field("foo")
    style_cls("foo")
    style_cls(c("foo", "bar"))
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
    style_emph("{foo {}")
    format_message(style_emph("{foo {}"))
  })
})

cli::test_that_cli(configs = c("plain", "ansi"), "can apply ANSI styles with cli", {
  expect_snapshot({
    ansi_red("foo")
    ansi_blue("foo")
    ansi_green("foo")
    ansi_yellow("foo")
    ansi_magenta("foo")
    ansi_cyan("foo")
    ansi_silver("foo")
    ansi_blurred("foo")
    ansi_bold("foo")
    ansi_italic("foo")
    ansi_underline("foo")
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
