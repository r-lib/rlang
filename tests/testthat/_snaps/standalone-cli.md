# can style strings with cli [plain]

    Code
      mark_emph("foo")
    Output
      [1] "{.emph {\"foo\"}}"
    Code
      mark_strong("foo")
    Output
      [1] "{.strong {\"foo\"}}"
    Code
      mark_code("foo")
    Output
      [1] "{.code {\"foo\"}}"
    Code
      mark_q("foo")
    Output
      [1] "{.q {\"foo\"}}"
    Code
      mark_pkg("foo")
    Output
      [1] "{.pkg {\"foo\"}}"
    Code
      mark_fn("foo")
    Output
      [1] "{.fn {\"foo\"}}"
    Code
      mark_arg("foo")
    Output
      [1] "{.arg {\"foo\"}}"
    Code
      mark_kbd("foo")
    Output
      [1] "{.kbd {\"foo\"}}"
    Code
      mark_key("foo")
    Output
      [1] "{.key {\"foo\"}}"
    Code
      mark_file("foo")
    Output
      [1] "{.file {\"foo\"}}"
    Code
      mark_path("foo")
    Output
      [1] "{.path {\"foo\"}}"
    Code
      mark_email("foo")
    Output
      [1] "{.email {\"foo\"}}"
    Code
      mark_url("foo")
    Output
      [1] "{.url {\"foo\"}}"
    Code
      mark_var("foo")
    Output
      [1] "{.var {\"foo\"}}"
    Code
      mark_envvar("foo")
    Output
      [1] "{.envvar {\"foo\"}}"
    Code
      mark_field("foo")
    Output
      [1] "{.field {\"foo\"}}"
    Code
      mark_cls("foo")
    Output
      [1] "{.cls {\"foo\"}}"
    Code
      mark_cls(c("foo", "bar"))
    Output
      [1] "{.cls {\"foo\"}}" "{.cls {\"bar\"}}"

# can style strings with cli [ansi]

    Code
      mark_emph("foo")
    Output
      [1] "{.emph {\"foo\"}}"
    Code
      mark_strong("foo")
    Output
      [1] "{.strong {\"foo\"}}"
    Code
      mark_code("foo")
    Output
      [1] "{.code {\"foo\"}}"
    Code
      mark_q("foo")
    Output
      [1] "{.q {\"foo\"}}"
    Code
      mark_pkg("foo")
    Output
      [1] "{.pkg {\"foo\"}}"
    Code
      mark_fn("foo")
    Output
      [1] "{.fn {\"foo\"}}"
    Code
      mark_arg("foo")
    Output
      [1] "{.arg {\"foo\"}}"
    Code
      mark_kbd("foo")
    Output
      [1] "{.kbd {\"foo\"}}"
    Code
      mark_key("foo")
    Output
      [1] "{.key {\"foo\"}}"
    Code
      mark_file("foo")
    Output
      [1] "{.file {\"foo\"}}"
    Code
      mark_path("foo")
    Output
      [1] "{.path {\"foo\"}}"
    Code
      mark_email("foo")
    Output
      [1] "{.email {\"foo\"}}"
    Code
      mark_url("foo")
    Output
      [1] "{.url {\"foo\"}}"
    Code
      mark_var("foo")
    Output
      [1] "{.var {\"foo\"}}"
    Code
      mark_envvar("foo")
    Output
      [1] "{.envvar {\"foo\"}}"
    Code
      mark_field("foo")
    Output
      [1] "{.field {\"foo\"}}"
    Code
      mark_cls("foo")
    Output
      [1] "{.cls {\"foo\"}}"
    Code
      mark_cls(c("foo", "bar"))
    Output
      [1] "{.cls {\"foo\"}}" "{.cls {\"bar\"}}"

# can format strings with cli [plain]

    Code
      format_emph("foo")
    Output
      [1] "foo"
    Code
      format_strong("foo")
    Output
      [1] "foo"
    Code
      format_code("foo")
    Output
      [1] "`foo`"
    Code
      format_q("foo")
    Output
      [1] "\"foo\""
    Code
      format_pkg("foo")
    Output
      [1] "foo"
    Code
      format_fn("foo")
    Output
      [1] "`foo()`"
    Code
      format_arg("foo")
    Output
      [1] "`foo`"
    Code
      format_kbd("foo")
    Output
      [1] "[foo]"
    Code
      format_key("foo")
    Output
      [1] "[foo]"
    Code
      format_file("foo")
    Output
      [1] "'foo'"
    Code
      format_path("foo")
    Output
      [1] "'foo'"
    Code
      format_email("foo")
    Output
      [1] "'foo'"
    Code
      format_url("foo")
    Output
      [1] "<foo>"
    Code
      format_var("foo")
    Output
      [1] "`foo`"
    Code
      format_envvar("foo")
    Output
      [1] "`foo`"
    Code
      format_field("foo")
    Output
      [1] "foo"
    Code
      format_cls("foo")
    Output
      [1] "<foo>"
    Code
      format_cls(c("foo", "bar"))
    Output
      [1] "<foo/bar>"

# can format strings with cli [ansi]

    Code
      format_emph("foo")
    Output
      [1] "\033[3mfoo\033[23m"
    Code
      format_strong("foo")
    Output
      [1] "\033[1mfoo\033[22m"
    Code
      format_code("foo")
    Output
      [1] "`foo`"
    Code
      format_q("foo")
    Output
      [1] "\"foo\""
    Code
      format_pkg("foo")
    Output
      [1] "\033[34mfoo\033[39m"
    Code
      format_fn("foo")
    Output
      [1] "`foo()`"
    Code
      format_arg("foo")
    Output
      [1] "`foo`"
    Code
      format_kbd("foo")
    Output
      [1] "\033[34m[foo]\033[39m"
    Code
      format_key("foo")
    Output
      [1] "\033[34m[foo]\033[39m"
    Code
      format_file("foo")
    Output
      [1] "\033[34mfoo\033[39m"
    Code
      format_path("foo")
    Output
      [1] "\033[34mfoo\033[39m"
    Code
      format_email("foo")
    Output
      [1] "\033[34mfoo\033[39m"
    Code
      format_url("foo")
    Output
      [1] "\033[3m\033[34m<foo>\033[39m\033[23m"
    Code
      format_var("foo")
    Output
      [1] "`foo`"
    Code
      format_envvar("foo")
    Output
      [1] "`foo`"
    Code
      format_field("foo")
    Output
      [1] "\033[32mfoo\033[39m"
    Code
      format_cls("foo")
    Output
      [1] "\033[34m<foo>\033[39m"
    Code
      format_cls(c("foo", "bar"))
    Output
      [1] "\033[34m<foo/bar>\033[39m"

# styled strings may contain `{` syntax [plain]

    Code
      mark_emph("{foo {}")
    Output
      [1] "{.emph {\"{foo {}\"}}"
    Code
      format_message(mark_emph("{foo {}"))
    Output
      [1] "{foo {}"

# styled strings may contain `{` syntax [ansi]

    Code
      mark_emph("{foo {}")
    Output
      [1] "{.emph {\"{foo {}\"}}"
    Code
      format_message(mark_emph("{foo {}"))
    Output
      [1] "\033[1m\033[22m\033[3m{foo {}\033[23m"

# can apply ANSI styles with cli [plain]

    Code
      col_black("foo")
    Output
      [1] "foo"
    Code
      col_blue("foo")
    Output
      [1] "foo"
    Code
      col_cyan("foo")
    Output
      [1] "foo"
    Code
      col_green("foo")
    Output
      [1] "foo"
    Code
      col_magenta("foo")
    Output
      [1] "foo"
    Code
      col_red("foo")
    Output
      [1] "foo"
    Code
      col_white("foo")
    Output
      [1] "foo"
    Code
      col_yellow("foo")
    Output
      [1] "foo"
    Code
      col_grey("foo")
    Output
      [1] "foo"
    Code
      col_silver("foo")
    Output
      [1] "foo"
    Code
      col_none("foo")
    Output
      [1] "foo"
    Code
      bg_black("foo")
    Output
      [1] "foo"
    Code
      bg_blue("foo")
    Output
      [1] "foo"
    Code
      bg_cyan("foo")
    Output
      [1] "foo"
    Code
      bg_green("foo")
    Output
      [1] "foo"
    Code
      bg_magenta("foo")
    Output
      [1] "foo"
    Code
      bg_red("foo")
    Output
      [1] "foo"
    Code
      bg_white("foo")
    Output
      [1] "foo"
    Code
      bg_yellow("foo")
    Output
      [1] "foo"
    Code
      bg_none("foo")
    Output
      [1] "foo"
    Code
      style_dim("foo")
    Output
      [1] "foo"
    Code
      style_blurred("foo")
    Output
      [1] "foo"
    Code
      style_bold("foo")
    Output
      [1] "foo"
    Code
      style_hidden("foo")
    Output
      [1] "foo"
    Code
      style_inverse("foo")
    Output
      [1] "foo"
    Code
      style_italic("foo")
    Output
      [1] "foo"
    Code
      style_strikethrough("foo")
    Output
      [1] "foo"
    Code
      style_underline("foo")
    Output
      [1] "foo"
    Code
      style_no_dim("foo")
    Output
      [1] "foo"
    Code
      style_no_blurred("foo")
    Output
      [1] "foo"
    Code
      style_no_bold("foo")
    Output
      [1] "foo"
    Code
      style_no_hidden("foo")
    Output
      [1] "foo"
    Code
      style_no_inverse("foo")
    Output
      [1] "foo"
    Code
      style_no_italic("foo")
    Output
      [1] "foo"
    Code
      style_no_strikethrough("foo")
    Output
      [1] "foo"
    Code
      style_no_underline("foo")
    Output
      [1] "foo"
    Code
      style_reset("foo")
    Output
      [1] "foo"
    Code
      style_no_colour("foo")
    Output
      [1] "foo"
    Code
      style_no_bg_colour("foo")
    Output
      [1] "foo"

# can apply ANSI styles with cli [ansi]

    Code
      col_black("foo")
    Output
      [1] "\033[30mfoo\033[39m"
    Code
      col_blue("foo")
    Output
      [1] "\033[34mfoo\033[39m"
    Code
      col_cyan("foo")
    Output
      [1] "\033[36mfoo\033[39m"
    Code
      col_green("foo")
    Output
      [1] "\033[32mfoo\033[39m"
    Code
      col_magenta("foo")
    Output
      [1] "\033[35mfoo\033[39m"
    Code
      col_red("foo")
    Output
      [1] "\033[31mfoo\033[39m"
    Code
      col_white("foo")
    Output
      [1] "\033[37mfoo\033[39m"
    Code
      col_yellow("foo")
    Output
      [1] "\033[33mfoo\033[39m"
    Code
      col_grey("foo")
    Output
      [1] "\033[90mfoo\033[39m"
    Code
      col_silver("foo")
    Output
      [1] "\033[90mfoo\033[39m"
    Code
      col_none("foo")
    Output
      [1] "\033[0m\033[22m\033[23m\033[24m\033[27m\033[28m\033[29m\033[49mfoo\033[39m"
    Code
      bg_black("foo")
    Output
      [1] "\033[40mfoo\033[49m"
    Code
      bg_blue("foo")
    Output
      [1] "\033[44mfoo\033[49m"
    Code
      bg_cyan("foo")
    Output
      [1] "\033[46mfoo\033[49m"
    Code
      bg_green("foo")
    Output
      [1] "\033[42mfoo\033[49m"
    Code
      bg_magenta("foo")
    Output
      [1] "\033[45mfoo\033[49m"
    Code
      bg_red("foo")
    Output
      [1] "\033[41mfoo\033[49m"
    Code
      bg_white("foo")
    Output
      [1] "\033[47mfoo\033[49m"
    Code
      bg_yellow("foo")
    Output
      [1] "\033[43mfoo\033[49m"
    Code
      bg_none("foo")
    Output
      [1] "\033[0m\033[22m\033[23m\033[24m\033[27m\033[28m\033[29m\033[39mfoo\033[49m"
    Code
      style_dim("foo")
    Output
      [1] "\033[2mfoo\033[22m"
    Code
      style_blurred("foo")
    Output
      [1] "\033[2mfoo\033[22m"
    Code
      style_bold("foo")
    Output
      [1] "\033[1mfoo\033[22m"
    Code
      style_hidden("foo")
    Output
      [1] "\033[8mfoo\033[28m"
    Code
      style_inverse("foo")
    Output
      [1] "\033[7mfoo\033[27m"
    Code
      style_italic("foo")
    Output
      [1] "\033[3mfoo\033[23m"
    Code
      style_strikethrough("foo")
    Output
      [1] "\033[9mfoo\033[29m"
    Code
      style_underline("foo")
    Output
      [1] "\033[4mfoo\033[24m"
    Code
      style_no_dim("foo")
    Output
      [1] "\033[0m\033[23m\033[24m\033[27m\033[28m\033[29m\033[39m\033[49mfoo\033[22m"
    Code
      style_no_blurred("foo")
    Output
      [1] "\033[0m\033[23m\033[24m\033[27m\033[28m\033[29m\033[39m\033[49mfoo\033[22m"
    Code
      style_no_bold("foo")
    Output
      [1] "\033[0m\033[23m\033[24m\033[27m\033[28m\033[29m\033[39m\033[49mfoo\033[22m"
    Code
      style_no_hidden("foo")
    Output
      [1] "\033[0m\033[22m\033[23m\033[24m\033[27m\033[29m\033[39m\033[49mfoo\033[28m"
    Code
      style_no_inverse("foo")
    Output
      [1] "\033[0m\033[22m\033[23m\033[24m\033[28m\033[29m\033[39m\033[49mfoo\033[27m"
    Code
      style_no_italic("foo")
    Output
      [1] "\033[0m\033[22m\033[24m\033[27m\033[28m\033[29m\033[39m\033[49mfoo\033[23m"
    Code
      style_no_strikethrough("foo")
    Output
      [1] "\033[0m\033[22m\033[23m\033[24m\033[27m\033[28m\033[39m\033[49mfoo\033[29m"
    Code
      style_no_underline("foo")
    Output
      [1] "\033[0m\033[22m\033[23m\033[27m\033[28m\033[29m\033[39m\033[49mfoo\033[24m"
    Code
      style_reset("foo")
    Output
      [1] "\033[0mfoo\033[0m\033[22m\033[23m\033[24m\033[27m\033[28m\033[29m\033[39m\033[49m"
    Code
      style_no_colour("foo")
    Output
      [1] "\033[0m\033[22m\033[23m\033[24m\033[27m\033[28m\033[29m\033[49mfoo\033[39m"
    Code
      style_no_bg_colour("foo")
    Output
      [1] "\033[0m\033[22m\033[23m\033[24m\033[27m\033[28m\033[29m\033[39mfoo\033[49m"

# can create symbols with cli [plain]

    Code
      symbol_info()
    Output
      [1] "i"
    Code
      symbol_cross()
    Output
      [1] "x"
    Code
      symbol_tick()
    Output
      [1] "v"
    Code
      symbol_bullet()
    Output
      [1] "*"
    Code
      symbol_arrow()
    Output
      [1] ">"
    Code
      symbol_alert()
    Output
      [1] "!"

# can create symbols with cli [ansi]

    Code
      symbol_info()
    Output
      [1] "i"
    Code
      symbol_cross()
    Output
      [1] "x"
    Code
      symbol_tick()
    Output
      [1] "v"
    Code
      symbol_bullet()
    Output
      [1] "*"
    Code
      symbol_arrow()
    Output
      [1] ">"
    Code
      symbol_alert()
    Output
      [1] "!"

# can create symbols with cli [unicode]

    Code
      symbol_info()
    Output
      [1] "ℹ"
    Code
      symbol_cross()
    Output
      [1] "✖"
    Code
      symbol_tick()
    Output
      [1] "✔"
    Code
      symbol_bullet()
    Output
      [1] "•"
    Code
      symbol_arrow()
    Output
      [1] "→"
    Code
      symbol_alert()
    Output
      [1] "!"

# can create symbols with cli [fancy]

    Code
      symbol_info()
    Output
      [1] "ℹ"
    Code
      symbol_cross()
    Output
      [1] "✖"
    Code
      symbol_tick()
    Output
      [1] "✔"
    Code
      symbol_bullet()
    Output
      [1] "•"
    Code
      symbol_arrow()
    Output
      [1] "→"
    Code
      symbol_alert()
    Output
      [1] "!"

# can create ANSI symbols with cli [plain]

    Code
      ansi_info()
    Output
      [1] "i"
    Code
      ansi_cross()
    Output
      [1] "x"
    Code
      ansi_tick()
    Output
      [1] "v"
    Code
      ansi_bullet()
    Output
      [1] "*"
    Code
      ansi_arrow()
    Output
      [1] ">"
    Code
      ansi_alert()
    Output
      [1] "!"

# can create ANSI symbols with cli [ansi]

    Code
      ansi_info()
    Output
      [1] "\033[34mi\033[39m"
    Code
      ansi_cross()
    Output
      [1] "\033[31mx\033[39m"
    Code
      ansi_tick()
    Output
      [1] "\033[32mv\033[39m"
    Code
      ansi_bullet()
    Output
      [1] "\033[36m*\033[39m"
    Code
      ansi_arrow()
    Output
      [1] ">"
    Code
      ansi_alert()
    Output
      [1] "\033[33m!\033[39m"

# can create ANSI symbols with cli [unicode]

    Code
      ansi_info()
    Output
      [1] "ℹ"
    Code
      ansi_cross()
    Output
      [1] "✖"
    Code
      ansi_tick()
    Output
      [1] "✔"
    Code
      ansi_bullet()
    Output
      [1] "•"
    Code
      ansi_arrow()
    Output
      [1] "→"
    Code
      ansi_alert()
    Output
      [1] "!"

# can create ANSI symbols with cli [fancy]

    Code
      ansi_info()
    Output
      [1] "\033[34mℹ\033[39m"
    Code
      ansi_cross()
    Output
      [1] "\033[31m✖\033[39m"
    Code
      ansi_tick()
    Output
      [1] "\033[32m✔\033[39m"
    Code
      ansi_bullet()
    Output
      [1] "\033[36m•\033[39m"
    Code
      ansi_arrow()
    Output
      [1] "→"
    Code
      ansi_alert()
    Output
      [1] "\033[33m!\033[39m"

# can format messages [plain]

    Code
      format_error(c("Header", i = "Bullet."))
    Output
      [1] "Header\ni Bullet."
    Code
      format_warning(c("Header", i = "Bullet."))
    Output
      [1] "Header\ni Bullet."
    Code
      format_message(c("Header", i = "Bullet."))
    Output
      [1] "Header\ni Bullet."

# can format messages [ansi]

    Code
      format_error(c("Header", i = "Bullet."))
    Output
      [1] "\033[1m\033[22mHeader\n\033[36mi\033[39m Bullet."
    Code
      format_warning(c("Header", i = "Bullet."))
    Output
      [1] "\033[1m\033[22mHeader\n\033[36mi\033[39m Bullet."
    Code
      format_message(c("Header", i = "Bullet."))
    Output
      [1] "\033[1m\033[22mHeader\n\033[36mi\033[39m Bullet."

# can format messages [unicode]

    Code
      format_error(c("Header", i = "Bullet."))
    Output
      [1] "Header\nℹ Bullet."
    Code
      format_warning(c("Header", i = "Bullet."))
    Output
      [1] "Header\nℹ Bullet."
    Code
      format_message(c("Header", i = "Bullet."))
    Output
      [1] "Header\nℹ Bullet."

# can format messages [fancy]

    Code
      format_error(c("Header", i = "Bullet."))
    Output
      [1] "\033[1m\033[22mHeader\n\033[36mℹ\033[39m Bullet."
    Code
      format_warning(c("Header", i = "Bullet."))
    Output
      [1] "\033[1m\033[22mHeader\n\033[36mℹ\033[39m Bullet."
    Code
      format_message(c("Header", i = "Bullet."))
    Output
      [1] "\033[1m\033[22mHeader\n\033[36mℹ\033[39m Bullet."

# cli_escape() conditionally escapes `{` [plain]

    Code
      format_error(cli_escape("{"))
    Output
      [1] "{"

# cli_escape() conditionally escapes `{` [ansi]

    Code
      format_error(cli_escape("{"))
    Output
      [1] "\033[1m\033[22m{"

