# can style strings with cli [plain]

    Code
      mark_emph("foo")
    Output
      [1] "_foo_"
    Code
      mark_strong("foo")
    Output
      [1] "*foo*"
    Code
      mark_code("foo")
    Output
      [1] "`foo`"
    Code
      mark_q("foo")
    Output
      [1] "foo"
    Code
      mark_pkg("foo")
    Output
      [1] "foo"
    Code
      mark_fn("foo")
    Output
      [1] "`foo()`"
    Code
      mark_arg("foo")
    Output
      [1] "`foo`"
    Code
      mark_kbd("foo")
    Output
      [1] "[foo]"
    Code
      mark_key("foo")
    Output
      [1] "[foo]"
    Code
      mark_file("foo")
    Output
      [1] "foo"
    Code
      mark_path("foo")
    Output
      [1] "foo"
    Code
      mark_email("foo")
    Output
      [1] "foo"
    Code
      mark_url("foo")
    Output
      [1] "<foo>"
    Code
      mark_var("foo")
    Output
      [1] "`foo`"
    Code
      mark_envvar("foo")
    Output
      [1] "`foo`"
    Code
      mark_field("foo")
    Output
      [1] "foo"
    Code
      mark_cls("foo")
    Output
      [1] "<foo>"
    Code
      mark_cls(c("foo", "bar"))
    Output
      [1] "<foo/bar>"

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
      [1] "_foo_"
    Code
      format_strong("foo")
    Output
      [1] "*foo*"
    Code
      format_code("foo")
    Output
      [1] "`foo`"
    Code
      format_q("foo")
    Output
      [1] "foo"
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
      [1] "foo"
    Code
      format_path("foo")
    Output
      [1] "foo"
    Code
      format_email("foo")
    Output
      [1] "foo"
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
      [1] "\033[1m\033[22m\033[3mfoo\033[23m"
    Code
      format_strong("foo")
    Output
      [1] "\033[1m\033[22m\033[1mfoo\033[22m"
    Code
      format_code("foo")
    Output
      [1] "\033[1m\033[22m\033[30m\033[47m`foo`\033[49m\033[39m"
    Code
      format_q("foo")
    Output
      [1] "\033[1m\033[22m\"foo\""
    Code
      format_pkg("foo")
    Output
      [1] "\033[1m\033[22m\033[34mfoo\033[39m"
    Code
      format_fn("foo")
    Output
      [1] "\033[1m\033[22m\033[30m\033[47m`foo()`\033[49m\033[39m"
    Code
      format_arg("foo")
    Output
      [1] "\033[1m\033[22m\033[30m\033[47m`foo`\033[49m\033[39m"
    Code
      format_kbd("foo")
    Output
      [1] "\033[1m\033[22m\033[34m[foo]\033[39m"
    Code
      format_key("foo")
    Output
      [1] "\033[1m\033[22m\033[34m[foo]\033[39m"
    Code
      format_file("foo")
    Output
      [1] "\033[1m\033[22m\033[34mfoo\033[39m"
    Code
      format_path("foo")
    Output
      [1] "\033[1m\033[22m\033[34mfoo\033[39m"
    Code
      format_email("foo")
    Output
      [1] "\033[1m\033[22m\033[34mfoo\033[39m"
    Code
      format_url("foo")
    Output
      [1] "\033[1m\033[22m\033[3m\033[34m<foo>\033[39m\033[23m"
    Code
      format_var("foo")
    Output
      [1] "\033[1m\033[22m\033[30m\033[47m`foo`\033[49m\033[39m"
    Code
      format_envvar("foo")
    Output
      [1] "\033[1m\033[22m\033[30m\033[47m`foo`\033[49m\033[39m"
    Code
      format_field("foo")
    Output
      [1] "\033[1m\033[22m\033[32mfoo\033[39m"
    Code
      format_cls("foo")
    Output
      [1] "\033[1m\033[22m\033[34m<foo>\033[39m"
    Code
      format_cls(c("foo", "bar"))
    Output
      [1] "\033[1m\033[22m\033[34m<foo/bar>\033[39m"

# styled strings may contain `{` syntax [plain]

    Code
      mark_emph("{foo {}")
    Output
      [1] "_{foo {}_"
    Code
      format_message(mark_emph("{foo {}"))
    Output
      [1] "_{foo {}_"

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
      <ansi_string>
      [1] [30mfoo[39m
    Code
      col_blue("foo")
    Output
      <ansi_string>
      [1] [34mfoo[39m
    Code
      col_cyan("foo")
    Output
      <ansi_string>
      [1] [36mfoo[39m
    Code
      col_green("foo")
    Output
      <ansi_string>
      [1] [32mfoo[39m
    Code
      col_magenta("foo")
    Output
      <ansi_string>
      [1] [35mfoo[39m
    Code
      col_red("foo")
    Output
      <ansi_string>
      [1] [31mfoo[39m
    Code
      col_white("foo")
    Output
      <ansi_string>
      [1] [37mfoo[39m
    Code
      col_yellow("foo")
    Output
      <ansi_string>
      [1] [33mfoo[39m
    Code
      col_grey("foo")
    Output
      <ansi_string>
      [1] [90mfoo[39m
    Code
      col_silver("foo")
    Output
      <ansi_string>
      [1] [90mfoo[39m
    Code
      col_none("foo")
    Output
      <ansi_string>
      [1] [0m[22m[23m[24m[27m[28m[29m[49mfoo[39m
    Code
      bg_black("foo")
    Output
      <ansi_string>
      [1] [40mfoo[49m
    Code
      bg_blue("foo")
    Output
      <ansi_string>
      [1] [44mfoo[49m
    Code
      bg_cyan("foo")
    Output
      <ansi_string>
      [1] [46mfoo[49m
    Code
      bg_green("foo")
    Output
      <ansi_string>
      [1] [42mfoo[49m
    Code
      bg_magenta("foo")
    Output
      <ansi_string>
      [1] [45mfoo[49m
    Code
      bg_red("foo")
    Output
      <ansi_string>
      [1] [41mfoo[49m
    Code
      bg_white("foo")
    Output
      <ansi_string>
      [1] [47mfoo[49m
    Code
      bg_yellow("foo")
    Output
      <ansi_string>
      [1] [43mfoo[49m
    Code
      bg_none("foo")
    Output
      <ansi_string>
      [1] [0m[22m[23m[24m[27m[28m[29m[39mfoo[49m
    Code
      style_dim("foo")
    Output
      <ansi_string>
      [1] [2mfoo[22m
    Code
      style_blurred("foo")
    Output
      <ansi_string>
      [1] [2mfoo[22m
    Code
      style_bold("foo")
    Output
      <ansi_string>
      [1] [1mfoo[22m
    Code
      style_hidden("foo")
    Output
      <ansi_string>
      [1] [8mfoo[28m
    Code
      style_inverse("foo")
    Output
      <ansi_string>
      [1] [7mfoo[27m
    Code
      style_italic("foo")
    Output
      <ansi_string>
      [1] [3mfoo[23m
    Code
      style_strikethrough("foo")
    Output
      <ansi_string>
      [1] [9mfoo[29m
    Code
      style_underline("foo")
    Output
      <ansi_string>
      [1] [4mfoo[24m
    Code
      style_no_dim("foo")
    Output
      <ansi_string>
      [1] [0m[23m[24m[27m[28m[29m[39m[49mfoo[22m
    Code
      style_no_blurred("foo")
    Output
      <ansi_string>
      [1] [0m[23m[24m[27m[28m[29m[39m[49mfoo[22m
    Code
      style_no_bold("foo")
    Output
      <ansi_string>
      [1] [0m[23m[24m[27m[28m[29m[39m[49mfoo[22m
    Code
      style_no_hidden("foo")
    Output
      <ansi_string>
      [1] [0m[22m[23m[24m[27m[29m[39m[49mfoo[28m
    Code
      style_no_inverse("foo")
    Output
      <ansi_string>
      [1] [0m[22m[23m[24m[28m[29m[39m[49mfoo[27m
    Code
      style_no_italic("foo")
    Output
      <ansi_string>
      [1] [0m[22m[24m[27m[28m[29m[39m[49mfoo[23m
    Code
      style_no_strikethrough("foo")
    Output
      <ansi_string>
      [1] [0m[22m[23m[24m[27m[28m[39m[49mfoo[29m
    Code
      style_no_underline("foo")
    Output
      <ansi_string>
      [1] [0m[22m[23m[27m[28m[29m[39m[49mfoo[24m
    Code
      style_reset("foo")
    Output
      <ansi_string>
      [1] [0mfoo[0m[22m[23m[24m[27m[28m[29m[39m[49m
    Code
      style_no_colour("foo")
    Output
      <ansi_string>
      [1] [0m[22m[23m[24m[27m[28m[29m[49mfoo[39m
    Code
      style_no_bg_colour("foo")
    Output
      <ansi_string>
      [1] [0m[22m[23m[24m[27m[28m[29m[39mfoo[49m

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
      <ansi_string>
      [1] [34mi[39m
    Code
      ansi_cross()
    Output
      <ansi_string>
      [1] [31mx[39m
    Code
      ansi_tick()
    Output
      <ansi_string>
      [1] [32mv[39m
    Code
      ansi_bullet()
    Output
      <ansi_string>
      [1] [36m*[39m
    Code
      ansi_arrow()
    Output
      [1] ">"
    Code
      ansi_alert()
    Output
      <ansi_string>
      [1] [33m![39m

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
      <ansi_string>
      [1] [34mℹ[39m
    Code
      ansi_cross()
    Output
      <ansi_string>
      [1] [31m✖[39m
    Code
      ansi_tick()
    Output
      <ansi_string>
      [1] [32m✔[39m
    Code
      ansi_bullet()
    Output
      <ansi_string>
      [1] [36m•[39m
    Code
      ansi_arrow()
    Output
      [1] "→"
    Code
      ansi_alert()
    Output
      <ansi_string>
      [1] [33m![39m

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

