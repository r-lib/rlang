# can request a line break in error bullets (#1130)

    Code
      (expect_error(abort(c("Main header.", "Header 1", x = "Bullet 1", x = "Bullet 2",
        "Header 2", x = "Bullet 3", x = "Bullet 4"))))
    Output
      <error/rlang_error>
      Error: Main header.
      Header 1
      x Bullet 1
      x Bullet 2
      Header 2
      x Bullet 3
      x Bullet 4
    Code
      (expect_error(abort(c("Main header.", "Header 1", x = "Bullet 1", ` ` = "Break line",
        x = "Bullet 2", "", "Header 2", x = "Bullet 3", ` ` = "Break line", x = "Bullet 4")))
      )
    Output
      <error/rlang_error>
      Error: Main header.
      Header 1
      x Bullet 1
        Break line
      x Bullet 2
      
      Header 2
      x Bullet 3
        Break line
      x Bullet 4

# format_error_bullets() generates bullets [plain]

    Code
      format_error_bullets(c("Header.", i = "Bullet."))
    Output
      [1] "Header.\ni Bullet."

# format_error_bullets() generates bullets [ansi]

    Code
      format_error_bullets(c("Header.", i = "Bullet."))
    Output
      [1] "Header.\n\033[34mi\033[39m Bullet."

# format_error_bullets() generates bullets [unicode]

    Code
      format_error_bullets(c("Header.", i = "Bullet."))
    Output
      [1] "Header.\nℹ Bullet."

# format_error_bullets() generates bullets [fancy]

    Code
      format_error_bullets(c("Header.", i = "Bullet."))
    Output
      [1] "Header.\n\033[34mℹ\033[39m Bullet."

# can use cli syntax in `cnd_message()` methods [plain]

    Code
      cnd_message(cnd)
    Output
      [1] "Header: User { {field}.\ni Bullet: User { {field}.\nFooter: User { {field}."

# can use cli syntax in `cnd_message()` methods [fancy]

    Code
      cnd_message(cnd)
    Output
      [1] "\033[1m\033[22m\033[1m\033[1mHeader: \033[3m\033[1m\033[3mUser { {field}.\033[3m\033[1m\033[23m\033[1m\033[22m\n\033[1m\033[22m\033[36mℹ\033[39m Bullet: \033[3m\033[3mUser { {field}.\033[3m\033[23m\n\033[1m\033[22mFooter: \033[3m\033[3mUser { {field}.\033[3m\033[23m"

# long prefixes cause a line break

    Code
      (expect_error(very_very_very_very_very_long_function_name()))
    Output
      <error/rlang_error>
      Error in `very_very_very_very_very_long_function_name()`: 
      My somewhat longish and verbose error message.

# prefixes include srcrefs

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `g()` at bar/baz/myfile.R:2:9: 
      Foo.

# inform() and warn() use fallback bullets formatting

    Code
      local_use_cli(format = FALSE)
      warn(msg)
    Warning <rlang_warning>
      foo
      i bar
    Code
      warn(msg, .frequency = "once", .frequency_id = as.character(runif(1)))
    Warning <rlang_warning>
      foo
      This warning is displayed once per session.
      i bar

---

    Code
      local_use_cli(format = TRUE)
      warn(msg)
    Warning <rlang_warning>
      foo
      i bar
    Code
      warn(msg, .frequency = "once", .frequency_id = as.character(runif(1)))
    Warning <rlang_warning>
      foo
      This warning is displayed once per session.
      i bar

---

    Code
      local_use_cli(format = FALSE)
      inform(msg)
    Message <rlang_message>
      foo
      i bar
    Code
      inform(msg, .frequency = "once", .frequency_id = as.character(runif(1)))
    Message <rlang_message>
      foo
      i bar
      This message is displayed once per session.

---

    Code
      local_use_cli(format = TRUE)
      inform(msg)
    Message <rlang_message>
      foo
      i bar
    Code
      inform(msg, .frequency = "once", .frequency_id = as.character(runif(1)))
    Message <rlang_message>
      foo
      This message is displayed once per session.
      i bar

