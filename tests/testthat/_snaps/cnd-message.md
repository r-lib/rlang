# `body` must be a character vector or a function

    Code
      (expect_error(stop(error_cnd("foo", body = 1:3)), "must be"))
    Output
      <error/rlang_error>
      Error in `cnd_body()`:
      ! `body` field must be a character vector or a function.

# can request a line break in error bullets (#1130)

    Code
      (expect_error(abort(c("Main header.", "Header 1", x = "Bullet 1", x = "Bullet 2",
        "Header 2", x = "Bullet 3", x = "Bullet 4"))))
    Output
      <error/rlang_error>
      Error:
      ! Main header.
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
      Error:
      ! Main header.
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
      [1] "\033[1m\033[22mHeader: \033[3mUser { {field}.\033[23m\n\033[36mℹ\033[39m Bullet: \033[3mUser { {field}.\033[23m\nFooter: \033[3mUser { {field}.\033[23m"

# prefix takes call into account

    Code
      err <- error_cnd(message = "msg", call = quote(foo(bar = TRUE)))
      writeLines(cnd_message_format_prefixed(err))
    Output
      Error in `foo()`:
      ! msg
    Code
      err1 <- error_cnd(message = "msg", call = expr(foo(bar = !!(1:3))))
      err2 <- error_cnd(message = "msg", call = quote(foo$bar()))
      err3 <- error_cnd(message = "msg", call = call2(identity))
      writeLines(cnd_message_format_prefixed(err1))
    Output
      Error in `foo()`:
      ! msg
    Code
      writeLines(cnd_message_format_prefixed(err2))
    Output
      Error:
      ! msg
    Code
      writeLines(cnd_message_format_prefixed(err3))
    Output
      Error:
      ! msg

# long prefixes cause a line break

    Code
      (expect_error(very_very_very_very_very_long_function_name()))
    Output
      <error/rlang_error>
      Error in `very_very_very_very_very_long_function_name()`:
      ! My somewhat longish and verbose error message.

# prefixes include srcrefs

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `g()` at bar/baz/myfile.R:2:9:
      ! Foo.

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
      i bar
      This warning is displayed once per session.

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
      i bar
      This warning is displayed once per session.

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
      i bar
      This message is displayed once per session.

# can supply bullet without header

    Code
      (catch_cnd(inform(c(i = "foo")), "message"))
    Output
      <message/rlang_message>
      Message:
      i foo
    Code
      (catch_cnd(warn(c(i = "foo")), "warning"))
    Output
      <warning/rlang_warning>
      Warning:
      i foo

# parent errors prints with bullets in all cases

    Code
      (expect_error(f(TRUE)))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! Wrapper
      Caused by error in `f()`:
      ! Header
      i Bullet
    Code
      (expect_error(f(FALSE)))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! Wrapper
      Caused by error in `f()`:
      ! Header
      i Bullet

# special syntax calls handle edge cases

    Code
      error_call_as_string(quote(`+`()))
    Output
      [1] "`+`()"
    Code
      error_call_as_string(quote(base::`+`(1, 2)))
    Output
      [1] "1 + 2"

# can print message with and without prefix

    Code
      foo <- error_cnd("foo", message = "Parent message.", body = c(`*` = "Bullet 1.",
        `*` = "Bullet 2."), use_cli_format = TRUE)
      bar <- error_cnd("bar", message = "Message.", body = c(`*` = "Bullet A.", `*` = "Bullet B."),
      parent = foo, use_cli_format = TRUE)
      writeLines(cnd_message(foo, prefix = TRUE))
    Output
      Error:
      ! Parent message.
      * Bullet 1.
      * Bullet 2.
    Code
      writeLines(cnd_message(bar, prefix = TRUE))
    Output
      Error:
      ! Message.
      * Bullet A.
      * Bullet B.
      Caused by error:
      ! Parent message.
      * Bullet 1.
      * Bullet 2.
    Code
      writeLines(cnd_message(foo, prefix = FALSE))
    Output
      Parent message.
      * Bullet 1.
      * Bullet 2.
    Code
      writeLines(cnd_message(bar, prefix = FALSE))
    Output
      Message.
      * Bullet A.
      * Bullet B.
      Caused by error:
      ! Parent message.
      * Bullet 1.
      * Bullet 2.

# can print message without inheritance

    Code
      foo <- error_cnd("foo", message = "Parent message.", body = c(`*` = "Bullet 1.",
        `*` = "Bullet 2."), use_cli_format = TRUE)
      bar <- error_cnd("bar", message = "Message.", body = c(`*` = "Bullet A.", `*` = "Bullet B."),
      parent = foo, use_cli_format = TRUE)
      writeLines(cnd_message(foo, inherit = FALSE, prefix = TRUE))
    Output
      Error:
      ! Parent message.
      * Bullet 1.
      * Bullet 2.
    Code
      writeLines(cnd_message(bar, inherit = FALSE, prefix = TRUE))
    Output
      Error:
      ! Message.
      * Bullet A.
      * Bullet B.
    Code
      writeLines(cnd_message(foo, inherit = FALSE, prefix = FALSE))
    Output
      Parent message.
      * Bullet 1.
      * Bullet 2.
    Code
      writeLines(cnd_message(bar, inherit = FALSE, prefix = FALSE))
    Output
      Message.
      * Bullet A.
      * Bullet B.

# as.character() and conditionMessage() methods for errors, warnings, and messages

    Code
      cat(as.character(cnd_with(error_cnd)))
    Output
      Error in `bar()`:
      ! Message.
      * Bullet A.
      * Bullet B.
    Code
      cat(as.character(cnd_with(warning_cnd)))
    Output
      Warning in `bar()`:
      Message.
      * Bullet A.
      * Bullet B.
    Code
      cat(as.character(cnd_with(message_cnd)))
    Output
      Message in `bar()`:
      Message.
      * Bullet A.
      * Bullet B.
    Code
      cat(as.character(cnd_with(error_cnd, parent = TRUE)))
    Output
      Error in `bar()`:
      ! Message.
      * Bullet A.
      * Bullet B.
      Caused by error in `foo()`:
      ! Parent message.
      * Bullet 1.
      * Bullet 2.
    Code
      cat(as.character(cnd_with(warning_cnd, parent = TRUE)))
    Output
      Warning in `bar()`:
      Message.
      * Bullet A.
      * Bullet B.
      Caused by error in `foo()`:
      ! Parent message.
      * Bullet 1.
      * Bullet 2.
    Code
      cat(as.character(cnd_with(message_cnd, parent = TRUE)))
    Output
      Message in `bar()`:
      Message.
      * Bullet A.
      * Bullet B.
      Caused by error in `foo()`:
      ! Parent message.
      * Bullet 1.
      * Bullet 2.
    Code
      cat(conditionMessage(cnd_with(error_cnd)))
    Output
      Message.
      * Bullet A.
      * Bullet B.
    Code
      cat(conditionMessage(cnd_with(warning_cnd)))
    Output
      Message.
      * Bullet A.
      * Bullet B.
    Code
      cat(conditionMessage(cnd_with(message_cnd)))
    Output
      Message.
      * Bullet A.
      * Bullet B.
    Code
      cat(conditionMessage(cnd_with(error_cnd, parent = TRUE)))
    Output
      Message.
      * Bullet A.
      * Bullet B.
      Caused by error in `foo()`:
      ! Parent message.
      * Bullet 1.
      * Bullet 2.
    Code
      cat(conditionMessage(cnd_with(warning_cnd, parent = TRUE)))
    Output
      Message.
      * Bullet A.
      * Bullet B.
      Caused by error in `foo()`:
      ! Parent message.
      * Bullet 1.
      * Bullet 2.
    Code
      cat(conditionMessage(cnd_with(message_cnd, parent = TRUE)))
    Output
      Message.
      * Bullet A.
      * Bullet B.
      Caused by error in `foo()`:
      ! Parent message.
      * Bullet 1.
      * Bullet 2.

# multiline operator calls are preserved

    <error/rlang_error>
    Error in `1 + ("veeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeery_long" +
        "veeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeery_long")`:
    ! This is the error message.

---

    <error/rlang_error>
    Error in `{
        1
        2
      } + {
        2
        3
      }`:
    ! This is the error message.

---

    <error/rlang_error>
    Error in `x[{
        1
        2
      }]`:
    ! This is the error message.

# can disable srcrefs in call formatting

    Code
      err(f())
    Output
      <error/rlang_error>
      Error in `g()`:
      ! foo

# fallback method supports unknown bullets (#1364)

    Code
      # With fallback
      (expect_error(abort(c("foo", i2 = "bar"))))
    Output
      <error/rlang_error>
      Error:
      ! foo
      bar
    Code
      (expect_error(abort(c(i1 = "foo", i2 = "bar"))))
    Output
      <error/rlang_error>
      Error:
      ! foo
      bar

---

    Code
      # With cli
      (expect_error(abort(c("foo", i2 = "bar"))))
    Output
      <error/rlang_error>
      Error:
      ! foo
      bar
    Code
      (expect_error(abort(c(i1 = "foo", i2 = "bar"))))
    Output
      <error/rlang_error>
      Error:
      foo
      bar

