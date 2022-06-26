# can use conditionMessage() method in subclasses of rlang errors

    Code
      cat_line(interactive)
    Output
      Error in `h()`:
      ! dispatched!
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(non_interactive)
    Output
      Error in `h()`:
      ! dispatched!
      Backtrace:
          x
       1. \-global f()
       2.   \-global g()
       3.     \-global h()
       4.       \-rlang::abort("", "foobar_error")
      Execution halted

# rlang_error.print() calls cnd_message() methods

    Code
      print(err)
    Output
      <error/foobar>
      Error in `h()`:
      ! Low-level message
      ---
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang (local) f()
       10. rlang (local) g()
       11. rlang (local) h()

# Overlapping backtraces are printed separately

    Code
      print(err)
    Output
      <error/rlang_error>
      Error in `c()`:
      ! High-level message
      Caused by error in `h()`:
      ! Low-level message
      ---
      Backtrace:
        1. rlang:::catch_error(a())
        9. rlang (local) a()
       10. rlang (local) b()
       11. rlang (local) c()
       16. rlang (local) f()
       17. rlang (local) g()
       18. rlang (local) h()

---

    Code
      print(err, simplify = "none")
    Output
      <error/rlang_error>
      Error in `c()`:
      ! High-level message
      Caused by error in `h()`:
      ! Low-level message
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(a())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) a()
       10.   \-rlang (local) b()
       11.     \-rlang (local) c()
       12.       +-base::tryCatch(...)
       13.       | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
       14.       |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       15.       |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
       16.       \-rlang (local) f()
       17.         \-rlang (local) g()
       18.           \-rlang (local) h()
       19.             \-rlang::abort("", "foobar", foobar_msg = "Low-level message")

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `c()`:
      ! High-level message
      Caused by error in `h()`:
      ! Low-level message
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(a())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) a()
       10.   \-rlang (local) b()
       11.     \-rlang (local) c()
       12.       +-base::tryCatch(...)
       13.       | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
       14.       |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       15.       |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
       16.       \-rlang (local) f()
       17.         \-rlang (local) g()
       18.           \-rlang (local) h()
       19.             \-rlang::abort("", "foobar", foobar_msg = "Low-level message")
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `c()`:
      ! High-level message
      Caused by error in `h()`:
      ! Low-level message
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(a())
        2. | <<\-rlang::catch_cnd(expr, "error")>>
        3. |   <<+-rlang::eval_bare(...)>>
        4. |   <<+-base::tryCatch(...)>>
        5. |   <<| \-base (local) tryCatchList(expr, classes, parentenv, handlers)>>
        6. |   <<|   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])>>
        7. |   <<|     \-base (local) doTryCatch(return(expr), name, parentenv, handler)>>
        8. |   <<\-base::force(expr)>>
        9. \-rlang (local) a()
       10.   \-rlang (local) b()
       11.     \-rlang (local) c()
       12.       +<<-base::tryCatch(...)>>
       13.       | <<\-base (local) tryCatchList(expr, classes, parentenv, handlers)>>
       14.       |   <<\-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])>>
       15.       |     <<\-base (local) doTryCatch(return(expr), name, parentenv, handler)>>
       16.       \-rlang (local) f()
       17.         \-rlang (local) g()
       18.           \-rlang (local) h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `c()`:
      ! High-level message
      Caused by error in `h()`:
      ! Low-level message
      ---
      Backtrace:
        1. rlang:::catch_error(a())
        9. rlang (local) a()
       10. rlang (local) b()
       11. rlang (local) c()
       16. rlang (local) f()
       17. rlang (local) g()
       18. rlang (local) h()

# 3-level ancestry works (#1248)

    Code
      catch_error(high())
    Output
      <error/high>
      Error in `high()`:
      ! High-level
      Caused by error in `mid()`:
      ! Mid-level
      Caused by error in `low()`:
      ! Low-level

# summary.rlang_error() prints full backtrace

    Code
      summary(err)
    Output
      <error/rlang_error>
      Error in `c()`:
      ! The high-level error message
      Caused by error in `h()`:
      ! The low-level error message
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(a())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) a()
       10.   +-base::tryCatch(b())
       11.   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
       12.   \-rlang (local) b()
       13.     \-rlang (local) c()
       14.       +-base::withCallingHandlers(f(), error = handler)
       15.       \-rlang (local) f()
       16.         +-base::tryCatch(g())
       17.         | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
       18.         \-rlang (local) g()
       19.           \-rlang (local) h()
       20.             \-rlang::abort("The low-level error message", foo = "foo")

# don't print message or backtrace fields if empty

    Code
      print(err)
    Output
      <error/foo>

# base parent errors are printed with rlang method

    Code
      print(rlang_err)
    Output
      <error/bar>
      Error:
      ! baz
      Caused by error:
      ! foo

# errors are printed with call

    Code
      print(err)
    Output
      <error/rlang_error>
      Error in `foo()`:
      ! msg

# calls are consistently displayed on rethrow (#1240)

    Code
      (expect_error(with_context(base_problem(), "step_dummy")))
    Output
      <error/rlang_error>
      Error in `step_dummy()`:
      ! Problem while executing step.
      Caused by error in `base_problem()`:
      ! oh no!
    Code
      (expect_error(with_context(rlang_problem(), "step_dummy")))
    Output
      <error/rlang_error>
      Error in `step_dummy()`:
      ! Problem while executing step.
      Caused by error in `rlang_problem()`:
      ! oh no!

# external backtraces are displayed (#1098)

    Code
      print(err)
    Output
      <error/rlang_error>
      Error in `baz()`:
      ! High-level message
      Caused by error in `h()`:
      ! Low-level message
      ---
      Backtrace:
       1. quux()
       2. foofy()
    Code
      summary(err)
    Output
      <error/rlang_error>
      Error in `baz()`:
      ! High-level message
      Caused by error in `h()`:
      ! Low-level message
      ---
      Backtrace:
          x
       1. \-quux()
       2.   \-foofy()

# rethrowing from an exiting handler

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `baz()`:
      ! bar
      Caused by error in `h()`:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang::catch_cnd(foo(), "error")
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        5. | |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        6. | |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        7. | \-base::force(expr)
        8. \-rlang (local) foo()
        9.   \-rlang (local) bar()
       10.     \-rlang (local) baz()
       11.       +-base::tryCatch(f(), error = function(err) abort("bar", parent = err))
       12.       | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
       13.       |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       14.       |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
       15.       \-rlang (local) f()
       16.         \-rlang (local) g()
       17.           \-rlang (local) h()
       18.             \-rlang::abort("foo")
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `baz()`:
      ! bar
      Caused by error in `h()`:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang::catch_cnd(foo(), "error")
        2. | <<+-rlang::eval_bare(...)>>
        3. | <<+-base::tryCatch(...)>>
        4. | <<| \-base (local) tryCatchList(expr, classes, parentenv, handlers)>>
        5. | <<|   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])>>
        6. | <<|     \-base (local) doTryCatch(return(expr), name, parentenv, handler)>>
        7. | <<\-base::force(expr)>>
        8. \-rlang (local) foo()
        9.   \-rlang (local) bar()
       10.     \-rlang (local) baz()
       11.       +<<-base::tryCatch(f(), error = function(err) abort("bar", parent = err))>>
       12.       | <<\-base (local) tryCatchList(expr, classes, parentenv, handlers)>>
       13.       |   <<\-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])>>
       14.       |     <<\-base (local) doTryCatch(return(expr), name, parentenv, handler)>>
       15.       \-rlang (local) f()
       16.         \-rlang (local) g()
       17.           \-rlang (local) h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `baz()`:
      ! bar
      Caused by error in `h()`:
      ! foo
      ---
      Backtrace:
        1. rlang::catch_cnd(foo(), "error")
        8. rlang (local) foo()
        9. rlang (local) bar()
       10. rlang (local) baz()
       15. rlang (local) f()
       16. rlang (local) g()
       17. rlang (local) h()

# bare conditions must be subclassed

    Code
      (expect_error(cnd()))
    Output
      <error/rlang_error>
      Error in `cnd()`:
      ! `class` is absent but must be supplied.
    Code
      (expect_error(signal("")))
    Output
      <error/rlang_error>
      Error in `signal()`:
      ! `class` is absent but must be supplied.

# cnd_type_header() formats condition classes

    Code
      cnd_type_header(error_cnd())
    Output
      [1] "<error/rlang_error>"
    Code
      cnd_type_header(warning_cnd())
    Output
      [1] "<warning/rlang_warning>"
    Code
      cnd_type_header(message_cnd())
    Output
      [1] "<message/rlang_message>"
    Code
      cnd_type_header(error_cnd(class = "foobar"))
    Output
      [1] "<error/foobar>"

# can format warnings and other conditions

    <warning/rlang_warning>
    Warning in `quux()`:
    Header.
    i Bullet.
    ---
    Backtrace:
     1. foo()
     2. bar()

---

    <message/rlang_message>
    Message in `quux()`:
    Header.
    i Bullet.
    Caused by warning in `quux()`:
    ! Header.
    i Bullet.
    ---
    Backtrace:
     1. foo()
     2. bar()

---

    <condition/foobar>
    Condition in `quux()`:
    Header.
    i Bullet.
    ---
    Backtrace:
     1. foo()
     2. bar()

# warnings and messages have `summary()` methods

    Code
      print(warning)
    Output
      <warning/rlang_warning>
      ---
      Backtrace:
       1. f()
       2. g()
    Code
      print(message)
    Output
      <message/rlang_message>
      ---
      Backtrace:
       1. f()
       2. g()
    Code
      summary(warning)
    Output
      <warning/rlang_warning>
      ---
      Backtrace:
          x
       1. \-f()
       2.   \-g()
    Code
      summary(message)
    Output
      <message/rlang_message>
      ---
      Backtrace:
          x
       1. \-f()
       2.   \-g()

# cnd ctors check arguments

    Code
      (expect_error(warning_cnd(class = list())))
    Output
      <error/rlang_error>
      Error in `warning_cnd()`:
      ! `class` must be a character vector, not a list.
    Code
      (expect_error(error_cnd(class = list())))
    Output
      <error/rlang_error>
      Error in `error_cnd()`:
      ! `class` must be a character vector, not a list.
    Code
      (expect_error(message_cnd(message = 1)))
    Output
      <error/rlang_error>
      Error in `message_cnd()`:
      ! `message` must be a character vector, not a number.

# picks up cli format flag

    Code
      cnd_signal(error_cnd(message = c("foo", i = "bar")))
    Error <rlang_error>
      foo
      i bar
    Code
      cnd_signal(warning_cnd(message = c("foo", i = "bar")))
    Warning <rlang_warning>
      foo
      i bar
    Code
      cnd_signal(message_cnd(message = c("foo", i = "bar")))
    Message <rlang_message>
      foo
      i bar

---

    Code
      cnd_signal(error_cnd(message = c("foo", i = "bar")))
    Error <rlang_error>
      foo
      bar
    Code
      cnd_signal(warning_cnd(message = c("foo", i = "bar")))
    Warning <rlang_warning>
      foo
      bar
    Code
      cnd_signal(message_cnd(message = c("foo", i = "bar")))
    Message <rlang_message>
      foo
      bar

# tree display option is picked up when printing errors

    Code
      print(cnd)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
        1. rlang::catch_cnd(f())
        8. rlang (local) f()
        9. rlang (local) g()
       10. rlang (local) h()
    Code
      local({
        local_options(`rlang:::trace_display_tree_override` = TRUE)
        print(cnd)
      })
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang::catch_cnd(f())
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        5. | |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        6. | |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        7. | \-base::force(expr)
        8. \-rlang (local) f()
        9.   \-rlang (local) g()
       10.     \-rlang (local) h()

