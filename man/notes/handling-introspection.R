# This file surveys the possible backtraces created during condition
# handling. We are interested in detecting the following frames:
#
# - Setup frame, e.g. `withCallingHandlers()` or `tryCatch()`. The
#   caller of this frame is useful as the default `call` when
#   rethrowing an error.
#
# - Signal frame, e.g. `stop()`, `warning()`, `signalCondition()`. Can
#   be a user frame as well when the condition is signalled from C,
#   e.g. with `Rf_error()`.
#
# - Handler frame. This is adjacent to the signal frame but may have
#   various intervening frames in between. Mostly this is about
#   detecting that we rethrowing from a handler.
#
# Knowing about these frames is useful for backtrace simplification
# (the default display in `rlang::last_error()`) and for figuring out
# a good default `call` field.
#
#
# # Backtrace simplification
#
# We generally want to hide everything between the signal frame and
# the handler frame. It's particularly important for the linear
# display of backtraces where we subset the last branch of the
# backtrace tree. If we didn't clean these frames, the user context
# would disappear in favour of (or in the best case be drowned in)
# condition handling frames.
#
#
# # Default call inference
#
# When rethrowing a chained error, the relevant `call` field is the
# caller of `withCallingHandlers()`. The backtrace between that setup
# frame and the signal frame can be arbitrarily large and there is
# currently no way of finding the setup frame with 100% reliability
# (there might be several on the stack).


f <- function() {
  throw()
}

handle <- function(...) {
  handler_helper()
  invokeRestart("abort")
}
handler_helper <- function() {
  print(rlang::trace_back())
}


#  Errors - Calling handlers

foo <- function(...) {
  withCallingHandlers(f(), ...)
}

### Text error

# Setup: 2
# Signal: 5
# Handler: 7
throw <- function() stop("foo")
foo(error = handle)
#>     ▆
#>  1. ├─global foo(error = handle)
#> >2. │ ├─base::withCallingHandlers(f(), ...)
#>  3. │ └─global f()
#>  4. │   └─global throw()
#> >5. │     └─base::stop("foo")
#>  6. └─base::.handleSimpleError(`<fn>`, "foo", base::quote(throw()))
#> >7.   └─global h(simpleError(msg, call))
#>  8.     └─global handler_helper()


### Condition error

# Setup: 2
# Signal: 5
# Handler: 6
throw <- function() stop(simpleError("foo"))
foo(error = handle)
#>     ▆
#>  1. ├─global foo(error = handle)
#> >2. │ ├─base::withCallingHandlers(f(), ...)
#>  3. │ └─global f()
#>  4. │   └─global throw()
#> >5. │     └─base::stop(simpleError("foo"))
#> >6. └─global `<fn>`(`<smplErrr>`)
#>  7.   └─global handler_helper()


### Condition error, simple signal

# Setup: 2
# Signal: 5
# Handler: 6
throw <- function() signalCondition(simpleError("foo"))
foo(error = handle)
#>     ▆
#>  1. ├─global foo(error = handle)
#> >2. │ ├─base::withCallingHandlers(f(), ...)
#>  3. │ └─global f()
#>  4. │   └─global throw()
#> >5. │     └─base::signalCondition(simpleError("foo"))
#> >6. └─global `<fn>`(`<smplErrr>`)
#>  7.   └─global handler_helper()


### Condition error, demoted to warning

# Setup: 2
# Signal: 5
# Handler: 9
throw <- function() warning(simpleError("foo"))
foo(error = handle)
#>     ▆
#>   1. ├─global foo(error = handle)
#>  >2. │ ├─base::withCallingHandlers(f(), ...)
#>   3. │ └─global f()
#>   4. │   └─global throw()
#>  >5. │     └─base::warning(simpleError("foo"))
#>   6. │       └─base::withRestarts(...)
#>   7. │         └─base withOneRestart(expr, restarts[[1L]])
#>   8. │           └─base doWithOneRestart(return(expr), restart)
#>  >9. └─global `<fn>`(`<smplErrr>`)
#>  10.   └─global handler_helper()


### Condition error, demoted to message

# Setup: 2
# Signal: 5
# Handler: 10
throw <- function() message(simpleError("foo"))
foo(error = handle)
#>      ▆
#>   1. ├─global foo(error = handle)
#>   2. │ ├─base::withCallingHandlers(f(), ...)
#>   3. │ └─global f()
#>   4. │   └─global throw()
#>   5. │     └─base::message(simpleError("foo"))
#>   6. │       ├─base::withRestarts(...)
#>   7. │       │ └─base withOneRestart(expr, restarts[[1L]])
#>   8. │       │   └─base doWithOneRestart(return(expr), restart)
#>   9. │       └─base::signalCondition(cond)
#>  10. └─global `<fn>`(`<smplErrr>`)
#>  11.   └─global handler_helper()


### C-level error

# In this case, the signal frame is a user function.

# Setup: 2
# Signal: 5
# Handler: 7
throw <- function() rlang:::errorcall(NULL, "foo")
foo(error = handle)
#>     ▆
#>  1. ├─global foo(error = handle)
#> >2. │ ├─base::withCallingHandlers(f(), ...)
#>  3. │ └─global f()
#>  4. │   └─global throw()
#> >5. │     └─rlang:::errorcall(NULL, "foo")
#>  6. └─base::.handleSimpleError(`<fn>`, "foo", base::quote(NULL))
#> >7.   └─global h(simpleError(msg, call))
#>  8.     └─global handler_helper()


### Text warning promoted to error

# The stack is not linear between 5 and 11. Compare to the next
# backtrace which is linear(ish).

# Setup: 2
# Signal: 5
# Handler: 11
throw <- function() {
  rlang::local_options(warn = 2)
  warning("foo")
}
foo(error = handle)
#>      ▆
#>   1. ├─global foo(error = handle)
#>  >2. │ ├─base::withCallingHandlers(f(), ...)
#>   3. │ └─global f()
#>   4. │   └─global throw()
#>  >5. │     └─base::warning("foo")
#>   6. ├─base::.signalSimpleWarning("foo", base::quote(throw()))
#>   7. │ └─base::withRestarts(...)
#>   8. │   └─base withOneRestart(expr, restarts[[1L]])
#>   9. │     └─base doWithOneRestart(return(expr), restart)
#>  10. └─base::.handleSimpleError(...)
#> >11.   └─global h(simpleError(msg, call))
#>  12.     └─global handler_helper()


### Condition warning promoted to error

# Setup: 2
# Signal: 5
# Handler: 10
throw <- function() {
  rlang::local_options(warn = 2)
  warning(simpleWarning("foo"))
}
foo(error = handle)
#>     ▆
#>   1. ├─global foo(error = handle)
#>  >2. │ ├─base::withCallingHandlers(f(), ...)
#>   3. │ └─global f()
#>   4. │   └─global throw()
#>  >5. │     └─base::warning(simpleWarning("foo"))
#>   6. │       └─base::withRestarts(...)
#>   7. │         └─base withOneRestart(expr, restarts[[1L]])
#>   8. │           └─base doWithOneRestart(return(expr), restart)
#>   9. └─base::.handleSimpleError(...)
#> >10.   └─global h(simpleError(msg, call))
#>  11.     └─global handler_helper()


### rlang error

# Setup: 2
# Signal: 5
# Handler: 10
throw <- function() rlang::abort("foo")
foo(error = handle)
#>      ▆
#>  1. ├─global foo(error = handle)
#>  2. │ ├─base::withCallingHandlers(f(), ...)
#>  3. │ └─global f()
#>  4. │   └─global throw()
#>  5. │     └─rlang::abort("foo")
#>  6. │       └─rlang::signal_abort(cnd, .file) at rlang/R/cnd-abort.R:281:2
#>  7. │         └─base::signalCondition(cnd) at rlang/R/cnd-abort.R:641:4
#>  8. └─global `<fn>`(`<rlng_rrr>`)
#>  9.   └─global handler_helper()


#  Errors - Exiting handlers

# This is much easier, all the stacks look the same!

bar <- function(...) {
  tryCatch(f(), ...)
}

# These all produce the same stack
throw <- function() stop("foo")
throw <- function() stop(simpleError("foo"))
throw <- function() rlang:::errorcall(NULL, "foo")
throw <- function() {
  rlang::local_options(warn = 2)
  warning("foo")
}

# Setup: 2
# Handler: 5
bar(error = handle)
#>     ▆
#>  1. └─global bar(error = handle)
#> >2.   └─base::tryCatch(f(), ...)
#>  3.     └─base tryCatchList(expr, classes, parentenv, handlers)
#>  4.       └─base tryCatchOne(expr, names, parentenv, handlers[[1L]])
#> >5.         └─value[[3L]](cond)
#>  6.           └─global handler_helper()

# The stack could be larger between 2 and 5 depending on the number of
# condition handlers passed to `tryCatch()`.
