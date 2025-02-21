#' Register default global handlers
#'
#' @description
#' `global_handle()` sets up a default configuration for error,
#' warning, and message handling. It calls:
#'
#' * [global_entrace()] to enable rlang errors and warnings globally.
#'
#' * [global_prompt_install()] to recover from `packageNotFoundError`s
#'   with a user prompt to install the missing package. Note that at
#'   the time of writing (R 4.1), there are only very limited
#'   situations where this handler works.
#'
#' @param entrace Passed as `enable` argument to [global_entrace()].
#' @param prompt_install Passed as `enable` argument to
#'   [global_prompt_install()].
#'
#' @export
global_handle <- function(entrace = TRUE, prompt_install = TRUE) {
  check_bool(entrace)
  check_bool(prompt_install)

  global_entrace(entrace)
  global_prompt_install(prompt_install)

  invisible(NULL)
}

#' Prompt user to install missing packages
#'
#' @description
#' When enabled, `packageNotFoundError` thrown by [loadNamespace()]
#' cause a user prompt to install the missing package and continue
#' without interrupting the current program.
#'
#' This is similar to how [check_installed()] prompts users to install
#' required packages. It uses the same install strategy, using pak if
#' available and [install.packages()] otherwise.
#'
#' @inheritParams global_entrace
#' @export
global_prompt_install <- function(enable = TRUE) {
  check_bool(enable)

  if (getRversion() <= "4.0") {
    return(invisible(NULL))
  }

  poke_global_handlers(
    enable,
    packageNotFoundError = hnd_prompt_install
  )
}

# To help with `load_all()`, hard-code to base env with `rlang::`
# indirection
hnd_prompt_install <- function(cnd) {
  if (!rlang::is_interactive()) {
    return(rlang::zap())
  }

  # Be defensive to avoid weird errors
  if (
    !rlang::is_string(cnd$package) ||
      is.null(findRestart("retry_loadNamespace"))
  ) {
    return(rlang::zap())
  }

  rlang::check_installed(cnd$package)
  invokeRestart("retry_loadNamespace")
}
environment(hnd_prompt_install) <- baseenv()


#' Try an expression with condition handlers
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `try_fetch()` establishes handlers for conditions of a given class
#' (`"error"`, `"warning"`, `"message"`, ...). Handlers are functions
#' that take a condition object as argument and are called when the
#' corresponding condition class has been signalled.
#'
#' A condition handler can:
#'
#' -   **Recover from conditions** with a value. In this case the computation of
#'     `expr` is aborted and the recovery value is returned from
#'     `try_fetch()`. Error recovery is useful when you don't want
#'     errors to abruptly interrupt your program but resume at the
#'     catching site instead.
#'
#'     ```
#'     # Recover with the value 0
#'     try_fetch(1 + "", error = function(cnd) 0)
#'     ```
#'
#' -   **Rethrow conditions**, e.g. using `abort(msg, parent = cnd)`.
#'     See the `parent` argument of [abort()]. This is typically done to
#'     add information to low-level errors about the high-level context
#'     in which they occurred.
#'
#'     ```
#'     try_fetch(1 + "", error = function(cnd) abort("Failed.", parent = cnd))
#'     ```
#'
#' -   **Inspect conditions**, for instance to log data about warnings
#'     or errors. In this case, the handler must return the [zap()]
#'     sentinel to instruct `try_fetch()` to ignore (or zap) that
#'     particular handler. The next matching handler is called if any,
#'     and errors bubble up to the user if no handler remains.
#'
#'     ```
#'     log <- NULL
#'     try_fetch(1 + "", error = function(cnd) {
#'       log <<- cnd
#'       zap()
#'     })
#'     ```
#'
#' Whereas `tryCatch()` catches conditions (discarding any running
#' code along the way) and then calls the handler, `try_fetch()` first
#' calls the handler with the condition on top of the currently
#' running code (fetches it where it stands) and then catches the
#' return value. This is a subtle difference that has implications
#' for the debuggability of your functions. See the comparison with
#' `tryCatch()` section below.
#'
#' Another difference between `try_fetch()` and the base equivalent is
#' that errors are matched across chains, see the `parent` argument of
#' [abort()]. This is a useful property that makes `try_fetch()`
#' insensitive to changes of implementation or context of evaluation
#' that cause a classed error to suddenly get chained to a contextual
#' error. Note that some chained conditions are not inherited, see the
#' `.inherit` argument of [abort()] or [warn()]. In particular,
#' downgraded conditions (e.g. from error to warning or from warning
#' to message) are not matched across parents.
#'
#' @param expr An R expression.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named condition
#'   handlers. The names specify the condition class for which a
#'   handler will be called.
#'
#' @section Stack overflows:
#'
#' A stack overflow occurs when a program keeps adding to itself until
#' the stack memory (whose size is very limited unlike heap memory) is
#' exhausted.
#'
#' ```
#' # A function that calls itself indefinitely causes stack overflows
#' f <- function() f()
#' f()
#' #> Error: C stack usage  9525680 is too close to the limit
#' ```
#'
#' Because memory is very limited when these errors happen, it is not
#' possible to call the handlers on the existing program stack.
#' Instead, error conditions are first caught by `try_fetch()` and only
#' then error handlers are called. Catching the error interrupts the
#' program up to the `try_fetch()` context, which allows R to reclaim
#' stack memory.
#'
#' The practical implication is that error handlers should never
#' assume that the whole call stack is preserved. For instance a
#' [trace_back()] capture might miss frames.
#'
#' Note that error handlers are only run for stack overflows on R >=
#' 4.2. On older versions of R the handlers are simply not run. This
#' is because these errors do not inherit from the class
#' `stackOverflowError` before R 4.2. Consider using [tryCatch()]
#' instead with critical error handlers that need to capture all
#' errors on old versions of R.
#'
#' @section Comparison with `tryCatch()`:
#'
#' `try_fetch()` generalises `tryCatch()` and `withCallingHandlers()`
#' in a single function. It reproduces the behaviour of both calling
#' and exiting handlers depending on the return value of the handler.
#' If the handler returns the [zap()] sentinel, it is taken as a
#' calling handler that declines to recover from a condition.
#' Otherwise, it is taken as an exiting handler which returns a value
#' from the catching site.
#'
#' The important difference between `tryCatch()` and `try_fetch()` is
#' that the program in `expr` is still fully running when an error
#' handler is called. Because the call stack is preserved, this makes
#' it possible to capture a full backtrace from within the handler,
#' e.g. when rethrowing the error with `abort(parent = cnd)`.
#' Technically, `try_fetch()` is more similar to (and implemented on
#' top of) [base::withCallingHandlers()] than `tryCatch().`
#'
#' @export
try_fetch <- function(expr, ...) {
  frame <- environment()

  catch <- value <- NULL

  throw <- function(x) {
    value <<- x
    delayedAssign("catch", return(value), frame, frame)
    catch
  }

  .External(ffi_try_fetch, frame)
}

handler_call <- quote(function(cnd) {
  {
    .__handler_frame__. <- TRUE
    .__setup_frame__. <- frame
    if (inherits(cnd, "message")) {
      except <- c("warning", "error")
    } else if (inherits(cnd, "warning")) {
      except <- "error"
    } else {
      except <- ""
    }
  }

  while (!is_null(cnd)) {
    if (inherits(cnd, CLASS)) {
      out <- handlers[[I]](cnd)
      if (!inherits(out, "rlang_zap")) throw(out)
    }

    inherit <- .subset2(.subset2(cnd, "rlang"), "inherit")
    if (is_false(inherit)) {
      return()
    }

    cnd <- .subset2(cnd, "parent")
  }
})


#' Catch a condition
#'
#' This is a small wrapper around `tryCatch()` that captures any
#' condition signalled while evaluating its argument. It is useful for
#' situations where you expect a specific condition to be signalled,
#' for debugging, and for unit testing.
#'
#' @param expr Expression to be evaluated with a catching condition
#'   handler.
#' @param classes A character vector of condition classes to catch. By
#'   default, catches all conditions.
#' @return A condition if any was signalled, `NULL` otherwise.
#' @examples
#' catch_cnd(10)
#' catch_cnd(abort("an error"))
#' catch_cnd(signal("my_condition", message = "a condition"))
#' @export
catch_cnd <- function(expr, classes = "condition") {
  stopifnot(is_character(classes))
  handlers <- rep_named(classes, list(identity))

  eval_bare(rlang::expr(
    tryCatch(!!!handlers, {
      force(expr)
      return(NULL)
    })
  ))
}

#' Muffle a condition
#'
#' Unlike [exiting()] handlers, [calling()] handlers must be explicit
#' that they have handled a condition to stop it from propagating to
#' other handlers. Use `cnd_muffle()` within a calling handler (or as
#' a calling handler, see examples) to prevent any other handlers from
#' being called for that condition.
#'
#'
#' @section Mufflable conditions:
#'
#' Most conditions signalled by base R are muffable, although the name
#' of the restart varies. cnd_muffle() will automatically call the
#' correct restart for you. It is compatible with the following
#' conditions:
#'
#' * `warning` and `message` conditions. In this case `cnd_muffle()`
#'   is equivalent to [base::suppressMessages()] and
#'   [base::suppressWarnings()].
#'
#' * Bare conditions signalled with `signal()` or [cnd_signal()]. Note
#'   that conditions signalled with [base::signalCondition()] are not
#'   mufflable.
#'
#' * Interrupts are sometimes signalled with a `resume` restart on
#'   recent R versions. When this is the case, you can muffle the
#'   interrupt with `cnd_muffle()`. Check if a restart is available
#'   with `base::findRestart("resume")`.
#'
#' If you call `cnd_muffle()` with a condition that is not mufflable
#' you will cause a new error to be signalled.
#'
#' * Errors are not mufflable since they are signalled in critical
#'   situations where execution cannot continue safely.
#'
#' * Conditions captured with [base::tryCatch()], [with_handlers()] or
#'   [catch_cnd()] are no longer mufflable. Muffling restarts _must_
#'   be called from a [calling] handler.
#'
#' @param cnd A condition to muffle.
#' @return If `cnd` is mufflable, `cnd_muffle()` jumps to the muffle
#'   restart and doesn't return. Otherwise, it returns `FALSE`.
#'
#' @examples
#' fn <- function() {
#'   inform("Beware!", "my_particular_msg")
#'   inform("On your guard!")
#'   "foobar"
#' }
#'
#' # Let's install a muffling handler for the condition thrown by `fn()`.
#' # This will suppress all `my_particular_wng` warnings but let other
#' # types of warnings go through:
#' with_handlers(fn(),
#'   my_particular_msg = calling(function(cnd) {
#'     inform("Dealt with this particular message")
#'     cnd_muffle(cnd)
#'   })
#' )
#'
#' # Note how execution of `fn()` continued normally after dealing
#' # with that particular message.
#'
#' # cnd_muffle() can also be passed to with_handlers() as a calling
#' # handler:
#' with_handlers(fn(),
#'   my_particular_msg = calling(cnd_muffle)
#' )
#' @keywords internal
#' @export
cnd_muffle <- function(cnd) {
  restart <- switch(
    cnd_type(cnd),
    message = "muffleMessage",
    warning = "muffleWarning",
    interrupt = "resume",
    "rlang_muffle"
  )

  if (!is_null(findRestart(restart))) {
    invokeRestart(restart)
  }

  FALSE
}

if (getRversion() < "4.0") {
  utils::globalVariables("globalCallingHandlers")
}


poke_global_handlers <- function(enable, ...) {
  check_bool(enable)
  handlers <- list2(...)
  in_knitr <- knitr_in_progress()

  if (in_knitr) {
    if (enable) {
      knitr::opts_chunk$set(calling.handlers = handlers)
    } else {
      abort("Can't remove calling handlers in knitted documents")
    }
  } else {
    if (enable) {
      inject(globalCallingHandlers(!!!handlers))
    } else {
      inject(drop_global_handlers(!!!handlers))
    }
  }
}

drop_global_handlers <- function(...) {
  to_pop <- list(...)
  handlers <- globalCallingHandlers()

  for (i in seq_along(to_pop)) {
    if (loc <- detect_index(handlers, identical, to_pop[[i]])) {
      if (is_string(names(to_pop)[[i]], names(handlers)[[loc]])) {
        handlers[[loc]] <- NULL
      }
    }
  }

  globalCallingHandlers(NULL)
  globalCallingHandlers(handlers)
}
