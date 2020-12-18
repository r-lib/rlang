#' Add backtrace from error handler
#'
#' @description
#'
#' `entrace()` interrupts an error throw to add an [rlang
#' backtrace][trace_back()] to the error. The error throw is
#' immediately resumed. `cnd_entrace()` adds a backtrace to a
#' condition object, without any other effect. Both functions should
#' be called directly from an error handler.
#'
#' Set the `error` global option to `rlang::entrace` to
#' transform base errors to rlang errors. These enriched errors
#' include a backtrace. The RProfile is a good place to set the
#' handler. See [`rlang_backtrace_on_error`] for details.
#'
#' `entrace()` also works as a [calling][calling] handler, though it
#' is often more practical to use the higher-level function
#' [with_abort()].
#'
#' @inheritParams trace_back
#' @param cnd When `entrace()` is used as a calling handler, `cnd` is
#'   the condition to handle.
#' @param ... Unused. These dots are for future extensions.
#'
#' @seealso [with_abort()] to promote conditions to rlang errors.
#'   [cnd_entrace()] to manually add a backtrace to a condition.
#' @examples
#' if (FALSE) {  # Not run
#'
#' # Set the error handler in your RProfile like this:
#' if (requireNamespace("rlang", quietly = TRUE)) {
#'   options(error = rlang::entrace)
#' }
#'
#' }
#' @export
entrace <- function(cnd, ..., top = NULL, bottom = NULL) {
  check_dots_empty(...)

  if (!missing(cnd) && is_trace(cnd$trace)) {
    return()
  }

  if (is_null(bottom)) {
    nframe <- sys.nframe() - 1
    info <- signal_context_info(nframe)
    bottom <- sys.frame(info[[2]])
  }
  trace <- trace_back(top = top, bottom = bottom)

  if (missing(cnd)) {
    entrace_handle_top(trace)
  } else {
    abort(conditionMessage(cnd) %||% "", error = cnd, trace = trace)
  }
}

#' @rdname entrace
#' @export
cnd_entrace <- function(cnd, ..., top = NULL, bottom = NULL) {
  check_dots_empty(...)

  if (!is_null(cnd$trace)) {
    return(cnd)
  }

  if (is_null(bottom)) {
    nframe <- sys.parent() - 1
    info <- signal_context_info(nframe)
    bottom <- sys.frame(info[[2]])
  }
  cnd$trace <- trace_back(top = top, bottom = bottom)

  cnd
}

#' Return information about signalling context
#'
#' @param nframe The depth of the frame to inspect. In a condition
#'   handler, this would typically be `sys.nframe() - 1L`.
#'
#' @return A named list of two elements `type` and `depth`. The depth
#'   is the call frame number of the signalling context. The type is
#'   one of:
#'
#'   * `"unknown"`
#'   * `"stop_message"` for errors thrown with `base::stop("message")"
#'   * `"stop_condition"` for errors thrown with `base::stop(cnd_object)`
#'   * `"stop_native"` for errors thrown from C
#'   * `"stop_rlang"` for errors thrown with `rlang::abort()`
#'   * `"warning_message"` for warnings signalled with `base::warning("message")"
#'   * `"warning_condition"` for warnings signalled with `base::warning(cnd_object)`
#'   * `"warning_native"` for warnings signalled from C
#'   * `"warning_promoted"` for warnings promoted to errors with `getOption("warn")`
#'   * `"warning_rlang"` for warnings signalled with `rlang::warn()`
#'   * `"message"` for messages signalled with `base::message()`
#'   * `"message_rlang"` for messages signalled with `rlang::inform()`
#'   * `"condition"` for conditions signalled with `base::signalCondition()`
#'
#' @keywords internal
#' @noRd
signal_context_info <- function(nframe) {
  first <- sys_body(nframe)

  if (is_same_body(first, body(.handleSimpleError))) {
    if (is_same_body(sys_body(nframe - 1), body(stop))) {
      return(list(type = "stop_message", depth = nframe - 2))
    } else if (is_same_body(sys_body(nframe - 4), body(.signalSimpleWarning))) {
      return(list(type = "warning_promoted", depth = nframe - 6))
    } else {
      return(list(type = "stop_native", depth = nframe - 1))
    }
  }

  if (is_same_body(first, body(stop))) {
    if (is_same_body(sys_body(nframe - 1), body(abort))) {
      return(list(type = "stop_rlang", depth = nframe - 2))
    } else {
      return(list(type = "stop_condition", depth = nframe - 1))
    }
  }

  if (is_same_body(first, body(signalCondition))) {
    from_restarts <- from_withrestarts(nframe - 1)
    signal_body <- sys_body(nframe - 4)
    if (from_restarts && is_same_body(signal_body, body(message))) {
      return(list(type = "message", depth = nframe - 5))
    } else if (from_restarts && is_same_body(signal_body, body(inform))) {
      return(list(type = "message_rlang", depth = nframe - 5))
    } else {
      return(list(type = "condition", depth = nframe - 1))
    }
  }

  if (from_withrestarts(nframe)) {
    withrestarts_caller <- sys_body(nframe - 3)
    if (is_same_body(withrestarts_caller, body(.signalSimpleWarning))) {
      if (is_same_body(sys_body(nframe - 4), body(warning))) {
        return(list(type = "warning_message", depth = nframe - 5))
      } else {
        return(list(type = "warning_native", depth = nframe - 4))
      }
    } else if (is_same_body(withrestarts_caller, body(warning))) {
      if (is_same_body(sys_body(nframe - 4), body(warn))) {
        return(list(type = "warning_rlang", depth = nframe - 5))
      } else {
        return(list(type = "warning_condition", depth = nframe - 4))
      }
    }
  }

  list(type = "unknown", depth = nframe)
}

from_withrestarts <- function(nframe) {
  is_call(sys.call(nframe), "doWithOneRestart") &&
    is_same_body(sys_body(nframe - 2), body(withRestarts))
}
sys_body <- function(n) {
  body(sys.function(n))
}

entrace_handle_top <- function(trace) {
  # Happens with ctrl-c at top-level
  if (!trace_length(trace)) {
    return(entrace_exit())
  }

  stop_call <- sys.call(-2)
  stop_frame <- sys.frame(-2)
  cnd <- stop_frame$cond

  # False for errors thrown from the C side
  from_stop <- is_call(stop_call, "stop", ns = c("", "base"))

  # No need to do anything for rlang errors
  if (from_stop && (is_trace(cnd$trace) || is_true(cnd$rlang_entraced))) {
    return(entrace_exit())
  }

  if (from_stop) {
    if (is_null(cnd)) {
      msg_call <- quote(.makeMessage(..., domain = domain))
      msg <- eval_bare(msg_call, stop_frame)
    } else {
      msg <- cnd$message
    }
  } else {
    msg <- geterrmessage()
  }

  # Save a fake rlang error containing the backtrace
  err <- error_cnd(message = msg, error = cnd, trace = trace, parent = cnd)
  last_error_env$cnd <- err

  # Print backtrace for current error
  backtrace_lines <- format_onerror_backtrace(err)
  if (length(backtrace_lines)) {
    cat_line(backtrace_lines)
  }

  entrace_exit()
}

entrace_exit <- function() {
  # Disable error handler in non-interactive sessions to force
  # non-zero exit (#1052, rstudio/bookdown#920)
  if (!is_interactive()) {
    options(error = NULL)
  }

  NULL
}

add_backtrace <- function() {
  # Warnings don't go through when error is being handled
  msg <- "Warning: `add_backtrace()` is now exported as `entrace()` as of rlang 0.3.1"
  cat_line(msg, file = stderr())
  entrace(bottom = sys.frame(-1))
}

#' Promote all errors to rlang errors
#'
#' @description
#'
#' `with_abort()` promotes conditions as if they were thrown with
#' [abort()]. These errors embed a [backtrace][trace_back]. They are
#' particularly suitable to be set as *parent errors* (see `parent`
#' argument of [abort()]).
#'
#' @param expr An expression run in a context where errors are
#'   promoted to rlang errors.
#' @param classes Character vector of condition classes that should be
#'   promoted to rlang errors.
#'
#' @details
#'
#' `with_abort()` installs a [calling handler][calling] for errors and
#' rethrows non-rlang errors with [abort()]. However, error handlers
#' installed *within* `with_abort()` have priority. For this reason,
#' you should use [tryCatch()] and [exiting] handlers outside
#' `with_abort()` rather than inside.
#'
#' @examples
#' # with_abort() automatically casts simple errors thrown by stop()
#' # to rlang errors. It is is handy for rethrowing low level
#' # errors. The backtraces are then segmented between the low level
#' # and high level contexts.
#' f <- function() g()
#' g <- function() stop("Low level error")
#'
#' high_level <- function() {
#'   with_handlers(
#'     with_abort(f()),
#'     error = ~ abort("High level error", parent = .)
#'   )
#' }
#' @export
with_abort <- function(expr, classes = "error") {
  handlers <- rep_named(classes, list(entrace))
  handle_call <- rlang::expr(withCallingHandlers(expr, !!!handlers))
  .External2(rlang_ext2_eval, handle_call, current_env())
}
