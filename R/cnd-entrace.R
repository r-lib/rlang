#' Entrace unexpected errors
#'
#' @description
#' `global_entrace()` enriches base errors, warnings, and messages
#' with rlang features.
#'
#' - They are assigned a backtrace. You can configure whether to
#'   display a backtrace on error with the [rlang_backtrace_on_error]
#'   global option.
#'
#' - They are recorded in [last_error()], [last_warnings()], or
#'   [last_messages()]. You can inspect backtraces at any time by
#'   calling these functions.
#'
#' Set global entracing in your RProfile with:
#'
#' ```
#' rlang::global_entrace()
#' ```
#'
#' @param enable Whether to enable or disable global handling.
#' @param class A character vector of one or several classes of
#'   conditions to be entraced.
#'
#' @section Inside RMarkdown documents:
#'
#' Call `global_entrace()` inside an RMarkdown document to cause
#' errors and warnings to be promoted to rlang conditions that include
#' a backtrace. This needs to be done in a separate setup chunk before
#' the first error or warning.
#'
#' This is useful in conjunction with
#' [`rlang_backtrace_on_error_report`] and
#' [`rlang_backtrace_on_warning_report`]. To get full entracing in an
#' Rmd document, include this in a setup chunk before the first error
#' or warning is signalled.
#'
#' ````
#' ```{r setup}
#' rlang::global_entrace()
#' options(rlang_backtrace_on_warning_report = "full")
#' options(rlang_backtrace_on_error_report = "full")
#' ```
#' ````
#'
#' @section Under the hood:
#' On R 4.0 and newer, `global_entrace()` installs a global handler
#' with `globalCallingHandlers()`. On older R versions, `entrace()` is
#' set as an `option(error = )` handler. The latter method has the
#' disadvantage that only one handler can be set at a time. This means
#' that you need to manually switch between `entrace()` and other
#' handlers like [recover()]. Also this causes a conflict with IDE
#' handlers (e.g. in RStudio).
#' @export
global_entrace <- function(
  enable = TRUE,
  class = c("error", "warning", "message")
) {
  check_bool(enable)
  class <- arg_match(class, multiple = TRUE)

  if (getRversion() < "4.0" && !knitr_in_progress()) {
    return(global_entrace_fallback(enable, class))
  }

  handlers <- rep_named(class, list(hnd_entrace))
  poke_global_handlers(enable, !!!handlers)

  invisible(NULL)
}
global_entrace_fallback <- function(enable, class) {
  if (!"error" %in% class) {
    return(invisible(NULL))
  }

  if (enable) {
    options(error = entrace)
  } else {
    opt <- peek_option("error")
    if (identical(opt, entrace)) {
      options(error = NULL)
    }
  }

  invisible(NULL)
}

# Keep `rlang::` indirection in case rlang is reloaded. This way the
# global handlers can be set once in RProfile and they will always
# call into the most recently loaded version.
hnd_entrace <- function(cnd) rlang::entrace(cnd)

# Set to `base_env()` to avoid duplicate handlers in case of
# reload. This makes `global_entrace()` idempotent.  Requires
# https://bugs.r-project.org/show_bug.cgi?id=18197
environment(hnd_entrace) <- baseenv()


#' Add backtrace from error handler
#'
#' @keywords internal
#' @description
#' `entrace()` is a low level function. See [global_entrace()] for a
#' user-friendly way of enriching errors and other conditions from
#' your RProfile.
#'
#' * `entrace()` is meant to be used as a global handler. It enriches
#'   conditions with a backtrace. Errors are saved to [last_error()]
#'   and rethrown immediately. Messages and warnings are recorded into
#'   [last_messages()] and [last_warnings()] and let through.
#'
#' * `cnd_entrace()` adds a backtrace to a condition object, without
#'   any other effect. It should be called from a condition handler.
#'
#' `entrace()` also works as an `option(error = )` handler for
#' compatibility with versions of R older than 4.0.
#'
#' When used as calling handler, rlang trims the handler invokation
#' context from the backtrace.
#'
#' @inheritParams trace_back
#' @param cnd When `entrace()` is used as a calling handler, `cnd` is
#'   the condition to handle.
#' @param ... Unused. These dots are for future extensions.
#'
#' @seealso [global_entrace()] for configuring errors with
#'   `entrace()`. [cnd_entrace()] to manually add a backtrace to a
#'   condition.
#' @examples
#' quote({  # Not run
#'
#' # Set `entrace()` globally in your RProfile
#' globalCallingHandlers(error = rlang::entrace)
#'
#' # On older R versions which don't feature `globalCallingHandlers`,
#' # set the error handler like this:
#' options(error = rlang::entrace)
#'
#' })
#' @keywords internal
#' @export
entrace <- function(cnd, ..., top = NULL, bottom = NULL) {
  check_dots_empty0(...)

  if (!missing(cnd) && inherits(cnd, "rlang_error")) {
    poke_last_error(cnd)
    return()
  }

  if (is_null(bottom)) {
    if (missing(cnd)) {
      bottom <- current_env()
    } else {
      bottom <- caller_env()
    }
  }

  # Remove handler invokation context from the trace
  if (is_environment(bottom)) {
    nframe <- eval_bare(quote(base::sys.nframe()), bottom) - 1
    info <- signal_context_info(nframe)
    bottom <- sys.frame(info[[2]])
  }

  if (!has_new_cmd_frame() && the$n_conditions >= max_entracing()) {
    trace <- NULL
  } else {
    trace <- trace_back(top = top, bottom = bottom)
  }

  # `options(error = )` case
  if (missing(cnd)) {
    return(entrace_handle_top(trace))
  }

  # Log warnings
  if (is_warning(cnd)) {
    wrn <- as_rlang_warning(cnd, trace)
    push_warning(wrn)

    # Resignal enriched warning
    if (!is_null(findRestart("muffleWarning"))) {
      if (identical(peek_option("warn"), 2L)) {
        return()
      } else {
        warning(wrn)
        invokeRestart("muffleWarning")
      }
    } else {
      return()
    }
  }

  # Log messages
  if (is_message(cnd)) {
    push_message(as_rlang_message(cnd, trace))
    return()
  }

  # Rethrow errors
  if (is_error(cnd)) {
    if (has_recover()) {
      return()
    }
    entraced <- error_cnd(
      message = conditionMessage(cnd) %||% "",
      call = conditionCall(cnd),
      error = cnd,
      trace = trace,
      use_cli_format = FALSE
    )
    poke_last_error(entraced)
    cnd_signal(entraced)
  }

  # Ignore other condition types
  NULL
}

max_entracing <- function() {
  peek_option("rlang:::max_entracing") %||% 20
}

has_recover <- function() {
  handler_call <- peek_option("error")
  if (!is_call(handler_call)) {
    return(FALSE)
  }

  if (is_call(handler_call, "recover", ns = c("", "base"))) {
    return(TRUE)
  }

  identical(handler_call[[1]], utils::recover)
}

#' @rdname entrace
#' @export
cnd_entrace <- function(cnd, ..., top = NULL, bottom = NULL) {
  check_dots_empty0(...)

  if (cnd_some(cnd, function(x) !is_null(x[["trace"]]))) {
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

  if (identical(first, body(.handleSimpleError))) {
    if (identical(sys_body(nframe - 1), body(stop))) {
      return(list(type = "stop_message", depth = nframe - 2))
    } else if (identical(sys_body(nframe - 4), body(.signalSimpleWarning))) {
      return(list(type = "warning_promoted", depth = nframe - 6))
    } else {
      return(list(type = "stop_native", depth = nframe - 1))
    }
  }

  if (identical(first, body(stop))) {
    if (identical(sys_body(nframe - 1), body(abort))) {
      return(list(type = "stop_rlang", depth = nframe - 2))
    } else {
      return(list(type = "stop_condition", depth = nframe - 1))
    }
  }

  if (identical(first, body(signalCondition))) {
    from_restarts <- from_withrestarts(nframe - 1)
    signal_body <- sys_body(nframe - 4)
    if (from_restarts && identical(signal_body, body(message))) {
      return(list(type = "message", depth = nframe - 5))
    } else if (from_restarts && identical(signal_body, body(inform))) {
      return(list(type = "message_rlang", depth = nframe - 5))
    } else {
      return(list(type = "condition", depth = nframe - 1))
    }
  }

  if (from_withrestarts(nframe)) {
    withrestarts_caller <- sys_body(nframe - 3)
    if (identical(withrestarts_caller, body(.signalSimpleWarning))) {
      if (identical(sys_body(nframe - 4), body(warning))) {
        return(list(type = "warning_message", depth = nframe - 5))
      } else {
        return(list(type = "warning_native", depth = nframe - 4))
      }
    } else if (identical(withrestarts_caller, body(warning))) {
      if (identical(sys_body(nframe - 4), body(warn))) {
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
    identical(sys_body(nframe - 2), body(withRestarts))
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
  if (
    from_stop && (is_trace(cnd$trace) || is_true(cnd$rlang$internal$entraced))
  ) {
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
    # `geterrmessage()` returns the full error message including
    # prefix and newline, which we strip here
    msg <- geterrmessage()
    msg <- sub("^.*: ?", "", msg)
    msg <- sub("\n$", "", msg)
  }

  # Save a fake rlang error containing the backtrace
  err <- error_cnd(message = msg, error = cnd, trace = trace, parent = cnd)
  poke_last_error(err)

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
