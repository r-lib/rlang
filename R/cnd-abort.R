#' Signal an error, warning, or message
#'
#' @description
#'
#' These functions are equivalent to base functions [base::stop()],
#' [base::warning()], and [base::message()], but make it easy to supply
#' condition metadata:
#'
#' * Supply `class` to create a classed condition. Typed
#'   conditions can be captured or handled selectively, allowing for
#'   finer-grained error handling.
#'
#' * Supply metadata with named `...` arguments. This data will be
#'   stored in the condition object and can be examined by handlers.
#'
#' `abort()` throws subclassed errors, see
#' [`"rlang_error"`][rlang_error].
#'
#' `interrupt()` allows R code to simulate a user interrupt of the
#' kind that is signalled with `Ctrl-C`. It is currently not possible
#' to create custom interrupt condition objects.
#'
#' @section Error prefix:
#'
#' As with [base::stop()], errors thrown with `abort()` are prefixed
#' with `"Error: "`. Calls and source references are included in the
#' prefix, e.g. `"Error in `my_function()` at myfile.R:1:2:"`. There
#' are a few cosmetic differences:
#'
#' - The call is stripped from its arguments to keep it simple. It is
#'   then formatted using the [cli package](https://cli.r-lib.org/) if
#'   available.
#'
#' - A line break between the prefix and the message when the former
#'   is too long. When a source location is included, a line break is
#'   always inserted.
#'
#' If your throwing code is highly structured, you may have to
#' explicitly inform `abort()` about the relevant user-facing call to
#' include in the prefix. Internal helpers are rarely relevant to end
#' users. See the `call` argument of `abort()`.
#'
#' @section Backtrace:
#'
#' `abort()` always saves a backtrace. You can print a simplified
#' backtrace of the last error by calling [last_error()] and a full
#' backtrace with `summary(last_error())`. Learn how to control what is
#' displayed when an error is thrown with [`rlang_backtrace_on_error`].
#'
#' @section Muffling and silencing conditions:
#'
#' Signalling a condition with `inform()` or `warn()` causes a message
#' to be displayed in the console. These messages can be muffled with
#' [base::suppressMessages()] or [base::suppressWarnings()].
#'
#' `inform()` and `warn()` messages can also be silenced with the
#' global options `rlib_message_verbosity` and
#' `rlib_warning_verbosity`. These options take the values:
#'
#' - `"default"`: Verbose unless the `.frequency` argument is
#'   supplied.
#' - `"verbose"`: Always verbose.
#' - `"quiet"`: Always quiet.
#'
#' When set to quiet, the message is not displayed and the condition
#' is not signalled.
#'
#' @section `stdout` and `stderr`:
#'
#' By default, `abort()` and `inform()` print to standard output in
#' interactive sessions. This allows rlang to be in control of the
#' appearance of messages in IDEs like RStudio.
#'
#' There are two situations where messages are streamed to `stderr`:
#'
#' - In non-interactive sessions, messages are streamed to standard
#'   error so that R scripts can easily filter them out from normal
#'   output by redirecting `stderr`.
#'
#' - If a sink is active (either on output or on messages) messages
#'   are always streamd to `stderr`.
#'
#' These exceptions ensure consistency of behaviour in interactive and
#' non-interactive sessions, and when sinks are active.
#'
#' @details
#'
#' - `abort()` and `warn()` temporarily set the `warning.length`
#'   global option to the maximum value (8170), unless that option has
#'   been changed from the default value. The default limit (1000
#'   characters) is especially easy to hit when the message contains a
#'   lot of ANSI escapes, as created by the crayon or cli packages
#'
#' @inheritParams cnd
#' @param message The message to display. Character vectors are
#'   formatted with [format_error_bullets()]. The first element
#'   defines a message header and the rest of the vector defines
#'   bullets. Bullets named `i` and `x` define info and error bullets
#'   respectively, with special Unicode and colour formatting applied
#'   if possible.
#'
#'   If a message is not supplied, it is expected that the message is
#'   generated lazily through [conditionMessage()][cnd_message]. In
#'   that case, `class` must be supplied. Only `inform()` allows empty
#'   messages as it is occasionally useful to build user output
#'   incrementally.
#' @param class Subclass of the condition. This allows your users
#'   to selectively handle the conditions signalled by your functions.
#' @param ... Additional data to be stored in the condition object.
#'   If you supply condition fields, you should usually provide a
#'   `class` argument. You may consider prefixing condition fields
#'   with the name of your package or organisation to avoid any
#'   conflict in case of subclassing.
#' @param call The execution environment of a currently
#'   running function, e.g. `call = caller_env()`. The function will
#'   be mentioned in error messages as the source of the error.
#'
#'   When an error occurs, the corresponding function call (see
#'   [sys.call()]) is retrieved and stored as `call` field in the
#'   error object to provide users with contextual information about
#'   the error.
#'
#'   Can also be `NULL` or a function call to respectively disable the
#'   contextual call or hard-code it.
#'
#'   See also [rlang::local_error_call()] for an alternative way of
#'   providing this information.
#' @param body Additional bullets.
#' @param use_cli_format Whether to format `message` lazily using
#'   [cli](https://cli.r-lib.org/) if available. This results in
#'   prettier and more accurate formatting of messages. See also
#'   [local_use_cli()] to set this condition field by default in your
#'   namespace.
#'
#'   The downside is that you can no longer format (assemble multiple
#'   lines into a single string with lines separated by `\\n`
#'   characters) `message` ahead of time because that would conflict
#'   with cli formatting.
#' @param .file A connection or a string specifying where to print the
#'   message. The default depends on the context, see the `stdout` vs
#'   `stderr` section.
#' @param .subclass This argument was renamed to `class` in rlang
#'   0.4.2.  It will be deprecated in the next major version. This is
#'   for consistency with our conventions for class constructors
#'   documented in <https://adv-r.hadley.nz/s3.html#s3-subclassing>.
#'
#' @examples
#' # These examples are guarded to avoid throwing errors
#' if (FALSE) {
#'
#' # Signal an error with a message just like stop():
#' abort("Something bad happened")
#'
#' # Give a class to the error:
#' abort("Something bad happened", "somepkg_bad_error")
#'
#' # This will allow your users to handle the error selectively
#' tryCatch(
#'   somepkg_function(),
#'   somepkg_bad_error = function(err) {
#'     warn(conditionMessage(err)) # Demote the error to a warning
#'     NA                          # Return an alternative value
#'   }
#' )
#'
#' # You can also specify metadata that will be stored in the condition:
#' abort("Something bad happened", "somepkg_bad_error", data = 1:10)
#'
#' # This data can then be consulted by user handlers:
#' tryCatch(
#'   somepkg_function(),
#'   somepkg_bad_error = function(err) {
#'     # Compute an alternative return value with the data:
#'     recover_error(err$data)
#'   }
#' )
#'
#' # If you call low-level APIs it is good practice to handle
#' # technical errors and rethrow them with a more meaningful
#' # message. Always prefer doing this from `withCallingHandlers()`
#' # rather than `tryCatch()` because the former preserves the stack
#' # on error and makes it possible for users to use `recover()`.
#' file <- "http://foo.bar/baz"
#' try(withCallingHandlers(
#'   download(file),
#'   error = function(err) {
#'     msg <- sprintf("Can't download `%s`", file)
#'     abort(msg, parent = err)
#' }))
#' # Note how we supplied the parent error to `abort()` as `parent` to
#' # get a decomposition of error messages across error contexts.
#'
#' # Unhandled errors are saved automatically by `abort()` and can be
#' # retrieved with `last_error()`. The error prints with a simplified
#' # backtrace:
#' abort("Saved error?")
#' last_error()
#'
#' # Use `summary()` to print the full backtrace and the condition fields:
#' summary(last_error())
#'
#' }
#' @export
abort <- function(message = NULL,
                  class = NULL,
                  ...,
                  call,
                  body = NULL,
                  trace = NULL,
                  parent = NULL,
                  use_cli_format = NULL,
                  .file = NULL,
                  .subclass = deprecated()) {
  if (is_null(trace) && is_null(peek_option("rlang:::disable_trace_capture"))) {
    # Prevents infloops when rlang throws during trace capture
    with_options("rlang:::disable_trace_capture" = TRUE, {
      trace <- trace_back()

      # Remove throwing context. Especially important when rethrowing
      # from a condition handler because in that case there are a
      # bunch of irrelevant frames in the trailing branch of the call
      # stack. We want to display the call tree branch that is
      # relevant to users in simplified backtraces.
      if (is_null(parent)) {
        context <- trace_length(trace)
      } else {
        context <- trace_capture_depth(trace)
      }

      trace <- trace_trim_context(trace, context)
    })
  }

  message <- validate_signal_args(message, class, call, .subclass)
  message_info <- cnd_message_info(message, body, caller_env(), use_cli_format = use_cli_format)
  message <- message_info$message
  extra_fields <- message_info$extra_fields

  # Don't record call by default when supplied a parent because it
  # probably means that we are called from a condition handler
  if (is_missing(call)) {
    if (is_null(parent)) {
      call <- default_error_call(caller_env())
    } else {
      call <- NULL
    }
  } else {
    call <- error_call(call)
  }

  cnd <- error_cnd(
    class,
    ...,
    message = message,
    !!!extra_fields,
    call = call,
    parent = parent,
    trace = trace
  )
  signal_abort(cnd, .file)
}

cnd_message_info <- function(message,
                             body,
                             env,
                             cli_opts = use_cli(env),
                             use_cli_format = NULL) {
  if (!is_null(use_cli_format)) {
    cli_opts[["format"]] <- use_cli_format
  }

  fields <- list()

  if (cli_opts[["inline"]]) {
    message[] <- map_chr(message, cli::format_inline, .envir = env)
  }

  # Formatting with cli is delayed until print time so we can properly
  # indent and width-wrap depending on the context
  if (cli_opts[["format"]]) {
    fields$use_cli_format <- TRUE
    fields$body <- c(message[-1], body)
    message <- message[1]
  } else {
    # Compatibility with older bullets formatting
    if (is_null(names(message)) && length(message) > 1) {
      names(message) <- c("", rep_len("*", length(message) - 1))
    }
    message <- .rlang_cli_format_fallback(c(message, body))
  }

  list(message = message, extra_fields = fields)
}


#' Use cli to format error messages
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `local_use_cli()` marks a package namespace or the environment of a
#' running function with a special flag that instructs [abort()] to
#' use cli to format error messages. This formatting happens lazily,
#' at print-time, in various places:
#'
#' - When an unexpected error is displayed to the user.
#' - When a captured error is printed in the console, for instance via
#'   [last_error()].
#' - When [conditionMessage()] is called.
#'
#' cli formats messages and bullets with indentation and
#' width-wrapping to produce a polished display of messages.
#'
#' @inheritParams args_dots_empty
#' @param format Whether to use cli at print-time to format messages
#'   and bullets.
#' @param inline `r lifecycle::badge("experimental")` Whether to use
#'   cli at throw-time to format the inline parts of a message. This
#'   makes it possible to use cli interpolation and formatting with
#'   `abort()`.
#' @param frame A package namespace or an environment of a running
#'   function.
#'
#' @section Usage:
#'
#' To use cli formatting automatically in your package:
#'
#' 1. Make sure [run_on_load()] is called from your `.onLoad()` hook.
#'
#' 2. Call `on_load(local_use_cli())` at the top level of your namespace.
#'
#' It is also possible to call `local_use_cli()` inside a running
#' function, in which case the flag only applies within that function.
#'
#' @keywords internal
#' @export
local_use_cli <- function(...,
                          format = TRUE,
                          inline = FALSE,
                          frame = caller_env()) {
  check_dots_empty0(...)

  use_cli <- c(format = format, inline = inline)

  if (is_namespace(frame)) {
    frame$.__rlang_use_cli__. <- use_cli
  } else {
    local_bindings(.__rlang_use_cli__. = use_cli, .frame = frame)
  }

  invisible(NULL)
}

use_cli <- function(env) {
  # Internal option to disable cli in case of recursive errors
  if (is_true(peek_option("rlang:::disable_cli"))) {
    return(FALSE)
  }

  # Formatting with cli is opt-in
  default <- c(format = FALSE, inline = FALSE)

  last <- topenv(env)

  # Search across load-all'd environments
  if (identical(last, global_env()) && "devtools_shims" %in% search()) {
    last <- empty_env()
  }

  flag <- env_get(
    env,
    ".__rlang_use_cli__.",
    default = default,
    inherit = TRUE,
    last = last
  )

  local_error_call("caller")
  check_use_cli_flag(flag)

  flag
}

# Makes sure `inline` can't be set without `format`. Formatting with
# cli is optional. If cli is not installed or too old, the rlang
# fallback formatting is used. On the other hand, formatting inline
# parts with cli requires a recent version of cli to be installed.
check_use_cli_flag <- function(flag) {
  local_error_call("caller")

  if (!is_logical(flag) || !identical(names(flag), c("format", "inline")) || anyNA(flag)) {
    abort("`.__rlang_use_cli__.` has unknown format.")
  }

  if (flag[["inline"]]) {
    if (!has_cli_format || !has_cli_inline) {
      with_options(
        "rlang:::disable_cli" = TRUE,
        abort(c(
          "`.__rlang_use_cli__.[[\"inline\"]]` is set to `TRUE` but cli is not installed or is too old.",
          "i" = "The package author should add a recent version of `cli` to their `Imports`."
        ))
      )
    }

    if (!flag[["format"]]) {
      abort("Can't use cli inline formatting without cli bullets formatting.")
    }
  }
}

# Private option to disable default error call except for exported
# functions
default_error_call <- function(env) {
  call <- error_call(env)

  opt <- peek_option("rlang:::restrict_default_error_call") %||% FALSE
  if (is_false(opt)) {
    return(call)
  }

  if (!is_call(call)) {
    return(call)
  }

  fn <- call[[1]]
  if (!is_symbol(fn)) {
    return(call)
  }
  fn <- as_string(fn)

  top <- topenv(env)
  if (!is_namespace(top)) {
    return(NULL)
  }
  if (!ns_exports_has(top, fn)) {
    return(NULL)
  }

  call
}

signal_abort <- function(cnd, file = NULL) {
  if (is_true(peek_option("rlang::::force_unhandled_error"))) {
    # Fall back with the full rlang error
    fallback <- cnd
  } else {
    # Let exiting and calling handlers handle the fully typed
    # condition. The error message hasn't been altered yet and won't
    # affect handling functions like `try()`.
    signalCondition(cnd)

    # If we're still here, the error is unhandled. Fall back with a
    # bare condition to avoid calling handlers logging the same error
    # twice
    fallback <- cnd
    class(fallback) <- c("rlang_error", "condition")
    fallback$message <- ""
    fallback$rlang$internal$entraced <- TRUE
  }

  # Save the unhandled error for `rlang::last_error()`.
  poke_last_error(cnd)

  # Include backtrace footer option in the condition
  cnd <- cnd_set_backtrace_on_error(cnd, peek_backtrace_on_error())

  # Print the error manually. This allows us to use our own style,
  # include parent errors, and work around limitations on the length
  # of error messages (#856).
  msg <- cnd_message(cnd, inherit = TRUE, prefix = TRUE)

  cat_line(msg, file = file %||% default_message_file())

  # Use `stop()` to run the `getOption("error")` handler (used by
  # RStudio to record a backtrace) and cause a long jump. Running the
  # handler manually wouldn't work because it might (and in RStudio's
  # case, it does) call `geterrmessage()`. Turn off the regular error
  # printing to avoid printing the error twice.
  local_options(show.error.messages = FALSE)
  stop(fallback)
}

#' Set local error call in an execution environment
#'
#' `local_error_call()` is an alternative to explicitly passing a
#' `call` argument to [abort()]. It sets the call (or a value that
#' indicates where to find the call, see below) in a local binding
#' that is automatically picked up by [abort()].
#'
#' @param call This can be:
#'
#'   - A call to be used as context for an error thrown in that
#'     execution environment.
#'
#'   - The `NULL` value to show no context.
#'
#'   - An execution environment, e.g. as returned by [caller_env()].
#'     The [sys.call()] for that environment is taken as context.
#' @param frame The execution environment in which to set the local
#'   error call.
#'
#' @section Motivation for setting local error calls:
#'
#' By default [abort()] uses the function call of its caller as
#' context in error messages:
#'
#' ```
#' foo <- function() abort("Uh oh.")
#' foo()
#' #> Error in `foo()`: Uh oh.
#' ```
#'
#' This is not always appropriate. For example a function that checks
#' an input on the behalf of another function should reference the
#' latter, not the former:
#'
#' ```
#' arg_check <- function(arg,
#'                       error_arg = as_string(substitute(arg))) {
#'   abort(cli::format_error("{.arg {error_arg}} is failing."))
#' }
#'
#' foo <- function(x) arg_check(x)
#' foo()
#' #> Error in `arg_check()`: `x` is failing.
#' ```
#'
#' The mismatch is clear in the example above. `arg_check()` does not
#' have any `x` argument and so it is confusing to present
#' `arg_check()` as being the relevant context for the failure of the
#' `x` argument.
#'
#' One way around this is to take a `call` or `error_call` argument
#' and pass it to `abort()`. Here we name this argument `error_call`
#' for consistency with `error_arg` which is prefixed because there is
#' an existing `arg` argument. In other situations, taking `arg` and
#' `call` arguments might be appropriate.
#'
#' ```
#' arg_check <- function(arg,
#'                       error_arg = as_string(substitute(arg)),
#'                       error_call = caller_env()) {
#'   abort(
#'     cli::format_error("{.arg {error_arg}} is failing."),
#'     call = error_call
#'   )
#' }
#'
#' foo <- function(x) arg_check(x)
#' foo()
#' #> Error in `foo()`: `x` is failing.
#' ```
#'
#' This is the generally recommended pattern for argument checking
#' functions. If you mention an argument in an error message, provide
#' your callers a way to supply a different argument name and a
#' different error call. `abort()` stores the error call in the `call`
#' condition field which is then used to generate the "in" part of
#' error messages.
#'
#' In more complex cases it's often burdensome to pass the relevant
#' call around, for instance if your checking and throwing code is
#' structured into many different functions. In this case, use
#' `local_error_call()` to set the call locally or instruct `abort()`
#' to climb the call stack one level to find the relevant call. In the
#' following example, the complexity is not so important that sparing
#' the argument passing makes a big difference. However this
#' illustrates the pattern:
#'
#' ```
#' arg_check <- function(arg,
#'                       error_arg = caller_arg(arg),
#'                       error_call = caller_env()) {
#'   # Set the local error call
#'   local_error_call(error_call)
#'
#'   my_classed_stop(
#'     cli::format_error("{.arg {error_arg}} is failing.")
#'   )
#' }
#'
#' my_classed_stop <- function(message) {
#'   # Forward the local error call to the caller's
#'   local_error_call(caller_env())
#'
#'   abort(message, class = "my_class")
#' }
#'
#' foo <- function(x) arg_check(x)
#' foo()
#' #> Error in `foo()`: `x` is failing.
#' ```
#'
#' @section Error call flags in performance-critical functions:
#'
#' The `call` argument can also be the string `"caller"`. This is
#' equivalent to `caller_env()` or `parent.frame()` but has a lower
#' overhead because call stack introspection is only performed when an
#' error is triggered. Note that eagerly calling `caller_env()` is
#' fast enough in almost all cases.
#'
#' If your function needs to be really fast, assign the error call
#' flag directly instead of calling `local_error_call()`:
#'
#' ```
#' .__error_call__. <- "caller"
#' ```
#'
#' @examples
#' # Set a context for error messages
#' function() {
#'   local_error_call(quote(foo()))
#'   local_error_call(sys.call())
#' }
#'
#' # Disable the context
#' function() {
#'   local_error_call(NULL)
#' }
#'
#' # Use the caller's context
#' function() {
#'   local_error_call(caller_env())
#' }
#' @export
local_error_call <- function(call, frame = caller_env()) {
  # This doesn't implement the semantics of a `local_` function
  # perfectly in order to be as fast as possible
  frame$.__error_call__. <- call
  invisible(NULL)
}

#' Documentation anchor for error arguments
#'
#' @description
#'
#' Use `@inheritParams rlang::args_error_context` in your package to
#' document `arg` and `call` arguments (or equivalently their prefixed
#' versions `error_arg` and `error_call`).
#'
#' - `arg` parameters should be formatted as argument (e.g. using
#'   cli's `.arg` specifier) and included in error messages. See also
#'   [caller_arg()].
#'
#' - `call` parameters should be included in error conditions in a
#'   field named `call`. An easy way to do this is by passing a `call`
#'   argument to [abort()]. See also [local_error_call()].
#'
#' @param arg,error_arg An argument name as a string. This argument
#'   will be mentioned in error messages as the input that is at the
#'   origin of a problem.
#' @param call,error_call The execution environment of a currently
#'   running function, e.g. `caller_env()`. The function will be
#'   mentioned in error messages as the source of the error. See the
#'   `call` argument of [rlang::abort()] for more information.
#'
#' @name args_error_context
NULL

#' Find the caller argument for error messages
#'
#' @description
#'
#' `caller_arg()` is a variant of `substitute()` or [ensym()] for
#' arguments that reference other arguments. Unlike `substitute()`
#' which returns an expression, `caller_arg()` formats the expression
#' as a single line string which can be included in error messages.
#'
#' - When included in an error message, the resulting label should
#'   generally be formatted as argument, for instance using the `.arg`
#'   in the cli package.
#'
#' - Use `@inheritParams rlang::args_error_context` to document an
#'   `arg` or `error_arg` argument that takes `error_arg()` as default.
#'
#' @param arg An argument name in the current function.
#' @usage NULL
#'
#' @examples
#' arg_checker <- function(x, arg = caller_arg(x), call = caller_env()) {
#'   cli::cli_abort("{.arg arg} must be a thingy.", call = call)
#' }
#'
#' my_function <- function(my_arg) {
#'   arg_checker(my_arg)
#' }
#'
#' try(my_function(NULL))
#' @export
caller_arg <- function(arg) {
  arg <- substitute(arg)
  if (!is_symbol(arg)) {
    abort(sprintf(
      "%s must be an argument name.",
      format_arg("arg")
    ))
  }

  expr <- do.call(substitute, list(arg), envir = caller_env())
  as_label(expr)
}

#' Validate and format a function call for use in error messages
#'
#' @description
#'
#' `format_error_call()` simplifies its input to a simple call (see
#' section below) and formats the result as code (using cli if
#' available). Use this function to generate the "in" part
#' of an error message from a stack frame call.
#'
#' If passed an environment, the corresponding `sys.call()` is taken
#' as call, unless there is a local flag (see [local_error_call()]).
#'
#' @section Details of formatting:
#'
#' - The arguments of function calls are stripped.
#'
#' - Complex function calls containing inlined objects return
#'   `NULL`.
#'
#' - Calls to `if` preserve the condition since it might be
#'   informative. Branches are dropped.
#'
#' - Calls to operators and other special syntax are formatted using
#'   their names rather than the potentially confusing function form.
#'
#' @inheritParams args_error_context
#' @return Either a string formatted as code or `NULL` if a simple
#'   call could not be generated.
#'
#' @keywords internal
#'
#' @examples
#' # Arguments are stripped
#' writeLines(format_error_call(quote(foo(bar, baz))))
#'
#' # Returns `NULL` with complex calls such as those that contain
#' # inlined functions
#' format_error_call(call2(list))
#'
#' # Operators are formatted using their names rather than in
#' # function call form
#' writeLines(format_error_call(quote(1 + 2)))
#' @export
format_error_call <- function(call) {
  call <- error_call(call)
  if (is_null(call)) {
    return(NULL)
  }

  label <- error_call_as_string(call)
  if (is_null(label)) {
    return(NULL)
  }

  format_code(label)
}

error_call_as_string <- function(call) {
  if (!is_call(call)) {
    return(NULL)
  }

  if (inherits(call, "AsIs")) {
    call <- expr_deparse(unclass(call))
    if (length(call) == 1) {
      return(call)
    } else {
      return(NULL)
    }
  }

  # Functions that forward their error context to their caller
  # shouldn't generally be called via NSE but there are exceptions,
  # such as testthat snapshots.
  #
  # - `do.call()` or `eval_bare()` shouldn't generally cause issues. If
  #   the environment exists on the stack, we find its `sys.call()`. If
  #   it doesn't exist, taking its `sys.call()` returns `NULL` which
  #   disables the error context.
  #
  # - On the other hand, `eval()` always creates a specific frame for
  #   all environments and the `sys.call()` for that frame is `eval()`.
  #   It wouldn't be useful to display this as the context so calls to
  #   `eval()` and `evalq()` are replaced by `NULL`.
  if (is_call(call, c("eval", "evalq"))) {
    return(NULL)
  }

  if (!is_call_simple(call)) {
    return(NULL)
  }

  # Remove namespace for now to simplify conversion
  old <- call[[1]]
  call[[1]] <- sym(call_name(call))

  # Deal with special-syntax calls. `if` carries useful information in
  # its call. For other operators we just return their name because
  # the functional form may be confusing.
  if (is_call(call, "if")) {
    # Deal with `if` bombs. Keep the condition as it is informative but
    # drop the branches to avoid multiline calls. See
    # https://github.com/r-lib/testthat/issues/1429
    call[[3]] <- quote(...)
    return(as_label(call[1:3]))
  }
  if (!is_string(call_parse_type(call), "")) {
    return(as_string(call[[1]]))
  }

  if (is_symbol(call[[1]]) && needs_backticks(call[[1]])) {
    return(as_string(call[[1]]))
  }

  # Remove distracting arguments from the call and restore namespace
  call[[1]] <- old
  as_label(call[1])
}

error_call <- function(call) {
  while (is_environment(call)) {
    flag <- error_flag(call)

    if (is_null(flag) || is_call(flag)) {
      call <- flag
      break
    }

    if (is_environment(flag)) {
      call <- flag
      next
    }

    if (is_string(flag, "caller")) {
      call <- eval_bare(call2(caller_env), call)
      next
    }

    # Replace `f.foo(...)` calls by `f(...)`
    if (is_string(gen <- call$.Generic)) {
      # Climb methods frames to find the generic call. This call
      # carries the relevant srcref.
      frames <- sys.frames()
      i <- detect_index(frames, identical, call, .right = TRUE)

      while (i > 1) {
        i <- i - 1
        prev <- frames[[i]]

        if (is_call(caller_call(prev), "NextMethod")) {
          next
        }

        if (identical(prev$.Generic, gen)) {
          next
        }

        # Recurse in case there is an error flag in a dispatch helper
        return(error_call(prev))
      }
    }

    call <- caller_call(call)
    break
  }

  if (!is_call(call)) {
    return(NULL)
  }

  call
}

call_restore <- function(x, to) {
  attr(x, "srcref") <- attr(to, "srcref")
  x
}

error_flag <- function(env, top = topenv(env)) {
  env_get(
    env,
    ".__error_call__.",
    default = TRUE,
    inherit = FALSE,
    last = top
  )
}

trace_trim_context <- function(trace, idx) {
  if (!is_scalar_integerish(idx)) {
    abort("`frame` must be a frame environment or index")
  }

  to_trim <- seq2(idx, trace_length(trace))
  if (length(to_trim)) {
    trace <- trace_slice(trace, -to_trim)
  }

  trace
}

# Assumes we're called from a calling or exiting handler
trace_capture_depth <- function(trace) {
  calls <- trace$call
  default <- length(calls)

  if (length(calls) <= 3L) {
    return(default)
  }

  depth <- trace_depth_wch(trace)
  if (!is_null(depth)) {
    return(depth)
  }

  depth <- trace_depth_trycatch(trace)
  if (!is_null(depth)) {
    return(depth)
  }

  default
}

trace_depth_wch <- function(trace) {
  calls <- trace$call
  parents <- trace$parent

  # GNU R currently structures evaluation of calling handlers as
  # called from the global env. We find the first call with 0 as
  # parent to skip all potential wrappers of the rethrowing handler.
  top <- which(parents == 0)
  top <- top[length(top)]
  if (!length(top)) {
    return(NULL)
  }
  if (top <= 2) {
    return(NULL)
  }

  # withCallingHandlers() - C level error case
  if (length(calls) > top + 1 &&
      identical(calls[[top + 1]], quote(h(simpleError(msg, call))))) {
    return(top)
  }

  # withCallingHandlers() - `abort()` case
  wch_calls <- calls[seq2(top - 2L, top - 0L)]
  if (!is_call(wch_calls[[1]], "signal_abort") ||
      !is_call(wch_calls[[2]], "signalCondition") ||
      !is_call(wch_calls[[3]]) && is_function(wch_calls[[3]][[1]])) {
    return(NULL)
  }

  top - 3L
}

trace_depth_trycatch <- function(trace) {
  calls <- trace$call

  exiting_call <- quote(value[[3L]](cond))
  exiting_n <- detect_index(calls, identical, exiting_call, .right = TRUE)

  if (exiting_n != 0L) {
    try_catch_calls <- calls[seq_len(exiting_n - 1L)]
    try_catch_n <- detect_index(try_catch_calls, is_call, "tryCatch", .right = TRUE)
    if (try_catch_n != 0L) {
      return(try_catch_n)
    }
  }

  NULL
}

#' Display backtrace on error
#'
#' @description
#' rlang errors carry a backtrace that can be inspected by calling
#' [last_error()]. You can also control the default display of the
#' backtrace by setting the option `rlang_backtrace_on_error` to one
#' of the following values:
#'
#' * `"none"` show nothing.
#' * `"reminder"`, the default in interactive sessions, displays a reminder that
#'   you can see the backtrace with [rlang::last_error()].
#' * `"branch"` displays a simplified backtrace.
#' * `"collapse"` displays a collapsed backtrace tree.
#' * `"full"`, the default in non-interactive sessions, displays the full tree.
#'
#' rlang errors are normally thrown with [abort()]. If you promote
#' base errors to rlang errors with [global_entrace()],
#' `rlang_backtrace_on_error` applies to all errors.
#'
#' @section Promote base errors to rlang errors:
#'
#' You can use `options(error = rlang::entrace)` to promote base errors to
#' rlang errors. This does two things:
#'
#' * It saves the base error as an rlang object so you can call [last_error()]
#'   to print the backtrace or inspect its data.
#'
#' * It prints the backtrace for the current error according to the
#'   `rlang_backtrace_on_error` option.
#'
#' @section Errors in RMarkdown:
#'
#' The display of errors depends on whether they're expected (i.e.
#' chunk option `error = TRUE`) or unexpected:
#'
#' * Expected errors are controlled by the global option
#'   `"rlang_backtrace_on_error_report"` (note the `_report` suffix).
#'   The default is `"none"` so that your expected errors don't
#'   include a reminder to run `rlang::last_error()`. Customise this
#'   option if you want to demonstrate what the error backtrace will
#'   look like.
#'
#'   You can also use [last_error()] to display the trace like you
#'   would in your session, but it currently only works in the next
#'   chunk.
#'
#' * Unexpected errors are controlled by the global option
#'   `"rlang_backtrace_on_error"`. The default is `"branch"` so you'll
#'   see a simplified backtrace in the knitr output to help you figure
#'   out what went wrong.
#'
#' When knitr is running (as determined by the `knitr.in.progress`
#' global option), the default top environment for backtraces is set
#' to the chunk environment `knitr::knit_global()`. This ensures that
#' the part of the call stack belonging to knitr does not end up in
#' backtraces. If needed, you can override this by setting the
#' `rlang_trace_top_env` global option.
#'
#' @name rlang_backtrace_on_error
#' @aliases add_backtrace
#'
#' @examples
#' # Display a simplified backtrace on error for both base and rlang
#' # errors:
#'
#' # options(
#' #   rlang_backtrace_on_error = "branch",
#' #   error = rlang::entrace
#' # )
#' # stop("foo")
NULL

backtrace_on_error_opts <- c("none", "reminder", "branch", "collapse", "full")

# Whenever the backtrace-on-error format is changed, the version in
# `inst/backtrace-ver` and in `tests/testthat/helper-rlang.R` must be
# bumped. This way `devtools::test()` will skip the tests that require
# the dev version to be installed locally.
format_onerror_backtrace <- function(cnd, opt = peek_backtrace_on_error()) {
  opt <- arg_match0(opt, backtrace_on_error_opts, "backtrace_on_error")

  if (opt == "none") {
    return(NULL)
  }

  trace <- cnd$trace

  # Show backtrace of oldest parent
  while (is_condition(cnd$parent)) {
    cnd <- cnd$parent
    if (!is_null(cnd$trace)) {
      trace <- cnd$trace
    }
  }

  if (is_null(trace) || !trace_length(trace)) {
    return(NULL)
  }

  # Should come after trace length check so that we don't display a
  # reminder when there is no trace to display
  if (opt == "reminder") {
    if (is_interactive()) {
      reminder <- silver("Run `rlang::last_error()` to see where the error occurred.")
    } else {
      reminder <- NULL
    }
    return(reminder)
  }

  if (opt == "branch") {
    max_frames <- 10L
  } else {
    max_frames <- NULL
  }

  simplify <- switch(
    opt,
    full = "none",
    reminder = "branch", # Check size of backtrace branch
    opt
  )

  paste_line(
    "Backtrace:",
    format(trace, simplify = simplify, max_frames = max_frames)
  )
}

peek_backtrace_on_error <- function() {
  opt <- peek_backtrace_on_error_opt("rlang_backtrace_on_error")
  if (!is_null(opt)) {
    return(opt)
  }

  if (report_in_progress()) {
    "branch"
  } else if (is_interactive()) {
    "reminder"
  } else {
    "full"
  }
}

# By default, we display no reminder or backtrace for errors captured
# by knitr
peek_backtrace_on_error_report <- function() {
  peek_backtrace_on_error_opt("rlang_backtrace_on_error_report") %||% "none"
}

peek_backtrace_on_error_opt <- function(name) {
  opt <- peek_option(name)

  if (!is_null(opt) && !is_string(opt, backtrace_on_error_opts)) {
    options(list2("{name}" := NULL))
    warn(c(
      sprintf("Invalid %s option.", format_arg(name)),
      i = "The option was just reset to `NULL`."
    ))
    return(NULL)
  }

  opt
}
