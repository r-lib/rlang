#' Signal an error, warning, or message
#'
#' @description
#' These functions are equivalent to base functions [base::stop()],
#' [base::warning()], and [base::message()]. They signal a condition
#' (an error, warning, or message respectively) and make it easy to
#' supply condition metadata:
#'
#' * Supply `class` to create a classed condition that can be caught
#'   or handled selectively, allowing for finer-grained error
#'   handling.
#'
#' * Supply metadata with named `...` arguments. This data is stored
#'   in the condition object and can be examined by handlers.
#'
#' * Supply `call` to inform users about which function the error
#'   occurred in.
#'
#' * Supply another condition as `parent` to create a [chained
#'   condition][topic-error-chaining].
#'
#' Certain components of condition messages are formatted with unicode
#' symbols and terminal colours by default. These aspects can be
#' customised, see `r link("topic_condition_customisation")`.
#'
#' @inheritParams cnd
#' @param message The message to display, formatted as a __bulleted
#'   list__. The first element is displayed as an _alert_ bullet
#'   prefixed with `!` by default. Elements named `"*"`, `"i"`, `"v"`,
#'   `"x"`, and `"!"` are formatted as regular, info, success,
#'   failure, and error bullets respectively. See `r link("topic_condition_formatting")`
#'   for more about bulleted messaging.
#'
#'   If a message is not supplied, it is expected that the message is
#'   generated __lazily__ through [cnd_header()] and [cnd_body()]
#'   methods. In that case, `class` must be supplied. Only `inform()`
#'   allows empty messages as it is occasionally useful to build user
#'   output incrementally.
#'
#'   If a function, it is stored in the `header` field of the error
#'   condition. This acts as a [cnd_header()] method that is invoked
#'   lazily when the error message is displayed.
#' @param class Subclass of the condition.
#' @param ... Additional data to be stored in the condition object.
#'   If you supply condition fields, you should usually provide a
#'   `class` argument. You may consider prefixing condition fields
#'   with the name of your package or organisation to prevent name
#'   collisions.
#' @param body,footer Additional bullets.
#' @param call The execution environment of a currently running
#'   function, e.g. `call = caller_env()`. The corresponding function
#'   call is retrieved and mentioned in error messages as the source
#'   of the error.
#'
#'   You only need to supply `call` when throwing a condition from a
#'   helper function which wouldn't be relevant to mention in the
#'   message.
#'
#'   Can also be `NULL` or a [defused function call][topic-defuse] to
#'   respectively not display any call or hard-code a code to display.
#'
#'   For more information about error calls, see `r link("topic_error_call")`.
#' @param parent Supply `parent` when you rethrow an error from a
#'   condition handler (e.g. with [try_fetch()]).
#'
#'   - If `parent` is a condition object, a _chained error_ is
#'     created, which is useful when you want to enhance an error with
#'     more details, while still retaining the original information.
#'
#'   - If `parent` is `NA`, it indicates an unchained rethrow, which
#'     is useful when you want to take ownership over an error and
#'     rethrow it with a custom message that better fits the
#'     surrounding context.
#'
#'     Technically, supplying `NA` lets `abort()` know it is called
#'     from a condition handler. This helps it create simpler
#'     backtraces where the condition handling context is hidden by
#'     default.
#'
#'   For more information about error calls, see `r link("topic_error_chaining")`.
#' @param use_cli_format Whether to format `message` lazily using
#'   [cli](https://cli.r-lib.org/) if available. This results in
#'   prettier and more accurate formatting of messages. See
#'   [local_use_cli()] to set this condition field by default in your
#'   package namespace.
#'
#'   If set to `TRUE`, `message` should be a character vector of
#'   individual and unformatted lines. Any newline character `"\\n"`
#'   already present in `message` is reformatted by cli's paragraph
#'   formatter. See `r link("topic_condition_formatting")`.
#' @param .inherit Whether the condition inherits from `parent`
#'   according to [cnd_inherits()] and [try_fetch()]. By default,
#'   parent conditions of higher severity are not inherited. For
#'   instance an error chained to a warning is not inherited to avoid
#'   unexpectedly catching an error downgraded to a warning.
#' @param .internal If `TRUE`, a footer bullet is added to `message`
#'   to let the user know that the error is internal and that they
#'   should report it to the package authors. This argument is
#'   incompatible with `footer`.
#' @param .file A connection or a string specifying where to print the
#'   message. The default depends on the context, see the `stdout` vs
#'   `stderr` section.
#' @param .frame The throwing context. Used as default for
#'   `.trace_bottom`, and to determine the internal package to mention
#'   in internal errors when `.internal` is `TRUE`.
#' @param .trace_bottom Used in the display of simplified backtraces
#'   as the last relevant call frame to show. This way, the irrelevant
#'   parts of backtraces corresponding to condition handling
#'   ([tryCatch()], [try_fetch()], `abort()`, etc.) are hidden by
#'   default. Defaults to `call` if it is an environment, or `.frame`
#'   otherwise. Without effect if `trace` is supplied.
#' @param .subclass `r lifecycle::badge("deprecated")` This argument
#'   was renamed to `class` in rlang 0.4.2 for consistency with our
#'   conventions for class constructors documented in
#'   <https://adv-r.hadley.nz/s3.html#s3-subclassing>.
#'
#' @section Error prefix:
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
#' `abort()` saves a backtrace in the `trace` component of the error
#' condition. You can print a simplified backtrace of the last error
#' by calling [last_error()] and a full backtrace with
#' `summary(last_error())`. Learn how to control what is displayed
#' when an error is thrown with [`rlang_backtrace_on_error`].
#'
#' @section Muffling and silencing conditions:
#' Signalling a condition with `inform()` or `warn()` displays a
#' message in the console. These messages can be muffled as usual with
#' [base::suppressMessages()] or [base::suppressWarnings()].
#'
#' `inform()` and `warn()` messages can also be silenced with the
#' global options `rlib_message_verbosity` and
#' `rlib_warning_verbosity`. These options take the values:
#'
#' - `"default"`: Verbose unless the `.frequency` argument is supplied.
#' - `"verbose"`: Always verbose.
#' - `"quiet"`: Always quiet.
#'
#' When set to quiet, the message is not displayed and the condition
#' is not signalled.
#'
#' @section `stdout` and `stderr`:
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
#' - `abort()` throws subclassed errors, see
#'   [`"rlang_error"`][rlang_error].
#'
#' - `warn()` temporarily set the `warning.length` global option to
#'   the maximum value (8170), unless that option has been changed
#'   from the default value. The default limit (1000 characters) is
#'   especially easy to hit when the message contains a lot of ANSI
#'   escapes, as created by the crayon or cli packages
#'
#' @seealso
#' - `r link("topic_error_call")`
#' - `r link("topic_error_chaining")`
#'
#' @examples
#' # These examples are guarded to avoid throwing errors
#' if (FALSE) {
#'
#' # Signal an error with a message just like stop():
#' abort("The error message.")
#'
#'
#' # Unhandled errors are saved automatically by `abort()` and can be
#' # retrieved with `last_error()`. The error prints with a simplified
#' # backtrace:
#' f <- function() try(g())
#' g <- function() evalq(h())
#' h <- function() abort("Tilt.")
#' last_error()
#'
#' # Use `summary()` to print the full backtrace and the condition fields:
#' summary(last_error())
#'
#'
#' # Give a class to the error:
#' abort("The error message", "mypkg_bad_error")
#'
#' # This allows callers to handle the error selectively
#' tryCatch(
#'   mypkg_function(),
#'   mypkg_bad_error = function(err) {
#'     warn(conditionMessage(err)) # Demote the error to a warning
#'     NA                          # Return an alternative value
#'   }
#' )
#'
#' # You can also specify metadata that will be stored in the condition:
#' abort("The error message.", "mypkg_bad_error", data = 1:10)
#'
#' # This data can then be consulted by user handlers:
#' tryCatch(
#'   mypkg_function(),
#'   mypkg_bad_error = function(err) {
#'     # Compute an alternative return value with the data:
#'     recover_error(err$data)
#'   }
#' )
#'
#'
#' # If you call low-level APIs it may be a good idea to create a
#' # chained error with the low-level error wrapped in a more
#' # user-friendly error. Use `try_fetch()` to fetch errors of a given
#' # class and rethrow them with the `parent` argument of `abort()`:
#' file <- "http://foo.bar/baz"
#' try(
#'   try_fetch(
#'     download(file),
#'     error = function(err) {
#'       msg <- sprintf("Can't download `%s`", file)
#'       abort(msg, parent = err)
#'   })
#' )
#'
#' }
#' @export
abort <- function(message = NULL,
                  class = NULL,
                  ...,
                  call,
                  body = NULL,
                  footer = NULL,
                  trace = NULL,
                  parent = NULL,
                  use_cli_format = NULL,
                  .inherit = TRUE,
                  .internal = FALSE,
                  .file = NULL,
                  .frame = caller_env(),
                  .trace_bottom = NULL,
                  .subclass = deprecated()) {
  check_environment(.frame)

  .__signal_frame__. <- TRUE

  rethrowing <- !is_null(parent)
  if (is_na(parent)) {
    parent <- NULL
  }

  if (is_list(maybe_missing(call))) {
    if (!identical(names(call), c("call", "frame")) &&
        !identical(names(call), c("", "frame"))) {
      abort("When a list, `call` must have \"call\" and \"frame\" names.")
    }
    .frame <- call[["frame"]] %||% .frame
    call <- call[["call"]]
  }

  # `.frame` is used to soft-truncate the backtrace
  if (is_null(.trace_bottom)) {
    if (rethrowing) {
      .trace_bottom <- .frame
    } else {
      # Truncate backtrace up to `call` if it is a frame
      if (is_environment(maybe_missing(call))) {
        .trace_bottom <- call
      } else {
        .trace_bottom <- .frame
      }
    }
  } else {
    check_environment(.trace_bottom)
  }

  info <- abort_context(.trace_bottom, rethrowing, maybe_missing(call))

  if (is_missing(call)) {
    if (is_null(info$from_handler)) {
      call <- .frame
    } else {
      call <- info$setup_caller
    }
  } else if (rethrowing && identical(call, info$handler_frame)) {
    call <- info$setup_caller
  }

  if (is_formula(message, scoped = TRUE, lhs = FALSE)) {
    message <- as_function(message)
  }

  message <- validate_signal_args(message, class, call, .subclass, "abort")
  error_call <- error_call(call)

  message_info <- cnd_message_info(
    message,
    body,
    footer,
    .frame,
    use_cli_format = use_cli_format,
    internal = .internal
  )
  message <- message_info$message
  extra_fields <- message_info$extra_fields
  use_cli_format <- message_info$use_cli_format

  extra_fields$rlang <- c(
    extra_fields$rlang,
    list(inherit = .inherit)
  )

  parent_trace <- if (rethrowing) parent[["trace"]]

  if (!is_null(parent_trace) && is_environment(call)) {
    calls <- sys.calls()
    frames <- sys.frames()

    loc_frame <- detect_index(frames, identical, call, .right = TRUE)
    if (loc_frame && loc_frame <= nrow(parent_trace)) {
      parent_call <- parent_trace[["call"]][[loc_frame]]
      this_call <- frame_call(call)

      if (identical(parent_call, this_call)) {
        if (is_null(parent_trace[["error_frame"]])) {
          parent_trace[["error_frame"]] <- FALSE
        }
        parent_trace[["error_frame"]][[loc_frame]] <- TRUE
        parent$trace <- parent_trace
      }
    }
  }

  cnd <- error_cnd(
    class,
    ...,
    message = message,
    !!!extra_fields,
    use_cli_format = use_cli_format,
    call = error_call,
    parent = parent
  )

  if (is_null(trace) && is_null(parent_trace) && is_null(peek_option("rlang:::disable_trace_capture"))) {
    with_options(
      # Prevents infloops when rlang throws during trace capture
      "rlang:::disable_trace_capture" = TRUE,
      "rlang:::visible_bottom" = info$bottom_frame,
      "rlang:::error_frame" = if (is_environment(call)) call else NULL,
      "rlang:::error_arg" = cnd[["arg"]],
      { trace <- trace_back() }
    )
  }
  cnd$trace <- trace

  signal_abort(cnd, .file)
}

abort_context <- function(frame,
                          rethrowing,
                          abort_call,
                          call = caller_env()) {
  calls <- sys.calls()
  frames <- sys.frames()
  parents <- sys.parents()

  frame_loc <- detect_index(frames, identical, frame)
  bottom_loc <- frame_loc
  setup_loc <- 0L

  setup_caller <- NULL
  from_handler <- NULL
  handler_frame <- NULL

  # If rethrowing we need to find:
  # - The caller of the condition setup frame. This replaces `call`
  #   when it points to the handler frame.
  # - The caller of the handler frame, used to soft-truncate the
  #   backtrace. This way we hide the condition signalling and
  #   handling context (which can be quite complex) in simplified
  #   backtraces.
  if (rethrowing) {

    # This iteration through callers may be incorrect in case of
    # intervening frames. Ideally, we'd iterate only over parent frames.
    # This shouldn't be likely to cause issues though.
    while (is_null(from_handler) && frame_loc > 1L) {
      prev_frame <- frames[[frame_loc - 1L]]
      if (env_has(prev_frame, ".__handler_frame__.")) {
        from_handler <- "calling"
        handler_frame <- frames[[frame_loc]]
        frame_loc <- frame_loc - 1L

        setup_frame <- env_get(prev_frame, ".__setup_frame__.", default = NULL)
        if (!is_null(setup_frame)) {
          setup_caller <- eval_bare(call2(parent.frame), setup_frame)
        }
      }

      if ((frame_loc - 1) > 0) {
        call1 <- calls[[frame_loc]]
        call2 <- calls[[frame_loc - 1]]

        if (is_exiting_handler_call(call1, call2)) {
          from_handler <- "exiting"
          handler_frame <- handler_frame %||% frames[[frame_loc]]
          setup_loc <- calls_try_catch_loc(calls, frame_loc)
          bottom_loc <- parents[[setup_loc]]
        } else {
          if (is_calling_handler_inlined_call(call1)) {
            from_handler <- "calling"
            handler_frame <- handler_frame %||% frames[[frame_loc]]
            bottom_loc <- calls_signal_loc(calls, frame_loc - 1L)
          } else if (is_calling_handler_simple_error_call(call1, call2)) {
            from_handler <- "calling"
            handler_frame <- handler_frame %||% frames[[frame_loc]]
            bottom_loc <- calls_signal_loc(calls, frame_loc - 2L)
          }
          setup_loc <- calls_setup_loc(calls, frames, frame_loc)
        }
      }
      if (is_null(from_handler)) {
        frame_loc <- frame_loc - 1L
      }
    }
  }

  if (bottom_loc) {
    # Skip frames marked with the sentinel `.__signal_frame__.`
    bottom_loc <- skip_signal_frames(bottom_loc, frames)
    bottom_frame <- frames[[bottom_loc]]
    if (!rethrowing && !is_missing(abort_call) && is_environment(abort_call)) {
      abort_call_loc <- detect_index(frames, identical, abort_call)
      if (abort_call_loc && abort_call_loc < bottom_loc) {
        bottom_frame <- frames[[abort_call_loc]]
      }
    }
  } else {
    bottom_frame <- NULL
  }

  if (is_null(setup_caller) && setup_loc && parents[[setup_loc]]) {
    setup_caller <- frames[[parents[[setup_loc]]]]
  }

  list(
    from_handler = from_handler,
    handler_frame = handler_frame,
    bottom_frame = bottom_frame,
    setup_caller = setup_caller
  )
}

calls_try_catch_loc <- function(calls, loc) {
  loc <- loc - 1L
  node <- as.pairlist(rev(calls[seq_len(loc)]))

  while (is_call(node_car(node), c("tryCatchList", "tryCatchOne"))) {
    node <- node_cdr(node)
    loc <- loc - 1L
  }

  loc
}

calls_signal_loc <- function(calls, loc) {
  # Visible bindings for R CMD check
  tmp_node <- tmp_loc <- found_restart <- NULL

  node <- as.pairlist(rev(calls[seq_len(loc)]))
  call <- node_car(node)

  advance <- function(node, i) {
    list(node_cdr(node), i - 1L)
  }
  advance_restart <- function(node, i) {
    found <- FALSE

    restart_fns <- c(
      "doWithOneRestart",
      "withOneRestart",
      "withRestarts"
    )
    while (is_call(node_car(node), restart_fns)) {
      node <- node_cdr(node)
      i <- i - 1L
      found <- TRUE
    }

    list(node, i, found)
  }

  if (is_call(call, "stop")) {
    return(loc)
  }

  if (is_call(call, "signalCondition")) {
    c(tmp_node, tmp_loc, found_restart) %<-% advance_restart(node, loc)

    if (found_restart && is_call(node_car(tmp_node), "message")) {
      return(tmp_loc)
    } else {
      return(loc)
    }
  }

  c(tmp_node, tmp_loc, found_restart) %<-% advance_restart(node, loc)
  if (found_restart) {
    if (is_call(node_car(tmp_node), ".signalSimpleWarning")) {
      c(tmp_node, tmp_loc) %<-% advance(tmp_node, tmp_loc)
    }
    if (is_call(node_car(tmp_node), "warning")) {
      return(tmp_loc)
    }
  }

  loc
}

calls_setup_loc <- function(calls, frames, handler_loc) {
  handler <- sys.function(handler_loc)
  top <- handler_loc

  while (TRUE) {
    calls <- calls[seq_len(top)]
    setup_loc <- detect_index(calls, is_call, "withCallingHandlers", .right = TRUE)

    if (!setup_loc) {
      return(0L)
    }

    signal_handlers <- frames[[setup_loc]][["handlers"]]

    if (some(signal_handlers, identical, handler)) {
      return(setup_loc)
    }

    top <- setup_loc - 1L
  }
}

skip_signal_frames <- function(loc, frames) {
  found <- FALSE
  while (loc > 1 && env_has(frames[[loc - 1L]], ".__signal_frame__.")) {
    found <- TRUE
    loc <- loc - 1L
  }

  if (found) {
    loc - 1L
  } else {
    loc
  }
}

is_calling_handler_inlined_call <- function(call) {
  is_call(call) && length(call) >= 2 && is_function(call[[1]]) && is_condition(call[[2]])
}
is_calling_handler_simple_error_call <- function(call1, call2) {
  identical(call1, quote(h(simpleError(msg, call)))) && is_call(call2, ".handleSimpleError")
}
is_exiting_handler_call <- function(call1, call2) {
  identical(call1, quote(value[[3L]](cond))) && is_call(call2, "tryCatchOne")
}

cnd_message_info <- function(message,
                             body,
                             footer,
                             env,
                             cli_opts = NULL,
                             use_cli_format = NULL,
                             internal = FALSE,
                             error_call = caller_env()) {
  if (internal) {
    check_exclusive(footer, .internal, .require = FALSE, .frame = error_call)
  }

  if (is_function(message)) {
    header <- message
    message <- ""
  } else {
    header <- NULL
  }

  if (length(message) > 1 && !is_character(body) && !is_null(body)) {
    stop_multiple_body(body, call = error_call)
  }

  cli_opts <- cli_opts %||% use_cli(env, error_call = error_call)

  if (!is_null(use_cli_format)) {
    cli_opts[["format"]] <- use_cli_format
  }

  fields <- list()

  if (cli_opts[["inline"]]) {
    message[] <- map_chr(message, cli::format_inline, .envir = env)
  }

  use_cli_format <- cli_opts[["format"]]

  # Formatting with cli is delayed until print time so we can properly
  # indent and width-wrap depending on the context
  if (use_cli_format) {
    if (length(message) > 1) {
      fields$body <- c(message[-1], body)
      message <- message[1]
    } else {
      fields$body <- body
    }
    if (!is_null(header)) {
      fields$header <- header
    }
    if (!is_null(footer)) {
      fields$footer <- footer
    }
    if (internal) {
      fields$footer <- footer_internal(env)
    }
  } else {
    # Compatibility with older bullets formatting
    if (length(message) > 1 && is_null(names(message))) {
      names(message) <- c("", rep_len("*", length(message) - 1))
    }
    if (is_character(body)) {
      message <- c(message, body)
    } else {
      fields$body <- body
    }
    if (is_character(footer)) {
      message <- c(message, footer)
    } else {
      fields$footer <- footer
    }
    if (internal) {
      message <- c(message, footer_internal(env))
    }
    message <- .rlang_cli_format_fallback(message)

    if (is_function(header)) {
      fields$header <- header
    }
  }

  list(
    message = message,
    use_cli_format = use_cli_format,
    extra_fields = fields
  )
}
utils::globalVariables(".internal")

footer_internal <- function(env) {
  top <- topenv(env)
  url_line <- NULL

  if (is_namespace(top)) {
    pkg <- ns_env_name(top)
    pkg_line <- sprintf(
      "This is an internal error that was detected in the %s package.",
      format_pkg(pkg)
    )

    url <- pkg_url_bug(pkg)
    if (!is_null(url)) {
      url_line <- sprintf(
        "Please report it at %s with a %s and the full backtrace.",
        format_url(url),
        format_href("reprex", "https://tidyverse.org/help/")
      )
    }
  } else {
    pkg_line <- "This is an internal error, please report it to the package authors."
  }

  c("i" = pkg_line, " " = url_line)
}

stop_multiple_body <- function(body, call) {
  msg <- c(
    sprintf(
      "Can't supply conflicting bodies in %s and %s.",
      format_arg("body"),
      format_arg("message")
    ),
    "x" = sprintf(
      "%s must be character or NULL when a length > 1 %s is supplied.",
      format_arg("body"),
      format_arg("message")
    ),
    "i" = sprintf(
      "%s is currently %s.",
      format_arg("body"),
      obj_type_friendly(body)
    )
  )
  abort(msg, call = call)
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

use_cli <- function(env, error_call) {
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

  check_use_cli_flag(flag, error_call = error_call)
  flag
}

# Makes sure `inline` can't be set without `format`. Formatting with
# cli is optional. If cli is not installed or too old, the rlang
# fallback formatting is used. On the other hand, formatting inline
# parts with cli requires a recent version of cli to be installed.
check_use_cli_flag <- function(flag, error_call) {
  if (!is_logical(flag) || !identical(names(flag), c("format", "inline")) || anyNA(flag)) {
    abort("`.__rlang_use_cli__.` has unknown format.", call = error_call)
  }

  if (flag[["inline"]]) {
    if (!has_cli_format) {
      msg <- c(
        "`.__rlang_use_cli__.[[\"inline\"]]` is set to `TRUE` but cli is not installed or is too old.",
        "i" = "The package author should add a recent version of `cli` to their `Imports`."
      )
      with_options(
        "rlang:::disable_cli" = TRUE,
        abort(call = error_call)
      )
    }

    if (!flag[["format"]]) {
      msg <- "Can't use cli inline formatting without cli bullets formatting."
      abort(msg, call = error_call)
    }
  }
}

signal_abort <- function(cnd, file = NULL) {
  # Hide this frame in backtraces
  .__signal_frame__. <- TRUE

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

  if (peek_show_error_messages()) {
    # Include backtrace footer option in the condition
    cnd <- cnd_set_backtrace_on_error(cnd, peek_backtrace_on_error())

    # Print the error manually. This allows us to use our own style,
    # include parent errors, and work around limitations on the length
    # of error messages (#856).
    msg <- cnd_message(cnd, inherit = TRUE, prefix = TRUE)

    cat_line(msg, file = file %||% default_message_file())
  }

  # Use `stop()` to run the `getOption("error")` handler (used by
  # RStudio to record a backtrace) and cause a long jump. Running the
  # handler manually wouldn't work because it might (and in RStudio's
  # case, it does) call `geterrmessage()`. Turn off the regular error
  # printing to avoid printing the error twice.
  local_options(show.error.messages = FALSE)
  stop(fallback)
}

peek_show_error_messages <- function() {
  # `abort()` respects the base R option `show.error.messages` (#1630).
  # The only time we don't display error messages is an explicit `FALSE`.
  # All other values still show error messages.
  !is_false(peek_option("show.error.messages"))
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
#' @param arg An argument name as a string. This argument
#'   will be mentioned in error messages as the input that is at the
#'   origin of a problem.
#' @param error_arg An argument name as a string. This argument
#'   will be mentioned in error messages as the input that is at the
#'   origin of a problem.
#' @param call The execution environment of a currently
#'   running function, e.g. `caller_env()`. The function will be
#'   mentioned in error messages as the source of the error. See the
#'   `call` argument of [rlang::abort()] for more information.
#' @param error_call The execution environment of a currently
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
#'   cli::cli_abort("{.arg {arg}} must be a thingy.", arg = arg, call = call)
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
  check_arg(arg)

  expr <- do.call(substitute, list(arg), envir = caller_env())
  as_label(expr)
}

#' Validate and format a function call for use in error messages
#'
#' @description
#'
#' - `error_call()` takes either a frame environment or a call. If the
#'   input is an environment, `error_call()` acts like [frame_call()]
#'   with some additional logic, e.g. for S3 methods and for frames
#'   with a [local_error_call()].
#'
#' - `format_error_call()` simplifies its input to a simple call (see
#'   section below) and formats the result as code (using cli if
#'   available). Use this function to generate the "in" part of an
#'   error message from a stack frame call.
#'
#'   `format_error_call()` first passes its input to `error_call()` to
#'   fetch calls from frame environments.
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

  if (grepl("\n", label)) {
    return(cli_with_whiteline_escapes(label, format_code))
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
  if (is_call(call, c("eval", "evalq", "eval_tidy"))) {
    return(NULL)
  }

  if (!is_call_simple(call)) {
    if (is_expression(call) && is_call_index(call)) {
      return(as_label(call[1]))
    } else {
      return(NULL)
    }
  }

  # Remove namespace for now to simplify conversion
  old <- call[[1]]
  call[[1]] <- sym(call_name(call))

  # Deal with `if` bombs. Keep the condition as it is informative but
  # drop the uninformative branches to avoid multiline calls. See
  # https://github.com/r-lib/testthat/issues/1429
  if (is_call(call, "if")) {
    call[[3]] <- quote(...)
    return(as_label(call[1:3]))
  }

  # Preserve operator calls, even if multiline
  if (!is_string(call_parse_type(call), "")) {
    return(paste(error_call_deparse(call), collapse = "\n"))
  }

  # FIXME! Deparse with arguments?
  if (is_symbol(call[[1]]) && needs_backticks(call[[1]])) {
    return(as_string(call[[1]]))
  }

  # Remove distracting arguments from the call and restore namespace
  call[[1]] <- old
  as_label(call[1])
}

# Add indent to ulterior lines
error_call_deparse <- function(call) {
  out <- expr_deparse(call)
  if (length(out) > 1) {
    out[-1] <- paste0("  ", out[-1])
  }
  out
}

#' @rdname format_error_call
#' @export
error_call <- function(call) {
  while (is_environment(call)) {
    flag <- env_get(call, ".__error_call__.", default = TRUE)

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

        if (is_call(frame_call(prev), "NextMethod")) {
          next
        }

        if (identical(prev$.Generic, gen)) {
          next
        }

        # Recurse in case there is an error flag in a dispatch helper
        return(error_call(prev))
      }
    }

    call <- frame_call(call)
    break
  }

  if (!is_call(call)) {
    return(NULL)
  }

  quo_squash(call)
}

call_restore <- function(x, to) {
  attr(x, "srcref") <- attr(to, "srcref")
  x
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
#' @section Warnings and errors in RMarkdown:
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
#' Similarly to `rlang_backtrace_on_error_report`, you can set
#' `rlang_backtrace_on_warning_report` inside RMarkdown documents to
#' tweak the display of warnings. This is useful in conjunction with
#' [global_entrace()]. Because of technical limitations, there is
#' currently no corresponding `rlang_backtrace_on_warning` option for
#' normal R sessions.
#'
#' To get full entracing in an Rmd document, include this in a setup
#' chunk before the first error or warning is signalled.
#'
#' ````
#' ```{r setup}
#' rlang::global_entrace()
#' options(rlang_backtrace_on_warning_report = "full")
#' options(rlang_backtrace_on_error_report = "full")
#' ```
#' ````
#'
#'
#' @name rlang_backtrace_on_error
#' @seealso rlang_backtrace_on_warning
#' @aliases add_backtrace rlang_backtrace_on_error_report
#'   rlang_backtrace_on_warning_report
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

backtrace_on_error_opts <- c("none", "reminder", "branch", "full")

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
      last_error <- style_rlang_run("last_trace()")
      reminder <- col_silver(paste0("Run `", last_error, "` to see where the error occurred."))
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

peek_backtrace_on_warning_report <- function() {
  opt <- peek_backtrace_on_error_opt("rlang_backtrace_on_warning_report") %||% "none"

  if (is_string(opt, "reminder")) {
    options(rlang_backtrace_on_warning_report = "none")
    warn(c(
      "`rlang_backtrace_on_warning_report` must be one of `c(\"none\", \"branch\", \"full\")`.",
      i = "The option was reset to \"none\"."
    ))

    opt <- "none"
  }

  opt
}

peek_backtrace_on_error_opt <- function(name) {
  opt <- peek_option(name)

  if (!is_null(opt)) {
    if (is_string(opt, "collapse")) {
      options(list2("{name}" := "none"))
      deprecate_collapse()
      return("none")
    }

    if (!is_string(opt, backtrace_on_error_opts)) {
      options(list2("{name}" := NULL))
      warn(c(
        sprintf("Invalid %s option.", format_arg(name)),
        i = "The option was just reset to `NULL`."
      ))
      return(NULL)
    }
  }

  opt
}
