#' Create a condition object
#'
#' These constructors make it easy to create subclassed conditions.
#' Conditions are objects that power the error system in R. They can
#' also be used for passing messages to pre-established handlers.
#'
#' `cnd()` creates objects inheriting from `condition`. Conditions
#' created with `error_cnd()`, `warning_cnd()` and `message_cnd()`
#' inherit from `error`, `warning` or `message`.
#'
#' @section Lifecycle:
#'
#' The `.type` and `.msg` arguments have been renamed to `.subclass`
#' and `message`. They are defunct as of rlang 0.3.0.
#'
#' @param .subclass The condition subclass.
#' @param ... Named data fields stored inside the condition
#'   object. These dots are evaluated with [explicit
#'   splicing][tidy-dots].
#' @param message A default message to inform the user about the
#'   condition when it is signalled.
#' @param trace A `trace` object created by [trace_back()].
#' @param parent A parent condition object created by [abort()].
#' @seealso [cnd_signal()], [with_handlers()].
#' @export
#' @examples
#' # Create a condition inheriting from the s3 type "foo":
#' cnd <- cnd("foo")
#'
#' # Signal the condition to potential handlers. Since this is a bare
#' # condition the signal has no effect if no handlers are set up:
#' cnd_signal(cnd)
#'
#' # When a relevant handler is set up, the signal causes the handler
#' # to be called:
#' with_handlers(cnd_signal(cnd), foo = exiting(function(c) "caught!"))
#'
#' # Handlers can be thrown or executed inplace. See with_handlers()
#' # documentation for more on this.
#'
#' # Signalling an error condition aborts the current computation:
#' err <- error_cnd("foo", message = "I am an error")
#' try(cnd_signal(err))
cnd <- function(.subclass, ..., message = "") {
  if (missing(.subclass)) {
    abort("Bare conditions must be subclassed")
  }
  .Call(rlang_new_condition, c(.subclass, "rlang_condition"), message, dots_list(...))
}
#' @rdname cnd
#' @export
error_cnd <- function(.subclass = NULL,
                      ...,
                      message = "",
                      trace = NULL,
                      parent = NULL) {
  if (!is_null(trace) && !inherits(trace, "rlang_trace")) {
    abort("`trace` must be NULL or an rlang backtrace")
  }
  if (!is_null(parent) && !inherits(parent, "condition")) {
    abort("`parent` must be NULL or a condition object")
  }
  fields <- dots_list(trace = trace, parent = parent, ...)
  .Call(rlang_new_condition, c(.subclass, "rlang_error", "error"), message, fields)
}
#' @rdname cnd
#' @export
warning_cnd <- function(.subclass = NULL, ..., message = "") {
  .Call(rlang_new_condition, c(.subclass, "warning"), message, dots_list(...))
}
#' @rdname cnd
#' @export
message_cnd <- function(.subclass = NULL, ..., message = "") {
  .Call(rlang_new_condition, c(.subclass, "message"), message, dots_list(...))
}

#' Is object a condition?
#' @param x An object to test.
#' @export
is_condition <- function(x) {
  inherits(x, "condition")
}

#' What type is a condition?
#'
#' Use `cnd_type()` to check what type a condition is.
#'
#' @param cnd A condition object.
#' @return A string, either `"condition"`, `"message"`, `"warning"`,
#'   `"error"` or `"interrupt"`.
#' @export
#' @examples
#' cnd_type(catch_cnd(abort("Abort!")))
#' cnd_type(catch_cnd(interrupt()))
cnd_type <- function(cnd) {
  .Call(rlang_cnd_type, cnd)
}

#' Signal a condition
#'
#' Signal a condition to handlers that have been established on the
#' stack. Conditions signalled with `cnd_signal()` are assumed to be
#' benign. Control flow can resume normally once the condition has
#' been signalled (if no handler jumped somewhere else on the
#' evaluation stack). On the other hand, `cnd_abort()` treats the
#' condition as critical and will jump out of the distressed call
#' frame (see [rst_abort()]), unless a handler can deal with the
#' condition.
#'
#' If `.critical` is `FALSE`, this function has no side effects beyond
#' calling handlers. In particular, execution will continue normally
#' after signalling the condition (unless a handler jumped somewhere
#' else via [rst_jump()] or by being [exiting()]). If `.critical` is
#' `TRUE`, the condition is signalled via [base::stop()] and the
#' program will terminate if no handler dealt with the condition by
#' jumping out of the distressed call frame.
#'
#' [calling()] handlers are called in turn when they decline to handle
#' the condition by returning normally. However, it is sometimes
#' useful for a calling handler to produce a side effect (signalling
#' another condition, displaying a message, logging something, etc),
#' prevent the condition from being passed to other handlers, and
#' resume execution from the place where the condition was
#' signalled. The easiest way to accomplish this is by jumping to a
#' restart point (see [with_restarts()]) established by the signalling
#' function. `cnd_signal()` always installs a muffle restart (see
#' [cnd_muffle()]).
#'
#' @section Lifecycle:
#'
#' * Modifying a condition object with `cnd_signal()` is defunct.
#'   Consequently the `.msg` and `.call` arguments are retired and
#'   defunct as of rlang 0.3.0.  In addition `.cnd` is renamed to
#'   `cnd` and soft-deprecated.
#'
#' * The `.mufflable` argument is soft-deprecated and no longer has
#'   any effect. Non-critical conditions are always signalled with a
#'   muffle restart.
#'
#' * Creating a condition object with [cnd_signal()] is
#'   soft-deprecated. Please use [signal()] instead.
#'
#' @param cnd A condition object (see [cnd()]).
#' @param .cnd,.mufflable These arguments are retired. `.cnd` has been
#'   renamed to `cnd` and `.mufflable` no longer has any effect as
#'   non-critical conditions are always signalled with a muffling
#'   restart.
#' @seealso [abort()], [warn()] and [inform()] for signalling typical
#'   R conditions. See [with_handlers()] for establishing condition
#'   handlers.
#' @export
#' @examples
#' # Creating a condition of type "foo"
#' cnd <- cnd("foo")
#'
#' # If no handler capable of dealing with "foo" is established on the
#' # stack, signalling the condition has no effect:
#' cnd_signal(cnd)
#'
#' # To learn more about establishing condition handlers, see
#' # documentation for with_handlers(), exiting() and calling():
#' with_handlers(cnd_signal(cnd),
#'   foo = calling(function(c) cat("side effect!\n"))
#' )
#'
#'
#' # By default, cnd_signal() creates a muffling restart which allows
#' # calling handlers to prevent a condition from being passed on to
#' # other handlers and to resume execution:
#' undesirable_handler <- calling(function(c) cat("please don't call me\n"))
#' muffling_handler <- calling(function(c) {
#'   cat("muffling foo...\n")
#'   cnd_muffle(c)
#' })
#'
#' with_handlers(foo = undesirable_handler,
#'   with_handlers(foo = muffling_handler, {
#'     cnd_signal(cnd("foo"))
#'     "return value"
#'   }))
cnd_signal <- function(cnd, .cnd, .mufflable) {
  validate_cnd_signal_args(cnd, .cnd, .mufflable)
  invisible(.Call(rlang_cnd_signal, cnd))
}
validate_cnd_signal_args <- function(cnd, .cnd, .mufflable,
                                     env = parent.frame()) {
  if (is_character(cnd)) {
    signal_soft_deprecated(paste_line(
      "Creating a condition with `cnd_signal()` is soft-deprecated as of rlang 0.3.0.",
      "Please use `signal()` instead."
    ))
    env$cnd <- cnd(cnd)
  }
  if (!missing(.cnd)) {
    signal_soft_deprecated(paste_line(
      "The `.cnd` argument is soft-deprecated as of rlang 0.3.0.",
      "Please use `cnd` instead."
    ))
    if (is_character(.cnd)) {
      signal_soft_deprecated(paste_line(
        "Creating a condition with `cnd_signal()` is soft-deprecated as of rlang 0.3.0.",
        "Please use `signal()` instead."
      ))
      .cnd <- cnd(.cnd)
    }
    env$cnd <- .cnd
  }
  if (!missing(.mufflable)) {
    signal_soft_deprecated(
      "`.mufflable` is soft-deprecated as of rlang 0.3.0 and no longer has any effect"
    )
  }
}

cnd_call <- function(call) {
  if (is_null(call) || is_false(call)) {
    return(NULL)
  }
  if (is_call(call)) {
    return(call)
  }

  if (is_true(call)) {
    call <- 1L
  } else if (!is_scalar_integerish(call, finite = TRUE)) {
    stop("`call` must be a scalar logical or number", call. = FALSE)
  }

  sys.call(sys.parent(call + 1L))
}


#' Signal an error, warning, or message
#'
#' @description
#'
#' These functions are equivalent to base functions [base::stop()],
#' [base::warning()] and [base::message()], but make it easy to supply
#' condition metadata:
#'
#' * Supply `.subclass` to create a classed condition. Typed
#'   conditions can be captured or handled selectively, allowing for
#'   finer-grained error handling.
#'
#' * Supply metadata with named `...` arguments. This data will be
#'   stored in the condition object and can be examined by handlers.
#'
#' `interrupt()` allows R code to simulate a user interrupt of the
#' kind that is signalled with `Ctrl-C`. It is currently not possible
#' to create custom interrupt condition objects.
#'
#'
#' @section Backtrace:
#'
#' Unlike `stop()` and `warning()`, these functions don't include call
#' information by default. This saves you from typing `call. = FALSE`
#' and produces cleaner error messages.
#'
#' A backtrace is always saved into error objects. You can print a
#' simplified backtrace of the last error by calling [last_error()]
#' and a full backtrace with `summary(last_error())`.
#'
#' You can also display a backtrace with the error message by setting
#' the experimental option `rlang__backtrace_on_error`. It supports the
#' following values:
#'
#' * `"reminder"`: Invite users to call `rlang::last_error()` to see a
#'   backtrace.
#' * `"branch"`: Display a simplified backtrace.
#' * `"collapse"`: Display a collapsed backtrace tree.
#' * `"full"`: Display a full backtrace tree.
#'
#'
#' @section Mufflable conditions:
#'
#' Signalling a condition with `inform()` or `warn()` causes a message
#' to be displayed in the console. These messages can be muffled with
#' [base::suppressMessages()] or [base::suppressWarnings()].
#'
#' On recent R versions (>= R 3.5.0), interrupts are typically
#' signalled with a `"resume"` restart. This is however not
#' guaranteed.
#'
#'
#' @section Lifecycle:
#'
#' These functions were changed in rlang 0.3.0 to take condition
#' metadata with `...`. Consequently:
#'
#' * All arguments were renamed to be prefixed with a dot, except for
#'   `type` which was renamed to `.subclass`.
#' * `.call` (previously `call`) can no longer be passed positionally.
#'
#' @inheritParams cnd
#' @param message The message to display.
#' @param .subclass Subclass of the condition. This allows your users
#'   to selectively handle the conditions signalled by your functions.
#' @param ... Additional data to be stored in the condition object.
#' @param call Deprecated as of rlang 0.3.0. Storing the full
#'   backtrace is now preferred to storing a simple call.
#' @param msg,type These arguments were renamed to `message` and
#'   `.type` and are deprecated as of rlang 0.3.0.
#'
#' @seealso [with_abort()] to convert all errors to rlang errors.
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
#'     warn(err$message) # Demote the error to a warning
#'     NA                # Return an alternative value
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
#' # If you call low-level APIs it is good practice to catch technical
#' # errors and rethrow them with a more meaningful message. Pass on
#' # the caught error as `parent` to get a nice decomposition of
#' # errors and backtraces:
#' file <- "http://foo.bar/baz"
#' tryCatch(
#'   download(file),
#'   error = function(err) {
#'     msg <- sprintf("Can't download `%s`", file)
#'     abort(msg, parent = err)
#' })
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
abort <- function(message, .subclass = NULL,
                  ...,
                  trace = NULL,
                  call = NULL,
                  parent = NULL,
                  msg, type) {
  validate_signal_args(msg, type, call)

  if (is_null(trace) && is_null(peek_option("rlang__disable_trace_capture"))) {
    # Prevents infloops when rlang throws during trace capture
    scoped_options("rlang__disable_trace_capture" = TRUE)

    trace <- trace_back()

    if (is_null(parent)) {
      context <- trace_length(trace)
    } else {
      context <- find_capture_context()
    }
    trace <- trace_trim_context(trace, context)
  }

  cnd <- error_cnd(.subclass,
    ...,
    message = message,
    parent = parent,
    trace = trace
  )

  stop(cnd)
}

trace_trim_context <- function(trace, frame = caller_env()) {
  if (is_environment(frame)) {
    idx <- detect_index(trace$envs, identical, env_label(frame))
  } else if (is_scalar_integerish(frame)) {
    idx <- frame
  } else {
    abort("`frame` must be a frame environment or index")
  }

  to_trim <- seq2(idx, trace_length(trace))
  if (length(to_trim)) {
    trace <- trace_subset(trace, -to_trim)
  }

  trace
}
# FIXME: Find more robust strategy of stripping catching context
find_capture_context <- function(n = 3L) {
  sys_parent <- sys.parent(n)
  thrower_frame <- sys.frame(sys_parent)

  call <- sys.call(sys_parent)
  frame <- sys.frame(sys_parent)
  if (!is_call(call, "tryCatchOne") || !env_inherits(frame, ns_env("base"))) {
    return(thrower_frame)
  }

  sys_parents <- sys.parents()
  while (!is_call(call, "tryCatch")) {
    sys_parent <- sys_parents[sys_parent]
    call <- sys.call(sys_parent)
  }

  next_parent <- sys_parents[sys_parent]
  call <- sys.call(next_parent)
  if (is_call(call, "with_handlers")) {
    sys_parent <- next_parent
  }

  sys.frame(sys_parent)
}


#' @rdname abort
#' @export
warn <- function(message, .subclass = NULL, ..., call = NULL, msg, type) {
  validate_signal_args(msg, type, call)

  cnd <- warning_cnd(.subclass, ..., message = message)
  warning(cnd)
}
#' @rdname abort
#' @export
inform <- function(message, .subclass = NULL, ..., call = NULL, msg, type) {
  validate_signal_args(msg, type, call)

  message <- paste0(message, "\n")
  cnd <- message_cnd(.subclass, ..., message = message)
  message(cnd)
}
#' @rdname abort
#' @export
signal <- function(message, .subclass, ...) {
  cnd <- cnd(.subclass, ..., message = message)
  cnd_signal(cnd)
}
validate_signal_args <- function(msg, type, call, env = parent.frame()) {
  if (!missing(msg)) {
    warn_deprecated("`msg` has been renamed to `message` and is deprecated as of rlang 0.3.0")
    env$message <- msg
  }
  if (!missing(type)) {
    warn_deprecated("`type` has been renamed to `.subclass` and is deprecated as of rlang 0.3.0")
    env$.subclass <- type
  }
  if (!is_null(call)) {
    warn_deprecated("`call` is deprecated as of rlang 0.3.0")
  }
}
#' @rdname abort
#' @export
interrupt <- function() {
  .Call(rlang_interrupt)
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
#'
#' @export
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
cnd_muffle <- function(cnd) {
  switch(cnd_type(cnd),
    message = invokeRestart("muffleMessage"),
    warning = invokeRestart("muffleWarning"),
    interrupt = invokeRestart("resume")
  )
  if (inherits(cnd, "rlang_condition")) {
    invokeRestart("rlang_muffle")
  }

  abort("Can't find a muffling restart")
}

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
#' @export
#' @examples
#' catch_cnd(10)
#' catch_cnd(abort("an error"))
#' catch_cnd(cnd_signal("my_condition", .msg = "a condition"))
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

#' @export
print.rlang_error <- function(x,
                              ...,
                              child = NULL,
                              simplify = c("branch", "collapse", "none"),
                              fields = FALSE) {
  if (is_null(child)) {
    header <- bold("<error>")
  } else {
    header <- bold("<error: parent>")
  }

  message <- x$message
  if (is_string(message) && nzchar(message)) {
    message <- sprintf("message: %s", italic(message))
  } else {
    message <- NULL
  }

  cat_line(
    header,
    message,
    sprintf("class:   `%s`", class(x)[[1]])
  )

  if (fields) {
    nms <- chr_enumerate(chr_quoted(names(x)), final = "and")
    cat_line(sprintf("fields:  %s", nms))
  }

  trace <- x$trace
  simplify <- arg_match(simplify, c("collapse", "branch", "none"))

  if (!is_null(trace)) {
    cat_line("backtrace:")

    if (!is_null(child)) {
      # Trim common portions of backtrace
      child_trace <- child$trace
      common <- map_lgl(trace$envs, `%in%`, child_trace$envs)
      trace <- trace_subset(trace, which(!common))

      # Trim catching context if any
      calls <- trace$calls
      if (length(calls) && is_call(calls[[1]], c("tryCatch", "with_handlers", "catch_cnd"))) {
        trace <- trace_subset_across(trace, -1, 1)
      }
    }

    trace_lines <- format(trace, ..., simplify = simplify)
    cat_line(red(trace_lines))
  }

  if (!is_null(x$parent)) {
    print.rlang_error(x$parent, ..., child = x, simplify = simplify, fields = fields)
  }

  # Recommend printing the full backtrace. Only do it after having
  # printed all parent errors first.
  if (simplify == "branch" && is_null(x$parent) && !is_null(trace)) {
    cat_line(silver("Call `rlang::last_trace()` to see the full backtrace"))
  }

  invisible(x)
}

# Last error to be returned in last_error()
last_error_env <- new.env(parent = emptyenv())
last_error_env$cnd <- NULL

#' @export
summary.rlang_error <- function(object, ...) {
  print(object, simplify = "none", fields = TRUE)
}

#' @export
conditionMessage.rlang_error <- function(c) {
  last_error_env$cnd <- c

  message <- c$message

  parents <- chr()
  while (is_condition(c$parent)) {
    c <- c$parent
    parents <- chr(parents, c$message)
  }

  if (length(parents)) {
    parents <- cli_branch(parents)
    message <- paste_line(
      message,
      "Parents:",
      parents
    )
  }

  backtrace <- format_onerror_backtrace(c$trace)
  paste_line(message, backtrace)
}

#' Display backtrace on error
#'
#' @description
#'
#' Errors thrown with [abort()] automatically save a backtrace that
#' can be inspected by calling [last_error()]. Optionally, you can
#' also display the backtrace alongside the error message by setting
#' the experimental option `rlang__backtrace_on_error` to one of the
#' following values:
#'
#' * `"reminder"`: Display a reminder that the backtrace can be
#'   inspected by calling [rlang::last_error()].
#' * `"branch"`: Display a simplified backtrace.
#' * `"collapse"`: Display a collapsed backtrace tree.
#' * `"full"`: Display the full backtrace tree.
#'
#'
#' @section Promote base errors to rlang errors:
#'
#' Call `options(error = rlang:::add_backtrace)` to instrument base
#' errors with rlang features. This handler does two things:
#'
#' * It saves the base error as an rlang object. This allows you to
#'   call [last_error()] to print the backtrace or inspect its data.
#'
#' * It prints the backtrace for the current error according to the
#'   [`rlang__backtrace_on_error`] option.
#'
#' This experimental handler is not exported because it is meant for
#' interactive use only.
#'
#' @name rlang__backtrace_on_error
#' @aliases add_backtrace
#'
#' @examples
#' # Display a simplified backtrace on error for both base and rlang
#' # errors:
#'
#' # options(
#' #   rlang__backtrace_on_error = "branch",
#' #   error = rlang:::add_backtrace
#' # )
#' # stop("foo")
NULL

format_onerror_backtrace <- function(trace) {
  show_trace <- peek_option("rlang__backtrace_on_error") %||% "reminder"

  if (!is_string(show_trace) || !show_trace %in% c("reminder", "branch", "collapse", "full")) {
    options(rlang__backtrace_on_error = NULL)
    warn("Invalid `rlang__backtrace_on_error` option (resetting to `NULL`)")
    return(NULL)
  }

  if (show_trace == "branch") {
    max_frames <- 10L
  } else {
    max_frames <- NULL
  }

  simplify <- switch(show_trace,
    full = "none",
    reminder = "branch", # Check size of backtrace branch
    show_trace
  )

  backtrace_lines <- format(trace, simplify = simplify, max_frames = max_frames)

  # Backtraces of size 0 and 1 are uninteresting
  if (length(backtrace_lines) <= 1L) {
    return(NULL)
  }

  if (show_trace == "reminder") {
    if (is_interactive()) {
      reminder <- silver("Call `rlang::last_error()` to see a backtrace")
    } else {
      reminder <- NULL
    }
    return(reminder)
  }

  paste_line(
    "Backtrace:",
    backtrace_lines
  )
}

#' Add backtrace from error handler
#'
#' @description
#'
#' Set the `error` global option to `quote(rlang::entrace())` to
#' transform base errors to rlang errors. These enriched errors
#' include a backtrace. The RProfile is a good place to set the
#' handler.
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

  if (!missing(cnd) && inherits(cnd, "rlang_error")) {
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
    abort(cnd$message %||% "", error = cnd, trace = trace)
  }
}

signal_context_info <- function(nframe) {
  first <- sys_body(nframe)

  if (is_same_body(first, body(.handleSimpleError))) {
    if (is_same_body(sys_body(nframe - 1), body(stop))) {
      return(list("stop_message", nframe - 2))
    } else {
      return(list("stop_native", nframe - 1))
    }
  }

  if (is_same_body(first, body(stop))) {
    if (is_same_body(sys_body(nframe - 1), body(abort))) {
      return(list("stop_rlang", nframe - 2))
    } else {
      return(list("stop_condition", nframe - 1))
    }
  }

  if (is_same_body(first, body(signalCondition))) {
    if (from_withrestarts(nframe - 1) && is_same_body(sys_body(nframe - 4), body(message))) {
      if (is_same_body(sys_body(nframe - 5), body(inform))) {
        return(list("message_rlang", nframe - 6))
      } else {
        return(list("message", nframe - 5))
      }
    } else {
      return(list("condition", nframe - 1))
    }
  }

  if (from_withrestarts(nframe)) {
    withrestarts_caller <- sys_body(nframe - 3)
    if (is_same_body(withrestarts_caller, body(.signalSimpleWarning))) {
      if (is_same_body(sys_body(nframe - 4), body(warning))) {
        return(list("warning_message", nframe - 5))
      } else {
        return(list("warning_native", nframe - 4))
      }
    } else if (is_same_body(withrestarts_caller, body(warning))) {
      if (is_same_body(sys_body(nframe - 4), body(warn))) {
        return(list("warning_rlang", nframe - 5))
      } else {
        return(list("warning_condition", nframe - 4))
      }
    }
  }

  list("unknown", nframe)
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
    return()
  }

  stop_call <- sys.call(-2)
  stop_frame <- sys.frame(-2)
  cnd <- stop_frame$cond

  # False for errors thrown from the C side
  from_stop <- is_call(stop_call, "stop", ns = c("", "base"))

  # No need to do anything for rlang errors
  if (from_stop && inherits(cnd, "rlang_error")) {
    return(NULL)
  }

  if (from_stop) {
    if (is_null(cnd)) {
      msg_call <- quote(.makeMessage(..., domain = domain))
      msg <- eval_bare(msg_call, stop_frame)
    } else {
      msg <- cnd$message
    }
  } else {
    msg <- NULL
  }

  # Save a fake rlang error containing the backtrace
  err <- error_cnd(message = msg, trace = trace, parent = cnd)
  last_error_env$cnd <- err

  # Print backtrace for current error
  backtrace_lines <- format_onerror_backtrace(trace)
  if (length(backtrace_lines)) {
    cat_line(backtrace_lines)
  }

  NULL
}

add_backtrace <- function() {
  # Warnings don't go through when error is being handled
  msg <- "Warning: `add_backtrace()` is now exported as `enframe()` as of rlang 0.3.1"
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
#' # For cleaner backtraces:
#' options(rlang_trace_top_env = current_env())
#'
#' # with_abort() automatically casts simple errors thrown by stop()
#' # to rlang errors:
#' fn <- function() stop("Base error")
#' try(with_abort(fn()))
#' last_error()
#'
#' # with_abort() is handy for rethrowing low level errors. The
#' # backtraces are then segmented between the low level and high
#' # level contexts.
#' low_level1 <- function() low_level2()
#' low_level2 <- function() stop("Low level error")
#'
#' high_level <- function() {
#'   with_handlers(
#'     with_abort(low_level1()),
#'     error = ~ abort("High level error", parent = .x)
#'   )
#' }
#'
#' try(high_level())
#' last_error()
#' summary(last_error())
#'
#' # Reset to default
#' options(rlang_trace_top_env = NULL)
#' @export
with_abort <- function(expr, classes = "error") {
  handlers <- rep_named(classes, list(entrace))
  handle_call <- rlang::expr(withCallingHandlers(expr, !!!handlers))
  .Call(rlang_eval, handle_call, current_env())
}
