#' Build an error message from parts
#'
#' @description
#'
#' `cnd_message()` assembles an error message from three generics:
#'
#' - `cnd_header()`
#' - `cnd_body()`
#' - `cnd_footer()`
#'
#' Methods for these generics must return a character vector. The
#' elements are combined into a single string with a newline
#' separator. Bullets syntax is supported, either through rlang (see
#' [format_error_bullets()]), or through cli if the condition has
#' `use_cli_format` set to `TRUE`.
#'
#' The default method for the error header returns the `message` field
#' of the condition object. The default methods for the body and
#' footer return the the `body` and `footer` fields if any, or empty
#' character vectors otherwise.
#'
#' `cnd_message()` is automatically called by the `conditionMessage()`
#' for rlang errors, warnings, and messages. Error classes created
#' with [abort()] only need to implement header, body or footer
#' methods. This provides a lot of flexibility for hierarchies of
#' error classes, for instance you could inherit the body of an error
#' message from a parent class while overriding the header and footer.
#'
#'
#' @section Overriding header, body, and footer methods:
#'
#' Sometimes the contents of an error message depends on the state of
#' your checking routine. In that case, it can be tricky to lazily
#' generate error messages with `cnd_header()`, `cnd_body()`, and
#' `cnd_footer()`: you have the choice between overspecifying your
#' error class hierarchies with one class per state, or replicating
#' the type-checking control flow within the `cnd_body()` method. None
#' of these options are ideal.
#'
#' A better option is to define `header`, `body`, or `footer` fields
#' in your condition object. These can be a static string, a
#' [lambda-formula][as_function], or a function with the same
#' signature as `cnd_header()`, `cnd_body()`, or `cnd_footer()`. These
#' fields override the message generics and make it easy to generate
#' an error message tailored to the state in which the error was
#' constructed.
#'
#' @param cnd A condition object.
#' @param ... Arguments passed to methods.
#' @param inherit Wether to include parent messages. Parent messages
#'   are printed with a "Caused by error:" prefix, even if `prefix` is
#'   `FALSE`.
#' @param prefix Whether to print the full message, including the
#'   condition prefix (`Error:`, `Warning:`, `Message:`, or
#'   `Condition:`). The prefix mentions the `call` field if present,
#'   and the `srcref` info if present. If `cnd` has a `parent` field
#'   (i.e. the condition is chained), the parent messages are included
#'   in the message with a `Caused by` prefix.
#'
#' @export
cnd_message <- function(cnd, ..., inherit = TRUE, prefix = FALSE) {
  orig <- cnd

  # Easier to zap the parent than thread `inherit` across functions
  if (!inherit) {
    cnd$parent <- NULL
  }

  if (prefix) {
    msg <- cnd_message_format_prefixed(cnd, ..., parent = FALSE)
  } else {
    msg <- cnd_message_format(cnd, ...)
  }

  # Parent messages are always prefixed
  while (is_condition(cnd <- cnd$parent)) {
    parent_msg <- cnd_message_format_prefixed(cnd, parent = TRUE)
    msg <- paste_line(msg, parent_msg)
  }

  backtrace_on_error <- cnd_backtrace_on_error(orig) %||% "none"
  trace_footer <- format_onerror_backtrace(orig, opt = backtrace_on_error)

  c(msg, trace_footer)
}
cnd_message_lines <- function(cnd, ...) {
  c(
    cnd_header(cnd, ...),
    cnd_body(cnd, ...),
    cnd_footer(cnd, ...)
  )
}

# Set an internal field that is processed by `cnd_message()`.
# `cnd_message()` is called by `conditionMessage()` and
# `as.character()` methods. The latter is called from `knitr::sew()`.
cnd_set_backtrace_on_error <- function(cnd, opt) {
  cnd$rlang$internal$backtrace_on_error <- opt
  cnd
}
cnd_backtrace_on_error <- function(cnd) {
  cnd[["rlang"]]$internal$backtrace_on_error
}

cnd_message_format <- function(cnd, ..., alert = FALSE) {
  lines <- cnd_message_lines(cnd, ...)
  if (is_string(lines, "")) {
    return("")
  }

  needs_alert <-
    alert &&
    length(lines) &&
    is_string(names2(lines)[[1]], "")

  if (!is_true(cnd$use_cli_format)) {
    out <- paste_line(lines)
    if (needs_alert) {
      out <- paste(ansi_alert(), out)
    }
    return(out)
  }

  if (needs_alert) {
    names2(lines)[[1]] <- "!"
  }

  cli_format <- switch(
    cnd_type(cnd),
    error = cli::format_error,
    warning = cli::format_warning,
    cli::format_message
  )

  cli_format(glue_escape(lines), .envir = emptyenv())
}

local_cli_indent <- function(frame = caller_env()) {
  cli::cli_div(
    class = "indented",
    theme = list(div.indented = list("margin-left" = 2)),
    .envir = frame
  )
}

#' @rdname cnd_message
#' @export
cnd_header <- function(cnd, ...) {
  if (is_null(cnd[["header"]])) {
    UseMethod("cnd_header")
  } else {
    exec_cnd_method("header", cnd)
  }
}
#' @export
cnd_header.default <- function(cnd, ...) {
  cnd$message
}

#' @rdname cnd_message
#' @export
cnd_body <- function(cnd, ...) {
  if (is_null(cnd[["body"]])) {
    UseMethod("cnd_body")
  } else {
    exec_cnd_method("body", cnd)
  }
}
#' @export
cnd_body.default <- function(cnd, ...) {
  chr()
}

#' @rdname cnd_message
#' @export
cnd_footer <- function(cnd, ...) {
  if (is_null(cnd[["footer"]])) {
    UseMethod("cnd_footer")
  } else {
    exec_cnd_method("footer", cnd)
  }
}
#' @export
cnd_footer.default <- function(cnd, ...) {
  chr()
}

exec_cnd_method <- function(name, cnd) {
  method <- cnd[[name]]

  if (is_function(method)) {
    method(cnd)
  } else if (is_bare_formula(method)) {
    method <- as_function(method)
    method(cnd)
  } else if (is_character(method)) {
    method
  } else {
    msg <- sprintf(
      "%s field must be a character vector or a function.",
      format_code(name)
    )
    abort(msg, call = caller_env())
  }
}


cnd_message_format_prefixed <- function(cnd,
                                        ...,
                                        parent = FALSE,
                                        alert = NULL) {
  type <- cnd_type(cnd)

  if (is_null(alert)) {
    alert <- is_error(cnd) || parent
  }

  if (parent) {
    prefix <- sprintf("Caused by %s", type)
  } else {
    prefix <- col_yellow(capitalise(type))
  }

  call <- format_error_call(cnd$call)

  message <- cnd_message_format(cnd, ..., alert = alert)
  message <- strip_trailing_newline(message)

  if (!nzchar(message)) {
    return(NULL)
  }

  has_loc <- FALSE

  if (is_null(call)) {
    prefix <- sprintf("%s:", prefix)
  } else {
    src_loc <- src_loc(attr(cnd$call, "srcref"))
    if (nzchar(src_loc) && peek_call_format_srcref()) {
      prefix <- sprintf("%s in %s at %s:", prefix, call, src_loc)
      has_loc <- TRUE
    } else {
      prefix <- sprintf("%s in %s:", prefix, call)
    }
  }
  prefix <- style_bold(prefix)

  paste0(prefix, "\n", message)
}

peek_call_format_srcref <- function() {
  opt <- peek_option("rlang_call_format_srcrefs")
  if (is_null(opt)) {
    !is_testing()
  } else {
    check_bool(opt, arg = "rlang_call_format_srcrefs")
    opt
  }
}

#' @export
conditionMessage.rlang_message <- function(c) {
  cnd_message(c)
}
#' @export
conditionMessage.rlang_warning <- function(c) {
  cnd_message(c)
}
#' @export
conditionMessage.rlang_error <- function(c) {
  cnd_message(c)
}

#' @export
as.character.rlang_message <- function(x, ...) {
  paste0(cnd_message(x, prefix = TRUE), "\n")
}
#' @export
as.character.rlang_warning <- function(x, ...) {
  paste0(cnd_message(x, prefix = TRUE), "\n")
}
#' @export
as.character.rlang_error <- function(x, ...) {
  paste0(cnd_message(x, prefix = TRUE), "\n")
}

on_load({
  s3_register("knitr::sew", "rlang_error", function(x, options, ...) {
    # Simulate interactive session to prevent full backtrace from
    # being included in error message
    local_interactive()

    # Save the unhandled error for `rlang::last_error()`.
    poke_last_error(x)

    # Include backtrace footer option in the condition. Processed by
    # `cnd_message()`.
    x <- cnd_set_backtrace_on_error(x, peek_backtrace_on_error_report())

    # The `sew.error()` method calls `as.character()`, which dispatches
    # to `cnd_message()`
    NextMethod()
  })
})

#' Format bullets for error messages
#'
#' @description
#' `format_error_bullets()` takes a character vector and returns a single
#' string (or an empty vector if the input is empty). The elements of
#' the input vector are assembled as a list of bullets, depending on
#' their names:
#'
#' - Unnamed elements are unindented. They act as titles or subtitles.
#' - Elements named `"*"` are bulleted with a cyan "bullet" symbol.
#' - Elements named `"i"` are bulleted with a blue "info" symbol.
#' - Elements named `"x"` are bulleted with a red "cross" symbol.
#' - Elements named `"v"` are bulleted with a green "tick" symbol.
#' - Elements named `"!"` are bulleted with a yellow "warning" symbol.
#' - Elements named `">"` are bulleted with an "arrow" symbol.
#' - Elements named `" "` start with an indented line break.
#'
#' For convenience, if the vector is fully unnamed, the elements are
#' formatted as "*" bullets.
#'
#' The bullet formatting for errors follows the idea that sentences in
#' error messages are best kept short and simple. The best way to
#' present the information is in the [cnd_body()] method of an error
#' conditon as a bullet list of simple sentences containing a single
#' clause. The info and cross symbols of the bullets provide hints on
#' how to interpret the bullet relative to the general error issue,
#' which should be supplied as [cnd_header()].
#'
#' @param x A named character vector of messages. Named elements are
#'   prefixed with the corresponding bullet. Elements named with a
#'   single space `" "` trigger a line break from the previous bullet.
#' @examples
#' # All bullets
#' writeLines(format_error_bullets(c("foo", "bar")))
#'
#' # This is equivalent to
#' writeLines(format_error_bullets(set_names(c("foo", "bar"), "*")))
#'
#' # Supply named elements to format info, cross, and tick bullets
#' writeLines(format_error_bullets(c(i = "foo", x = "bar", v = "baz", "*" = "quux")))
#'
#' # An unnamed element breaks the line
#' writeLines(format_error_bullets(c(i = "foo\nbar")))
#'
#' # A " " element breaks the line within a bullet (with indentation)
#' writeLines(format_error_bullets(c(i = "foo", " " = "bar")))
#' @export
format_error_bullets <- function(x) {
  # Treat unnamed vectors as all bullets
  if (is_null(names(x))) {
    x <- set_names(x, "*")
  }

  # Always use fallback for now
  .rlang_cli_format_fallback(x)
}

# FIXME: These won't be needed after warnings and messages have been
# switched to print-time formatting
rlang_format_warning <- function(x, env = caller_env()) {
  rlang_format(x, env, format_warning, cli::format_warning)
}
rlang_format_message <- function(x, env = caller_env()) {
  rlang_format(x, env, format_message, cli::format_message)
}
rlang_format <- function(x, env, partial_format, cli_format) {
  if (!can_format(x)) {
    return(x)
  }

  use_cli <- use_cli(env)
  inline <- use_cli[["inline"]]
  format <- use_cli[["format"]]

  # Full
  if (inline && format) {
    return(.rlang_cli_str_restore(cli_format(x, env), x))
  }

  # Partial
  if (format) {
    if (has_cli_format) {
      return(partial_format(cli_escape(x)))
    } else {
      return(.rlang_cli_format_fallback(x))
    }
  }

  # None
  x
}

# No-op for the empty string, e.g. for `abort("", class = "foo")` and
# a `conditionMessage.foo()` method. Don't format inputs escaped with `I()`.
can_format <- function(x) {
  !is_string(x, "") && !inherits(x, "AsIs")
}
