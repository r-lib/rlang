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
#' The default method for the error header returns the `message` field
#' of the condition object. The default methods for the body and
#' footer return empty character vectors. In general, methods for
#' these generics should return a character vector. The elements are
#' combined into a single string with a newline separator.
#'
#' `cnd_message()` is automatically called by the `conditionMessage()`
#' for rlang errors. Error classes created with [abort()] only need to
#' implement header, body or footer methods. This provides a lot of
#' flexibility for hierarchies of error classes, for instance you
#' could inherit the body of an error message from a parent class
#' while overriding the header and footer.
#'
#'
#' @section Overriding `cnd_body()`:
#'
#' `r lifecycle::badge("experimental")`
#'
#' Sometimes the contents of an error message depends on the state of
#' your checking routine. In that case, it can be tricky to lazily
#' generate error messages with `cnd_body()`: you have the choice
#' between overspecifying your error class hierarchies with one class
#' per state, or replicating the type-checking control flow within the
#' `cnd_body()` method. None of these options are ideal.
#'
#' A better option is to define a `body` field in your error object
#' containing a static string, a [lambda-formula][as_function], or a
#' function with the same signature as `cnd_body()`. This field
#' overrides the `cnd_body()` generic and makes it easy to generate an
#' error message tailored to the state in which the error was
#' constructed.
#'
#' @param cnd A condition object.
#' @param ... Arguments passed to methods.
#'
#' @export
cnd_message <- function(cnd) {
  paste_line(
    cnd_header(cnd),
    cnd_body(cnd),
    cnd_footer(cnd)
  )
}

#' @rdname cnd_message
#' @export
cnd_header <- function(cnd, ...) {
  UseMethod("cnd_header")
}
#' @export
cnd_header.default <- function(cnd, ...) {
  cnd$message
}

#' @rdname cnd_message
#' @export
cnd_body <- function(cnd, ...) {
  if (is_null(cnd$body)) {
    UseMethod("cnd_body")
  } else {
    override_cnd_body(cnd, ...)
  }
}
#' @export
cnd_body.default <- function(cnd, ...) {
  chr()
}

override_cnd_body <- function(cnd, ...) {
  body <- cnd$body

  if (is_function(body)) {
    body(cnd, ...)
  } else if (is_bare_formula(body)) {
    body <- as_function(body)
    body(cnd, ...)
  } else if (is_string(body)) {
    body
  } else {
    abort("`body` must be a string or a function.")
  }
}

#' @rdname cnd_message
#' @export
cnd_footer <- function(cnd, ...) {
  UseMethod("cnd_footer")
}
#' @export
cnd_footer.default <- function(cnd, ...) {
  chr()
}

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

rlang_format_error <- function(x, env = caller_env()) {
  rlang_format(x, env, format_error, cli::format_error)
}
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
  switch(
    use_cli_format(env),
    partial = partial_format(cli_escape(x)),
    full = .rlang_cli_str_restore(cli_format(x, env), x),
    .rlang_cli_format_fallback(x)
  )
}

# No-op for the empty string, e.g. for `abort("", class = "foo")` and
# a `conditionMessage.foo()` method. Don't format inputs escaped with `I()`.
can_format <- function(x) {
  !is_string(x, "") && !inherits(x, "AsIs")
}

use_cli_format <- function(env) {
  # Internal option to disable cli in case of recursive errors
  if (is_true(peek_option("rlang:::disable_cli"))) {
    return(FALSE)
  }

  # Formatting with cli is opt-in for now
  default <- FALSE

  last <- topenv(env)

  # Search across load-all'd environments
  if (identical(last, global_env()) && "devtools_shims" %in% search()) {
    last <- empty_env()
  }

  flag <- env_get(
    env,
    ".rlang_use_cli_format",
    default = default,
    inherit = TRUE,
    last = last
  )

  if (is_string(flag, "try")) {
    if (has_cli_format && .rlang_cli_has_ansi()) {
      return("partial")
    } else {
      return("fallback")
    }
  }

  if (!is_bool(flag)) {
    abort("`.rlang_use_cli_format` must be a logical value.")
  }

  if (flag && !has_cli_format) {
    with_options(
      "rlang:::disable_cli" = TRUE,
      abort(c(
        "`.rlang_use_cli_format` is set to `TRUE` but cli is not installed.",
        "i" = "The package author should add `cli` to their `Imports`."
      ))
    )
  }

  if (flag) {
    "full"
  } else {
    "fallback"
  }
}
