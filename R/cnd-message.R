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
#' `format_bullets()` takes a character vector and returns a single
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
#' writeLines(format_bullets(c("foo", "bar")))
#'
#' # This is equivalent to
#' writeLines(format_bullets(set_names(c("foo", "bar"), "*")))
#'
#' # Supply named elements to format info, cross, and tick bullets
#' writeLines(format_bullets(c(i = "foo", x = "bar", v = "baz", "*" = "quux")))
#'
#' # An unnamed element breaks the line
#' writeLines(format_bullets(c(i = "foo\nbar")))
#'
#' # A " " element breaks the line within a bullet (with indentation)
#' writeLines(format_bullets(c(i = "foo", " " = "bar")))
#' @export
format_bullets <- function(x) {
  if (!length(x)) {
    return(x)
  }

  nms <- names(x)

  # Treat unnamed vectors as all bullets
  if (is_null(nms)) {
    bullets <- rep_along(x, bullet())
    return(paste(bullets, x, sep = " ", collapse = "\n"))
  }

  if (!all(nms %in% c("i", "x", "v", "*", "!", ">", " ", ""))) {
    abort('Bullet names must be one of "i", "x", "v", "*", "!", ">", or " ".')
  }

  bullets <-
    ifelse(nms == "i", info(),
    ifelse(nms == "x", cross(),
    ifelse(nms == "v", tick(),
    ifelse(nms == "*", bullet(),
    ifelse(nms == "!", yellow("!"),
    ifelse(nms == ">", arrow_right(),
    ifelse(nms == "", "",
    ifelse(nms == " ", " ",
      "*"))))))))

  bullets <-
    ifelse(bullets == "", "", paste0(bullets, " "))

  paste0(bullets, x, collapse = "\n")
}

#' Formatting messages with the cli package:
#'
#' `r lifecycle::badge("experimental")`
#'
#' You can use [cli](https://cli.r-lib.org/) and
#' [glue](https://glue.tidyverse.org/) formatting syntax by adding
#' this flag to your namespace:
#'
#' ```
#' .rlang_use_cli_format <- TRUE
#' ```
#'
#' - If `TRUE`, an internal error is thrown if cli is not
#'   installed. Make sure it is installed by adding it to your
#'   Imports.
#'
#' - If `FALSE`, the fallback formatting is used.
#'
#' @name cli-format
NULL

#' Format message
#'
#' @description
#'
#' TODO:
#' - Flesh out documentation.
#' - Replace [format_bullets()] by `format_message()`.
#'
#' These utils take a character vector and return a single string
#' formatted according to [format_bullets()] (TODO: unexport this and
#' merge documentation).  If you generate condition messages lazily
#' from a [conditionMessage()] or [cnd_message()] method, call one of
#' these to format the message for display.
#'
#' If cli is available and if `env` has the cli formatting flag (see
#' [cli-format]), cli formatting is applied to `x`.
#'
#' `format_message()` powers [warn()] and [inform()].
#' `format_error_message()` powers [abort()]. The latter is needed to
#' get correct formatting in case cli is used.
#'
#' @param x A character vector to format into a message ready for
#'   display.
#' @param env The interpolation environment to use in case cli
#'   formatting is enabled.
#' @export
format_message <- function(x, env = caller_env()) {
  # No-op for the empty string, e.g. for `abort("", class = "foo")`
  # and a `conditionMessage.foo()` method
  if (is_string(x, "")) {
    return("")
  }

  orig <- x

  # Interpret unnamed vectors as bullets
  if (is_null(names(x))) {
    if (length(x) > 1) {
      x <- set_names(x, "*")
      names(x)[[1]] <- ""
    } else {
      x <- set_names(x, names2(x))
    }
  } 

  if (use_cli_format(env)) {
    if (!has_cli_format) {
      with_options(
        "rlang:::disable_cli" = TRUE,
        abort(c(
          "`.rlang_use_cli_format` is set to `TRUE` but cli is not installed.",
          "i" = "The package author should add `cli` to their `Imports`."
        ))
      )
    }
    out <- cli_format_message(x, env)
  } else {
    out <- format_bullets(x)
  }

  str_restore(out, orig)
}

#' @rdname format_message
#' @export
format_error_message <- function(x, env = caller_env()) {
  if (!has_cli_format) {
    return(format_message(x, env))
  }

  orig <- x

  # Add "Error: " for the wrapping, because R adds it unconditionally
  # TODO: I apparently can't translate this with
  # gettext("Error: ", domain = "R")
  x[[1]] <- paste0("Error: ", x[[1]]) 

  out <- format_message(x, env)

  # Remove "Error: " that was only needed for the wrapping
  out <- cli::ansi_substr(out, 8, nchar(out))

  str_restore(out, orig)
}

str_restore <- function(x, to) {
  to <- to[1]
  to[[1]] <- x
  to
}

use_cli_format <- function(env) {
  # Internal option to disable cli in case of recursive errors
  if (is_true(peek_option("rlang:::disable_cli"))) {
    return(FALSE)
  }

  # Formatting with cli is opt-in for now
  default <- FALSE

  flag <- env_get(
    env,
    ".rlang_use_cli_format",
    default = default,
    inherit = TRUE,
    last = topenv(env)
  )

  if (is_string(flag, "try")) {
    return(has_cli_format)
  }

  if (!is_bool(flag)) {
    abort("`.rlang_use_cli_format` must be a logical value.")
  }

  flag
}


# FIXME: This should be exported from cli
cli_format_message <- function(x, env) {
  fmt <- env_get(ns_env("cli"), "fmt")
  msg <- fmt(cli::cli_bullets(x, .envir = env), collapse = TRUE)

  # Remove trailing newline created by `fmt()`
  substr(msg, 1, nchar(msg) - 1)
}
