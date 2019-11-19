#' Build an error message from a main issue and bullet messages
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' `cnd_message()` assembles an error message from two components:
#'
#' - The `cnd_header()` generic. Methods should return a single line.
#'
#' - The `cnd_body()` generic. Methods should return a named vector
#'   of lines. These lines are automatically prefixed with a bullet by
#'   `cnd_message()` (see the section on error statements).
#'
#' - The `cnd_footer()` generic. Methods should return one or several
#'   lines.
#'
#' `cnd_message()` is automatically called by the `conditionMessage()`
#' for rlang errors so that errors thrown with [abort()] only need to
#' implement `cnd_header()` and `cnd_body()`. It can also be called
#' in custom `conditionMessage()` methods.
#'
#' Note that if you pass a named character vector to [abort()], you
#' get the same formatting behaviour as `cnd_message()`.
#'
#' @param cnd A condition object.
#'
#' @section Error statements:
#'
#' This experimental infrastructure is based on the idea that
#' sentences in error messages are best kept short and simple. From
#' this point of view, the best way to present the information is as a
#' bullet list of simple sentences containing a single clause.
#' `cnd_message()` helps following this structure by building an error
#' message from two parts: the __issue__ and the __bullets__.
#'
#' `cnd_header()` is the generic for the main error message. It should
#' be as generic as possible, but since it is a generic it is easy to
#' override by error subclasses.
#'
#' The `cnd_body()` methods should return a character vector of
#' sentences. These are automatically prefixed with bullets by
#' `cnd_message()`, according to the following scheme:
#'
#' - Elements named `"i"` are prefixed with a blue "info" symbol.
#' - Elements named `"x"` are prefixed with a red "cross" symbol.
#' - Unnamed elements are prefixed with a "*" symbol.
#'
#' While you are free to lay out the bullets in the order that you
#' like, "x" bullets should usually precede "i" bullets.
#'
#'
#' @section Overriding `cnd_body()`:
#'
#' Sometimes the generation of an error message depends on the state
#' of the type checking. In that case, it can be tricky to lazily
#' generate error messages with `cnd_body()`: you can either
#' overspecify your error class hierarchies with one class per state,
#' or replicate the type-checking control flow within the
#' `cnd_body()` method. None of these options are ideal.
#'
#' A better option is to define a `cnd_body` field in your error
#' object. This should be a function (or a lambda-formula which will
#' be passed to [as_function()]) with the same signature as
#' `cnd_body()` methods. This function overrides the
#' `cnd_body()` generic and can generate an error message tailored
#' to the state in which the error was constructed.
#'
#' Note that as a rule, `cnd_header()` should be a general thematic
#' issues that does not depend on state. For this reason, it isn't
#' possible to define an overriding method in the condition object.
#'
#'
#' @section Life cycle:
#'
#' This infrastructure is experimental. In particular, the output of
#' `cnd_message()` is likely to change in the future and you shouldn't
#' test it verbatim in a way that makes R CMD check fail. Instead, use
#' [testthat::verify_output()] to monitor the output without causing
#' CRAN check failures when it changes.
#'
#' @keywords internal
#' @export
cnd_message <- function(cnd) {
  paste_line(
    cnd_header(cnd),
    format_bullets(cnd_body(cnd)),
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
  if (is_function(cnd$cnd_bullets)) {
    cnd$cnd_bullets(cnd, ...)
  } else if (is_bare_formula(cnd$cnd_bullets)) {
    cnd_body <- as_function(cnd$cnd_bullets)
    cnd_body(cnd, ...)
  } else {
    UseMethod("cnd_body")
  }
}
#' @export
cnd_body.default <- function(cnd, ...) {
  chr()
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

#' @rdname cnd_message
#' @export
#' @param x A named character vector of messages. Elements named as
#'   `x` or `i` are prefixed with the corresponding bullet.
format_bullets <- function(x) {
  if (!length(x)) {
    return(x)
  }

  nms <- names2(x)
  stopifnot(nms %in% c("i", "x", ""))

  bullets <- ifelse(nms == "i", info(), ifelse(nms == "x", cross(), "*"))
  bullets <- paste(bullets, x, collapse = "\n")
  bullets
}

collapse_cnd_message <- function(x) {
  if (length(x) > 1L) {
    paste(
      x[[1]],
      format_bullets(x[-1]),
      sep = "\n"
    )
  } else {
    x
  }
}
