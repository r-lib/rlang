#' Build an error message from a main issue and bullet messages
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' `cnd_message()` assembles an error message from two components:
#'
#' - The `cnd_issue()` generic. Methods should return a single line.
#'
#' - The `cnd_bullets()` generic. Methods should return a named vector
#'   of lines. These lines are automatically prefixed with a bullet by
#'   `cnd_message()` (see the section on error statements).
#'
#' `cnd_message()` is automatically called by the `conditionMessage()`
#' for rlang errors so that errors thrown with [abort()] only need to
#' implement `cnd_issue()` and `cnd_bullets()`. It can also be called
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
#' `cnd_issue()` is the generic for the main error message. It should
#' be as generic as possible, but since it is a generic it is easy to
#' override by error subclasses.
#'
#' The `cnd_bullets()` methods should return a character vector of
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
#' @section Overriding `cnd_bullets()`:
#'
#' Sometimes the generation of an error message depends on the state
#' of the type checking. In that case, it can be tricky to lazily
#' generate error messages with `cnd_bullets()`: you can either
#' overspecify your error class hierarchies with one class per state,
#' or replicate the type-checking control flow within the
#' `cnd_bullets()` method. None of these options are ideal.
#'
#' A better option is to define a `cnd_bullets` field in your error
#' object. This should be a function (or a lambda-formula which will
#' be passed to [as_function()]) with the same signature as
#' `cnd_bullets()` methods. This function overrides the
#' `cnd_bullets()` generic and can generate an error message tailored
#' to the state in which the error was constructed.
#'
#' Note that as a rule, `cnd_issue()` should be a general thematic
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
    cnd_issue(cnd),
    format_bullets(cnd_bullets(cnd))
  )
}

#' @rdname cnd_message
#' @export
cnd_issue <- function(cnd, ...) {
  UseMethod("cnd_issue")
}
#' @export
cnd_issue.default <- function(cnd, ...) {
  cnd$message
}

#' @rdname cnd_message
#' @export
cnd_bullets <- function(cnd, ...) {
  if (is_function(cnd$cnd_bullets)) {
    cnd$cnd_bullets(cnd, ...)
  } else if (is_bare_formula(cnd$cnd_bullets)) {
    cnd_bullets <- as_function(cnd$cnd_bullets)
    cnd_bullets(cnd, ...)
  } else {
    UseMethod("cnd_bullets")
  }
}
#' @export
cnd_bullets.default <- function(cnd, ...) {
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
