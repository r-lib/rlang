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
#' - The `cnd_bullets()` generic. methods should return a named
#'   vector of lines. These lines are automatically prefixed with a
#'   bullet by `cnd_message()` (see the section on error
#'   statements).
#'
#' `cnd_message()` is automatically called by the `conditionMessage()`
#' for rlang errors so that errors thrown with [abort()] only need to
#' implement `cnd_issue()` and `cnd_bullets()`. It can also be called
#' in custom `conditionMessage()` methods.
#'
#' @param cnd A condition object.
#'
#' @section Error statements:
#'
#' This experimental infrastructure is based on the idea that
#' sentences in error messages are best kept very short and very
#' simple. From this point of view, you should strive for simple
#' sentences, ideally containing a single phrase. The best way to
#' present the information is then as a list of bullets.
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
#' You are free to lay out the bullets in the order that you
#' like. However, the "x" elements should usually come before "i"
#' elements.
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
#' To work around this, you can define a `.bullets` field in your
#' error object. This should be a function (or a lambda-formula which
#' will be passed to [as_function()]) with the same signature as
#' `cnd_bullets()` methods. This function overrides the
#' `cnd_bullets()` generic and can generate an error message tailored
#' to the state in which the error was constructed.
#'
#' Note that as a rule, `cnd_issue()` should be a general thematic
#' issues that does not depend on state, and so does not need to be
#' overridden.
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
  if (is_function(cnd$.bullets)) {
    cnd$.bullets(cnd, ...)
  } else if (is_bare_formula(cnd$.bullets)) {
    cnd_bullets <- as_function(cnd$.bullets)
    cnd_bullets(cnd, ...)
  } else {
    UseMethod("cnd_bullets")
  }
}
#' @export
cnd_bullets.default <- function(cnd, ...) {
  chr()
}

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
