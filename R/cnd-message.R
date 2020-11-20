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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
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
#'
#' `format_error_bullets()` takes a character vector and returns a single
#' string (or an empty vector if the input is empty). The elements of
#' the input vector are assembled as a list of bullets, depending on
#' their names:
#'
#' - Elements named `"i"` are bulleted with a blue "info" symbol.
#' - Elements named `"x"` are bulleted with a red "cross" symbol.
#' - Unnamed elements are bulleted with a "*" symbol.
#'
#' This experimental infrastructure is based on the idea that
#' sentences in error messages are best kept short and simple. From
#' this point of view, the best way to present the information is in
#' the [cnd_body()] method of an error conditon, as a bullet list of
#' simple sentences containing a single clause. The info and cross
#' symbols of the bullets provide hints on how to interpret the bullet
#' relative to the general error issue, which should be supplied as
#' [cnd_header()].
#'
#' @param x A named character vector of messages. Elements named as
#'   `x` or `i` are prefixed with the corresponding bullet.
#' @export
format_error_bullets <- function(x) {
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
      format_error_bullets(x[-1]),
      sep = "\n"
    )
  } else {
    x
  }
}
