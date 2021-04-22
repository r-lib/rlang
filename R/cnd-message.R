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
  collapse_cnd_message(cnd$message)
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
  if (is_null(nms) || all(nms == "")) {
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

collapse_cnd_message <- function(x) {
  if (length(x) == 1L) {
    return(x)
  }

  if (is_null(names(x))) {
    x <- set_names(x, "*")
    names(x)[[1]] <- ""
  }

  format_bullets(x)
}
