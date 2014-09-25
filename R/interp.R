#' Interpolate values into an expression.
#'
#' This is useful if you want to build an expression up from a mixture of
#' constants and variables.
#'
#' @param _obj An object to modify: can be a call, name, formula,
#'   \code{\link{lazy}}, or a string.
#' @param ...,.values Either individual name-value pairs, or a list
#'   (or environment) of values.
#' @export
#' @examples
#' # Interp works with formulas, lazy objects, quoted calls and strings
#' interp(~ x + y, x = 10)
#' interp(lazy(x + y), x = 10)
#' interp(quote(x + y), x = 10)
#' interp("x + y", x = 10)
#'
#' # Use as.name if you have a character string that gives a
#' # variable name
#' interp(~ mean(var), var = as.name("mpg"))
#' # or supply the quoted name directly
#' interp(~ mean(var), var = quote(mpg))
#'
#' # Or a function!
#' interp(~ f(a, b), f = as.name("+"))
#' # Remember every action in R is a function call:
#' # http://adv-r.had.co.nz/Functions.html#all-calls
#'
#' # If you've built up a list of values through some other
#' # mechanism, use .values
#' interp(~ x + y, .values = list(x = 10))
#'
#' # You can also interpolate variables defined in the current
#' # environment, but this is a little risky.
#' y <- 10
#' interp(~ x + y, .values = environment())
interp <- function(`_obj`, ..., .values) {
  UseMethod("interp")
}

#' @export
interp.call <- function(`_obj`, ..., .values) {
  values <- all_values(.values, ...)

  substitute_(`_obj`, values)
}

#' @export
interp.name <- function(`_obj`, ..., .values) {
  values <- all_values(.values, ...)

  substitute_(`_obj`, values)
}

#' @export
interp.formula <- function(`_obj`, ..., .values) {
  values <- all_values(.values, ...)

  `_obj`[[2]] <- substitute_(`_obj`[[2]], values)
  `_obj`
}

#' @export
interp.lazy <- function(`_obj`, ..., .values) {
  values <- all_values(.values, ...)

  `_obj`$expr <-  substitute_(`_obj`$expr, values)
  `_obj`
}

#' @export
interp.character <- function(`_obj`, ..., .values) {
  values <- all_values(.values, ...)

  expr1 <- parse(text = `_obj`)[[1]]
  expr2 <- substitute_(expr1, values)
  deparse(expr2)
}

substitute_ <- function(x, env) {
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}


all_values <- function(.values, ...) {
  if (missing(.values)) {
    values <- list(...)
  } else if (identical(.values, globalenv())) {
    # substitute doesn't want to replace in globalenv
    values <- as.list(globalenv())
  } else {
    values <- .values
  }
  # Replace lazy objects with their expressions
  is_lazy <- vapply(values, is.lazy, logical(1))
  values[is_lazy] <- lapply(values[is_lazy], `[[`, "expr")

  values
}

#' Generate a missing argument.
#'
#' @export
#' @examples
#' interp(~f(x), x = missing_arg())
missing_arg <- function() {
  quote(expr = )
}
