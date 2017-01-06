#' @export
#' @rdname f_eval
f_eval_rhs <- function(f, data = NULL) {
  if (!is_formula(f)) {
    stop("`f` is not a formula", call. = FALSE)
  }

  expr <- f_rhs(interp(f, data = data))
  eval_expr(expr, f_env(f), data)
}

#' @export
#' @rdname f_eval
f_eval_lhs <- function(f, data = NULL) {
  if (!is_formula(f)) {
    stop("`f` is not a formula", call. = FALSE)
  }

  expr <- f_lhs(interp(f, data = data))
  eval_expr(expr, f_env(f), data)
}

#' Evaluate a formula
#'
#' \code{f_eval_rhs} evaluates the RHS of a formula and \code{f_eval_lhs}
#' evaluates the LHS. \code{f_eval} is a shortcut for \code{f_eval_rhs} since
#' that is what you most commonly need.
#'
#' If \code{data} is specified, variables will be looked for first in this
#' object, and if not found in the environment of the formula.
#'
#' @section Pronouns:
#' When used with \code{data}, \code{f_eval} provides two pronouns to make it
#' possible to be explicit about where you want values to come from:
#' \code{.env} and \code{.data}. These are thin wrappers around \code{.data}
#' and \code{.env} that throw errors if you try to access non-existent values.
#'
#' @param f A formula. Any expressions wrapped in \code{ UQ() } will
#'   will be "unquoted", i.e. they will be evaluated, and the results inserted
#'   back into the formula. See \code{\link{interp}} for more details.
#' @param data A list (or data frame). \code{data_source} is a generic used to
#'   find the data associated with a given object. If you want to make
#'   \code{f_eval} work for your own objects, you can define a method for this
#'   generic.
#' @param x An object for which you want to find associated data.
#' @export
#' @examples
#' f_eval(~ 1 + 2 + 3)
#'
#' # formulas automatically capture their enclosing environment
#' foo <- function(x) {
#'   y <- 10
#'   ~ x + y
#' }
#' f <- foo(1)
#' f
#' f_eval(f)
#'
#' # If you supply data, f_eval will look their first:
#' f_eval(~ cyl, mtcars)
#'
#' # To avoid ambiguity, you can use .env and .data pronouns to be
#' # explicit:
#' cyl <- 10
#' f_eval(~ .data$cyl, mtcars)
#' f_eval(~ .env$cyl, mtcars)
#'
#' # Imagine you are computing the mean of a variable:
#' f_eval(~ mean(cyl), mtcars)
#' # How can you change the variable that's being computed?
#' # The easiest way is "unquote" with !!
#' # See ?interp for more details
#' var <- ~ cyl
#' f_eval(~ mean( !!var ), mtcars)
f_eval <- f_eval_rhs


eval_expr <- function(expr, env, data) {
  data <- data_source(data)

  eval_env <- env_new(env)
  eval_env$.env <- data_source(env)
  eval_env$.data <- data

  eval(expr, data, eval_env)
}


#' @rdname f_eval
#' @export
data_source <- function(x, lookup_msg = NULL) {
  UseMethod("data_source")
}
#' @export
data_source.default <- function(x, lookup_msg = NULL) {
  if (is_dictionary(x)) {
    data_source_new(as.list(x), lookup_msg)
  } else {
    abort("Data source must be a dictionary")
  }
}
#' @export
data_source.data_source <- function(x, lookup_msg = NULL) {
  if (!is_null(lookup_msg)) {
    attr(x, "lookup_msg") <- lookup_msg
  }
  x
}
#' @export
data_source.NULL <- function(x, lookup_msg = NULL) {
  NULL
}
#' @export
data_source.environment <- function(x, lookup_msg = NULL) {
  lookup_msg <- lookup_msg %||% "Object '%s' not found in environment"
  if (!identical(x, env_global())) {
    x <- env_clone(x)
  }
  data_source_new(x, lookup_msg)
}
#' @export
data_source.data.frame <- function(x, lookup_msg = NULL) {
  lookup_msg <- lookup_msg %||% "Variable '%s' not found in data"
  data_source_new(x, lookup_msg)
}

data_source_new <- function(x, lookup_msg) {
  msg <- lookup_msg %||% "Object '%s' not found in pronoun"
  class <- c("data_source", class(x))
  structure(list(src = x), lookup_msg = msg, class = class)
}

#' @export
`$.data_source` <- function(x, name) {
  src <- .subset2(x, "src")
  if (!has_binding(src, name)) {
    abort(sprintf(attr(x, "lookup_msg"), name))
  }
  src[[name]]
}
#' @export
`[[.data_source` <- function(x, i, ...) {
  if (!is_scalar_character(i)) {
    abort("Must subset with a string")
  }
  src <- .subset2(x, "src")
  if (!has_binding(src, i)) {
    abort(sprintf(attr(x, "lookup_msg"), i))
  }
  src[[i, ...]]
}

has_binding <- function(x, name) {
  UseMethod("has_binding")
}
#' @export
has_binding.default <- function(x, name) {
  name %in% names(x)
}
#' @export
has_binding.environment <- function(x, name) {
  env_has(x, name)
}

# Unclassing before print() or str() is necessary because default
# methods index objects with integers

#' @export
print.data_source <- function(x, ...) {
  print(unclass_src(x), ...)
}
#' @export
str.data_source <- function(object, ...) {
  str(unclass_src(object), ...)
}

unclass_src <- function(x) {
  i <- match("data_source", class(x))
  class(x) <- class(x)[-i]
  x
}
