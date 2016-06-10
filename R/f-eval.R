#' @export
#' @rdname f_eval
f_eval_rhs <- function(f, data = NULL) {
  if (!is_formula(f)) {
    stop("`f` is not a formula", call. = FALSE)
  }

  expr <- f_rhs(f_interp(f, data = data))
  eval_expr(expr, f_env(f), data)
}

#' @export
#' @rdname f_eval
f_eval_lhs <- function(f, data = NULL) {
  if (!is_formula(f)) {
    stop("`f` is not a formula", call. = FALSE)
  }

  expr <- f_lhs(f_interp(f, data = data))
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
#' @param f A formula. Any expressions wrapped in \code{ uq() } will
#'   will be "unquoted", i.e. they will be evaluated, and the results inserted
#'   back into the formula. See \code{\link{f_interp}} for more details.
#' @param data A list (or data frame). \code{find_data} is a generic used to
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
#' # The easiest way is "unquote" with uq()
#' # See ?f_interp for more details
#' var <- ~ cyl
#' f_eval(~ mean( uq(var) ), mtcars)
f_eval <- f_eval_rhs


eval_expr <- function(expr, env, data) {
  data <- find_data(data)

  expr_env <- new.env(parent = env)
  expr_env$.env <- complain(env, "Object '%s' not found in environment")
  expr_env$.data <- complain(data, "Variable '%s' not found in data")

  eval(expr, data, expr_env)
}


#' @rdname f_eval
#' @export
find_data <- function(x) UseMethod("find_data")

#' @export
find_data.NULL <- function(x) list()
#' @export
find_data.list <- function(x) x
#' @export
find_data.data.frame <- function(x) x

#' @export
find_data.default <- function(x) {
  stop("Do not know how to find data associated with `x`", call. = FALSE)
}
