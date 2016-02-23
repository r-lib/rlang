#' Evaluate a formula
#'
#' \code{feval} evaluates the \code{\link{rhs}} of a formula in its environment.
#' If \code{data} is supplied, it will look for the values associated with
#' symbols in their first.
#'
#' @section Pronouns:
#' When used with \code{data}, \code{feval} provides two pronouns to make it
#' possible to be explicit about where you want values to come from:
#' \code{.env} and \code{.data}. These are thin wrappers around \code{.data}
#' and \code{.env} that throw errors if you try to access non-existant values.
#'
#' @param f A one-sided formula. Any expressions wrapped in \code{ (( )) } will
#'   will be "unquoted", i.e. they will be evaluated, and the results insert
#'   back into the formula. See \code{\link{finterp}} for more details.
#' @param data A list (or data frame). When looking up the value associated
#'   with a name, \code{feval_data} will first look in this object.
#' @export
#' @examples
#' feval(~ 1 + 2 + 3)
#'
#' # formulas automatically capture their enclosing environment
#' foo <- function(x) {
#'   y <- 10
#'   ~ x + y
#' }
#' f <- foo(1)
#' f
#' feval(f)
#'
#' # If you supply data, feval will for their first:
#' feval(~ cyl, mtcars)
#'
#' # To avoid ambiguity, you can use .env and .data pronouns to be
#' # explicit:
#' cyl <- 10
#' feval(~ .data$cyl, mtcars)
#' feval(~ .env$cyl, mtcars)
#'
#' # Imagine you are computing the mean of a variable:
#' feval(~ mean(cyl), mtcars)
#' # How can you change the variable that's being computed?
#' # The easiest way is to take advantage of that the fact that anything
#' # inside (( )) will be evaluated and literally inserted into formula.
#' # See ?finterp for more details
#' var <- quote(cyl)
#' feval(~ mean( ((var)) ), mtcars)
#'
#' # If you were using this inside a function, you might want to
#' # take one more step of explicitness. Unfortunately data$((var)) is
#' # not valid R code, so we need to use the prefix for of `$`.
#' feval(~ mean( `$`(.data, ((var)) )), mtcars)
feval <- function(f, data = NULL) {
  expr <- rhs(finterp(f))

  if (!is.null(data) && !is.list(data)) {
    stop("`data` must be must be NULL, a list, or a data frame.", call. = FALSE)
  }

  parent_env <- environment(f)
  env <- new.env(parent = parent_env)
  env$.env <- complain(parent_env)
  env$.data <- complain(data)

  eval(expr, data, env)
}
