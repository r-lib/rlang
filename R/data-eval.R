#' Evaluate a formula
#'
#' \code{feval} evaluates the RHS of a formula in its environment.
#' If \code{data} is supplied, it will look for the values associated with
#' symbols in their first. It also provides two pronouns to make it possible to
#' be explicit about where you want values to come from: \code{.env} and
#' \code{.data}.
#'
#' @param f A one-sided formula.
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
feval <- function(f, data = NULL) {
  if (is.null(data)) {
    return(eval(rhs(f), environment(f)))
  }

  if (!is.list(data)) {
    stop("`data` must be a list or data frame.", call. = FALSE)
  }

  parent_env <- environment(f)
  env <- new.env(parent = parent_env)
  env$.env <- parent_env
  env$.data <- data

  eval(rhs(f), data, env)
}
