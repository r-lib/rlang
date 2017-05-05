#' Parse R code
#'
#' These functions parse and transform text into R expressions. This
#' is the first step to interpret or evaluate a piece of R code
#' written by a programmer.
#'
#' `parse_expr()` returns one expression. If the text contains more
#' than one expression (separated by colons or new lines), an error is
#' issued. On the other hand `parse_exprs()` can handle multiple
#' expressions. It always returns a list of expressions (compare to
#' [base::parse()] which returns an base::expression vector). All
#' functions also support R connections.
#'
#' The versions prefixed with `f_` return expressions quoted in
#' formulas rather than raw expressions.
#'
#' @param x Text containing expressions to parse_expr for
#'   `parse_expr()` and `parse_exprs()`. Can also be an R connection,
#'   for instance to a file. If the supplied connection is not open,
#'   it will be automatically closed and destroyed.
#' @param env The environment for the formulas. Defaults to the
#'   context in which the parse_expr function was called. Can be any
#'   object with a `as_env()` method.
#' @return `parse_expr()` returns a formula, `parse_exprs()` returns a
#'   list of formulas.
#' @seealso [base::parse()]
#' @export
#' @examples
#' # parse_expr() can parse_expr any R expression:
#' parse_expr("mtcars %>% dplyr::mutate(cyl_prime = cyl / sd(cyl))")
#'
#' # A string can contain several expressions separated by ; or \n
#' parse_exprs("NULL; list()\n foo(bar)")
#'
#' # The versions suffixed with _f return formulas:
#' parse_quosure("foo %>% bar()")
#' parse_quosures("1; 2; mtcars")
#'
#' # The env argument is passed to as_env(). It can be e.g. a string
#' # representing a scoped package environment:
#' parse_quosure("identity(letters)", env = empty_env())
#' parse_quosures("identity(letters); mtcars", env = "base")
#'
#'
#' # You can also parse source files by passing a R connection. Let's
#' # create a file containing R code:
#' path <- tempfile("my-file.R")
#' cat("1; 2; mtcars", file = path)
#'
#' # We can now parse it by supplying a connection:
#' parse_exprs(file(path))
parse_expr <- function(x) {
  exprs <- parse_exprs(x)

  n <- length(exprs)
  if (n == 0) {
    abort("No expression to parse_expr")
  } else if (n > 1) {
    abort("More than one expression parsed_expr")
  }

  exprs[[1]]
}
#' @rdname parse_expr
#' @export
parse_exprs <- function(x) {
  if (inherits(x, "connection")) {
    if (!isOpen(x)) {
      open(x)
      on.exit(close(x))
    }
    exprs <- parse(file = x)
  } else if (is_scalar_character(x)) {
    exprs <- parse(text = x)
  } else {
    abort("`x` must be a string or a R connection")
  }
  as.list(exprs)
}

#' @rdname parse_expr
#' @export
parse_quosure <- function(x, env = caller_env()) {
  new_quosure(parse_expr(x), as_env(env))
}
#' @rdname parse_expr
#' @export
parse_quosures <- function(x, env = caller_env()) {
  map(parse_exprs(x), new_quosure, env = as_env(env))
}
