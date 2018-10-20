#' Parse R code
#'
#' These functions parse and transform text into R expressions. This
#' is the first step to interpret or evaluate a piece of R code
#' written by a programmer.
#'
#' `parse_expr()` returns one expression. If the text contains more
#' than one expression (separated by semicolons or new lines), an error is
#' issued. On the other hand `parse_exprs()` can handle multiple
#' expressions. It always returns a list of expressions (compare to
#' [base::parse()] which returns a base::expression vector). All
#' functions also support R connections.
#'
#' The versions suffixed with `_quo` and `_quos` return
#' [quosures][quotation] rather than raw expressions.
#'
#'
#' @section Life cycle:
#'
#' - `parse_quosure()` and `parse_quosures()` were soft-deprecated in
#'   rlang 0.2.0 and renamed to `parse_quo()` and `parse_quos()`. This
#'   is consistent with the rule that abbreviated suffixes indicate
#'   the return type of a function.
#'
#' @param x Text containing expressions to parse_expr for
#'   `parse_expr()` and `parse_exprs()`. Can also be an R connection,
#'   for instance to a file. If the supplied connection is not open,
#'   it will be automatically closed and destroyed.
#' @param env The environment for the quosures. Depending on the use
#'   case, a good default might be the [global
#'   environment][global_env] but you might also want to evaluate the
#'   R code in an isolated context (perhaps a child of the global
#'   environment or of the [base environment][base_env]).
#' @return `parse_expr()` returns an [expression][is_expression],
#'   `parse_exprs()` returns a list of expressions. Note that for the
#'   plural variants the length of the output may be greater than the
#'   length of the input. This would happen is one of the strings
#'   contain several expressions (such as `"foo; bar"`).
#' @seealso [base::parse()]
#' @export
#' @examples
#' # parse_expr() can parse any R expression:
#' parse_expr("mtcars %>% dplyr::mutate(cyl_prime = cyl / sd(cyl))")
#'
#' # A string can contain several expressions separated by ; or \n
#' parse_exprs("NULL; list()\n foo(bar)")
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
    abort("No expression to parse")
  } else if (n > 1) {
    abort("More than one expression parsed")
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
  } else if (is_string(x)) {
    exprs <- parse(text = x)
  } else if (is.character(x)) {
    x <- paste(x, collapse = "; ")
    exprs <- parse(text = x)
  } else {
    abort("`x` must be a character vector or an R connection")
  }
  as.list(exprs)
}

#' @rdname parse_expr
#' @export
parse_quo <- function(x, env) {
  if (missing(env)) {
    abort("The quosure environment should be explicitly supplied as `env`")
  }
  new_quosure(parse_expr(x), as_environment(env))
}
#' @rdname parse_expr
#' @export
parse_quos <- function(x, env) {
  if (missing(env)) {
    abort("The quosure environment should be explicitly supplied as `env`")
  }
  out <- map(parse_exprs(x), new_quosure, env = as_environment(env))
  new_quosures(out)
}
