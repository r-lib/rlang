#' Capture expression for later lazy evaluation
#'
#' \code{lazy()} uses non-standard evaluation to turn promises into lazy
#' objects; \code{lazy_()} does standard evaluation and is suitable for
#' programming.
#'
#' @export
#' @examples
#' lazy_(quote(a + x), globalenv())
#' lazy(a + b / c)
#'
#' f <- function(x = b - a) {
#'   lazy(x)
#' }
#' f()
#' f(a + b / c)
lazy_ <- function(expr, env) {
  structure(list(expr = expr, env = env), class = "lazy")
}

#' @rdname lazy_
#' @export
#' @useDynLib lazy
#' @importFrom Rcpp sourceCpp
lazy <- function(x, env = parent.frame()) {
  if (identical(env, topenv(env))) {
    # For interactive
    make_lazy_name_env(quote(x), environment())
  } else {
    make_lazy_name_env(substitute(x), env)
  }
}

is.lazy <- function(x) inherits(x, "lazy")

#' @export
print.lazy <- function(x, ...) {
  cat("<lazy>\n")
  cat("  code: ", format(x$expr), "\n", sep = "")
  cat("  env: ", environmentName(x$env), "\n", sep = "")
}

