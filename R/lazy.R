#' Capture expression for later lazy evaluation.
#'
#' \code{lazy()} uses non-standard evaluation to turn promises into lazy
#' objects; \code{lazy_()} does standard evaluation and is suitable for
#' programming.
#'
#' Use \code{lazy()} like you'd use \code{\link{substitute}()}
#' to capture an unevaluated promise. Compared to \code{substitute()} it
#' also captures the environment associated with the promise, so that you
#' can correctly replay it in the future.
#'
#' @param expr Expression to capture. For \code{lazy_} must be a name
#'   or a call.
#' @param env Environment in which to evaluate expr.
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
  stopifnot(is.call(expr) | is.name(expr))

  structure(list(expr = expr, env = env), class = "lazy")
}

#' @rdname lazy_
#' @export
#' @useDynLib lazy
#' @importFrom Rcpp sourceCpp
lazy <- function(expr, env = parent.frame()) {
  if (identical(env, topenv(env))) {
    # For interactive
    make_lazy_name_env(quote(expr), environment())
  } else {
    expr <- substitute(expr)
    if (!is.name(expr)) {
      stop("Please supply an argument name", call. = FALSE)
    }
    make_lazy_name_env(expr, env)
  }
}

is.lazy <- function(x) inherits(x, "lazy")

#' @export
print.lazy <- function(x, ...) {
  code <- deparse(x$expr)
  if (length(code) > 1) {
    code <- paste(code[[1]], "...")
  }

  cat("<lazy>\n")
  cat("  code: ", code, "\n", sep = "")
  cat("  env: ", env_name(x$env), "\n", sep = "")
}

env_name <- function(x) {
  name <- environmentName(x)
  if (name != "") {
    name
  } else {
    address(x)
  }

}

