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
#'
#' # Lazy is designed to be used inside a function - you should
#' # give it the name of a function argument (a promise)
#' f <- function(x = b - a) {
#'   lazy(x)
#' }
#' f()
#' f(a + b / c)
#'
#' # Lazy works slightly differently when called from the global
#' # environment. This makes it a little easier to play with interactively
#' lazy(a + b / c)
#'
#' # By default, lazy will climb all the way back to the initial promise
#' # This is handy if you have if you have nested functions:
#' g <- function(y) f(y)
#' h <- function(z) g(z)
#' f(a + b)
#' g(a + b)
#' h(a + b)
#'
#' # To avoid this behavour, set follow_symbols = FALSE
#' f <- function(x) {
#'   lazy(x, follow_symbols = FALSE)
#' }
#' g <- function(y) f(y)
#' h <- function(z) g(z)
#' f(a + b)
#' g(a + b)
#' h(a + b)
lazy_ <- function(expr, env) {
  stopifnot(is.call(expr) | is.name(expr))

  structure(list(expr = expr, env = env), class = "lazy")
}

#' @rdname lazy_
#' @export
#' @useDynLib lazy make_lazy
lazy <- function(expr, env = parent.frame(), follow_symbols = TRUE) {
  if (identical(env, topenv(env))) {
    # For interactive experimentation
    lazy_(substitute(expr), env)
  } else {
    expr <- substitute(expr)
    if (!is.name(expr)) {
      stop("Please supply an argument name", call. = FALSE)
    }
    .Call(make_lazy, expr, env, follow_symbols)
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
  cat("  env: ", format(x$env), "\n", sep = "")
}
