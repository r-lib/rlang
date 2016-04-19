"%||%" <- function(x, y) if(is.null(x)) y else x

is_atomic <- function(x) {
  typeof(x) %in% c("logical", "integer", "double", "complex", "character", "raw")
}

is_vector <- function(x) {
  is_atomic(x) || is.list(x)
}

has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !(is.na(nms) | nms == "")
  }
}

substitute_ <- function(x, env) {
  if (identical(env, globalenv())) {
    env <- as.list(env)
  }

  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}

#' Generate a missing argument.
#'
#' @export
#' @examples
#' f_interp(~f(x = uq(missing_arg())))
#' f_interp(~f(x = uq(NULL)))
missing_arg <- function() {
  quote(expr = )
}
