#' Create a call by "hand"
#'
#' @param f Function to call. For \code{make_call}, either a string, a symbol
#'   or a quoted call. For \code{do_call}, a bare function name or call.
#' @param ...,.args Arguments to the call either in or out of a list
#' @export
#' @examples
#' # f can either be a string, a symbol or a call
#' new_call("f", a = 1)
#' new_call(quote(f), a = 1)
#' new_call(quote(f()), a = 1)
#'
#' #' Can supply arguments individually or in a list
#' new_call(quote(f), a = 1, b = 2)
#' new_call(quote(f), .args = list(a = 1, b = 2))
new_call <- function(f, ..., .args = list()) {
  if (is.character(f)) {
    if (length(f) != 1) {
      stop("Character `f` must be length 1", call. = FALSE)
    }
    f <- as.name(f)
  }

  args <- c(list(...), as.list(.args))
  as.call(c(f, args))
}

#' Modify the arguments of a call.
#'
#' @param call A call to modify. It is first standardised with
#'   \code{\link{standardise_call}}.
#' @param new_args A named list of expressions (constants, names or calls)
#'   used to modify the call. Use \code{NULL} to remove arguments.
#' @export
#' @examples
#' call <- quote(mean(x, na.rm = TRUE))
#'
#' # Modify an existing argument
#' modify_call(call, list(na.rm = FALSE))
#' modify_call(call, list(x = quote(y)))
#'
#' # Remove an argument
#' modify_call(call, list(na.rm = NULL))
#'
#' # Add a new argument
#' modify_call(call, list(trim = 0.1))
#'
#' # Add an explicit missing argument
#' modify_call(call, list(na.rm = quote(expr = )))
modify_call <- function(call, new_args) {
  stopifnot(is.call(call), is.list(new_args))

  call <- standardise_call(call)

  nms <- names(new_args) %||% rep("", length(new_args))
  if (any(nms == "")) {
    stop("All new arguments must be named", call. = FALSE)
  }

  for(nm in nms) {
    call[[nm]] <- new_args[[nm]]
  }
  call
}

#' Standardise a function call
#'
#' @param call A call
#' @param env Environment in which to look up call value.
#' @export
standardise_call <- function(call, env = parent.frame()) {
  stopifnot(is.call(call))
  f <- eval(call[[1]], env)
  if (is.primitive(f)) return(call)

  match.call(f, call)
}
