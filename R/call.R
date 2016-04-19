#' Create a call by "hand"
#'
#' @param f Function to call. For \code{make_call}, either a string, a symbol
#'   or a quoted call. For \code{do_call}, a bare function name or call.
#' @param ...,.args Arguments to the call either in or out of a list
#' @export
#' @examples
#' # f can either be a string, a symbol or a call
#' call_new("f", a = 1)
#' call_new(quote(f), a = 1)
#' call_new(quote(f()), a = 1)
#'
#' #' Can supply arguments individually or in a list
#' call_new(quote(f), a = 1, b = 2)
#' call_new(quote(f), .args = list(a = 1, b = 2))
call_new <- function(f, ..., .args = list()) {
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
#'   \code{\link{call_standardise}}.
#' @param env Environment in which to look up call value.
#' @param new_args A named list of expressions (constants, names or calls)
#'   used to modify the call. Use \code{NULL} to remove arguments.
#' @export
#' @examples
#' call <- quote(mean(x, na.rm = TRUE))
#' call_standardise(call)
#'
#' # Modify an existing argument
#' call_modify(call, list(na.rm = FALSE))
#' call_modify(call, list(x = quote(y)))
#'
#' # Remove an argument
#' call_modify(call, list(na.rm = NULL))
#'
#' # Add a new argument
#' call_modify(call, list(trim = 0.1))
#'
#' # Add an explicit missing argument
#' call_modify(call, list(na.rm = quote(expr = )))
call_modify <- function(call, new_args, env = parent.frame()) {
  stopifnot(is.call(call), is.list(new_args))

  call <- call_standardise(call, env)

  if (!all(has_names(new_args))) {
    stop("All new arguments must be named", call. = FALSE)
  }

  for (nm in names(new_args)) {
    call[[nm]] <- new_args[[nm]]
  }
  call
}

#' @rdname call_modify
#' @export
call_standardise <- function(call, env = parent.frame()) {
  stopifnot(is_call(call))

  f <- eval(call[[1]], env)
  if (is.primitive(f)) return(call)

  match.call(f, call)
}
