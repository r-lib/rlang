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
