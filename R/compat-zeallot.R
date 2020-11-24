# nocov start --- compat-zeallot --- 2020-11-23

# This drop-in file implements a simple version of zeallot::`%<-%`.
# Please find the most recent version in rlang's repository.


`%<-%` <- function(lhs, value) {
  lhs <- substitute(lhs)
  env <- caller_env()

  if (!is_call(lhs, "c")) {
    abort("The left-hand side of `%<-%` must be a call to `c()`.")
  }

  vars <- as.list(lhs[-1])

  if (length(value) != length(vars)) {
    abort("The left- and right-hand sides of `%<-%` must be the same length.")
  }

  for (i in seq_along(vars)) {
    var <- vars[[i]]
    if (!is_symbol(var)) {
      abort(paste0("Element ", i, " of the left-hand side of `%<-%` must be a symbol."))
    }

    env[[as_string(var)]] <- value[[i]]
  }

  invisible(value)
}


# nocov end
