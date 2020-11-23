# nocov start --- compat-zeallot --- 2020-11-23

# This drop-in file implements a simple version of zeallot::`%<-%`.
# Please find the most recent version in rlang's repository.


`%<-%` <- function(lhs, rhs) {
  lhs <- substitute(lhs)
  env <- caller_env()

  if (!is_call(lhs, "c")) {
    abort("The LHS of `%<-%` must be a `c()` call.")
  }

  vars <- as.list(lhs[-1])

  if (length(rhs) < length(vars)) {
    abort("The RHS of `%<-%` must be long enough for the number of assigned variables.")
  }

  for (i in seq_along(vars)) {
    var <- vars[[i]]
    if (!is_symbol(var)) {
      abort("The LHS of `%<-%` must refer to symbols.")
    }

    env[[as_string(var)]] <- rhs[[i]]
  }

  invisible(rhs)
}


# nocov end
