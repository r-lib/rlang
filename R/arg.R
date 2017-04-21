#' Match an argument to a character vector
#'
#' @description
#'
#' This is equivalent to [base::match.arg()] with a few differences:
#'
#' * Partial matches trigger an error.
#'
#' * Error messages are a bit more informative and obey the tidyverse
#'   standards.
#'
#' @param arg A symbol referring to an argument accepting strings.
#' @param values The possible values that `arg` can take. If `NULL`,
#'   the values are taken from the function definition of the [caller
#'   frame][caller_frame].
#' @return The string supplied to `arg`.
#' @export
#' @examples
#' fn <- function(x = c("foo", "bar")) arg_match(x)
#' fn("bar")
#'
#' # This would throw an informative error if run:
#' # fn("b")
#' # fn("baz")
arg_match <- function(arg, values = NULL) {
  arg_expr <- enexpr(arg)
  if (!is_symbol(arg_expr)) {
    abort("Internal error: `arg_match()` expects a symbol")
  }

  arg_nm <- as_string(arg_expr)

  if (is_null(values)) {
    fn <- caller_fn()
    values <- fn_fmls(fn)[[arg_nm]]
    values <- eval_bare(values, get_env(fn))
  }
  if (!is_character(values)) {
    abort("Internal error: `values` must be a character vector")
  }
  if (!is_character(arg)) {
    abort(paste0(chr_quoted(arg_nm), " must be a character vector"))
  }

  arg <- arg[[1]]
  i <- match(arg, values)

  if (is_na(i)) {
    msg <- paste0(chr_quoted(arg_nm), " should be one of: ")
    msg <- paste0(msg, chr_enumerate(chr_quoted(values, "\"")))

    i_partial <- pmatch(arg, values)
    if (!is_na(i_partial)) {
      candidate <- values[[i_partial]]
      candidate <- chr_quoted(candidate, "\"")
      msg <- paste0(msg, "\n", "Did you mean ", candidate, "?")
    }

    abort(msg)
  }

  values[[i]]
}

chr_quoted <- function(chr, type = "`") {
  paste0(type, chr, type)
}
chr_enumerate <- function(chr, sep = ", ") {
  if (length(chr) < 2) {
    return(chr)
  }
  n <- length(chr)
  head <- chr[seq_len(n - 1)]
  last <- chr[length(chr)]

  head <- paste(head, collapse = ", ")
  paste(head, "or", last)
}

#' Generate or handle a missing argument
#'
#' These functions help using the missing argument as a regular R
#' object. It is valid to generate a missing argument and assign it in
#' the current environment or in a list. However, once assigned in the
#' environment, the missing argument normally cannot be touched.
#' `maybe_missing()` checks whether the object is the missing
#' argument, and regenerate it if needed to prevent R from throwing a
#' missing error. In addition, `is_missing()` lets you check for a
#' missing argument in a larger range of situations than
#' [base::missing()] (see examples).
#' @param x An object that might be the missing argument.
#' @export
#' @examples
#' # The missing argument can be useful to generate calls
#' quo(f(x = !! missing_arg()))
#' quo(f(x = !! NULL))
#'
#'
#' # It is perfectly valid to generate and assign the missing
#' # argument.
#' x <- missing_arg()
#' l <- list(missing_arg())
#'
#' # Note that accessing a missing argument contained in a list does
#' # not trigger an error:
#' l[[1]]
#' is.null(l[[1]])
#'
#' # But if the missing argument is assigned in the current
#' # environment, it is no longer possible to touch it. The following
#' # lines would all return errors:
#' #> x
#' #> is.null(x)
#'
#' # In these cases, you can use maybe_missing() to manipulate an
#' # object that might be the missing argument without triggering a
#' # missing error:
#' maybe_missing(x)
#' is.null(maybe_missing(x))
#' is_missing(maybe_missing(x))
#'
#'
#' # base::missing() does not work well if you supply an
#' # expression. The following lines would throw an error:
#'
#' #> missing(missing_arg())
#' #> missing(l[[1]])
#'
#' # while is_missing() will work as expected:
#' is_missing(missing_arg())
#' is_missing(l[[1]])
missing_arg <- function() {
  quote(expr = )
}

#' @rdname missing_arg
#' @export
is_missing <- function(x) {
  expr <- substitute(x)
  if (typeof(expr) == "symbol" && missing(x)) {
    TRUE
  } else {
    identical(x, missing_arg())
  }
}

#' @rdname missing_arg
#' @export
maybe_missing <- function(x) {
  if (is_missing(x) || quo_is_missing(x)) {
    missing_arg()
  } else {
    x
  }
}
