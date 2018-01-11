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
chr_enumerate <- function(chr, sep = ", ", final = "or") {
  if (length(chr) < 2) {
    return(chr)
  }
  n <- length(chr)
  head <- chr[seq_len(n - 1)]
  last <- chr[length(chr)]

  head <- paste(head, collapse = ", ")
  paste(head, final, last)
}

#' Generate or handle a missing argument
#'
#' @description
#'
#' These functions help using the missing argument as a regular R
#' object.
#'
#' * `missing_arg()` generates a missing argument.
#'
#' * `is_missing()` is like [base::missing()] but also supports
#'   testing for missing arguments contained in other objects like
#'   lists.
#'
#' * `maybe_missing()` is useful to pass down an input that might be
#'   missing to another function. It avoids triggering an
#'   "argument is missing" error.
#'
#'
#' @section Other ways to reify the missing argument:
#'
#' * `base::quote(expr = )` is the canonical way to create a missing
#'   argument object.
#'
#' * `expr()` called without argument creates a missing argument.
#'
#' * `quo()` called without argument creates an empty quosure, i.e. a
#'   quosure containing the missing argument object.
#'
#'
#' @section Fragility of the missing argument object:
#'
#' The missing argument is an object that triggers an error if and
#' only if it is the result of evaluating a symbol. No error is
#' produced when a function call evaluates to the missing argument
#' object. This means that expressions like `x[[1]] <- missing_arg()`
#' are perfectly safe. Likewise, `x[[1]]` is safe even if the result
#' is the missing object.
#'
#' However, as soon as the missing argument is passed down between
#' functions through an argument, you're at risk of triggering a
#' missing error. This is because arguments are passed through
#' symbols. To work around this, `is_missing()` and `maybe_missing(x)`
#' use a bit of magic to determine if the input is the missing
#' argument without triggering a missing error.
#'
#' `maybe_missing()` is particularly useful for prototyping
#' meta-programming algorithm in R. The missing argument is a likely
#' input when computing on the language because it is a standard
#' object in formals lists. While C functions are always allowed to
#' return the missing argument and pass it to other C functions, this
#' is not the case on the R side. If you're implementing your
#' meta-programming algorithm in R, use `maybe_missing()` when an
#' input might be the missing argument object.
#'
#'
#' @section Life cycle:
#'
#' * `missing_arg()` and `is_missing()` are stable.
#' * Like the rest of rlang, `maybe_missing()` is maturing.
#'
#' @param x An object that might be the missing argument.
#' @export
#' @examples
#' # The missing argument usually arises inside a function when the
#' # user omits an argument that does not have a default:
#' fn <- function(x) is_missing(x)
#' fn()
#'
#' # Creating a missing argument can also be useful to generate calls
#' args <- list(1, missing_arg(), 3, missing_arg())
#' quo(fn(!!! args))
#'
#' # Other ways to create that object include:
#' quote(expr = )
#' expr()
#'
#' # It is perfectly valid to generate and assign the missing
#' # argument in a list.
#' x <- missing_arg()
#' l <- list(missing_arg())
#'
#' # Just don't evaluate a symbol that contains the empty argument.
#' # Evaluating the object `x` that we created above would trigger an
#' # error.
#' # x  # Not run
#'
#' # On the other hand accessing a missing argument contained in a
#' # list does not trigger an error because subsetting is a function
#' # call:
#' l[[1]]
#' is.null(l[[1]])
#'
#' # In case you really need to access a symbol that might contain the
#' # empty argument object, use maybe_missing():
#' maybe_missing(x)
#' is.null(maybe_missing(x))
#' is_missing(maybe_missing(x))
#'
#'
#' # Note that base::missing() only works on symbols and does not
#' # support complex expressions. For this reason the following lines
#' # would throw an error:
#'
#' #> missing(missing_arg())
#' #> missing(l[[1]])
#'
#' # while is_missing() will work as expected:
#' is_missing(missing_arg())
#' is_missing(l[[1]])
missing_arg <- function() {
  .Call(rlang_missing_arg)
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
  if (is_missing(x)) {
    missing_arg()
  } else {
    x
  }
}
