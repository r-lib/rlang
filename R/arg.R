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
#' `arg_match()` derives the possible values from the
#' [caller function][caller_fn].
#'
#' @param arg A symbol referring to an argument accepting strings.
#' @param values A character vector of possible values that `arg` can take.
#' @param ... These dots are for future extensions and must be empty.
#' @param multiple Whether `arg` may contain zero or several values.
#' @inheritParams args_error_context
#' @return The string supplied to `arg`.
#' @importFrom utils adist
#' @seealso [check_required()]
#' @export
#' @examples
#' fn <- function(x = c("foo", "bar")) arg_match(x)
#' fn("bar")
#'
#' # Throws an informative error for mismatches:
#' try(fn("b"))
#' try(fn("baz"))
arg_match <- function(
  arg,
  values = NULL,
  ...,
  multiple = FALSE,
  error_arg = caller_arg(arg),
  error_call = caller_env()
) {
  check_dots_empty0(...)

  arg_expr <- enexpr(arg)
  error_arg <- as_string(error_arg)

  check_symbol(arg_expr, arg = "arg")
  check_character(arg, arg = error_arg, call = error_call)

  if (is_null(values)) {
    fn <- caller_fn()
    values <- formals(fn)[[error_arg]]
    values <- eval_bare(values, get_env(fn))
  }

  if (multiple) {
    return(arg_match_multi(arg, values, error_arg, error_call))
  }

  if (!length(arg)) {
    msg <- sprintf(
      "%s must be length 1, not length 0",
      format_arg(error_arg)
    )
    abort(msg, call = error_call, arg = error_arg)
  }
  if (length(arg) > 1 && !setequal(arg, values)) {
    msg <- arg_match_invalid_msg(arg, values, error_arg)
    abort(msg, call = error_call, arg = error_arg)
  }

  arg <- arg[[1]]
  arg_match0(
    arg,
    values,
    error_arg,
    error_call = error_call
  )
}

arg_match_multi <- function(arg, values, error_arg, error_call) {
  map_chr(arg, ~ arg_match0(.x, values, error_arg, error_call = error_call))
}

#' @description
#' `arg_match0()` is a bare-bones version if performance is at a premium.
#' It requires a string as `arg` and explicit character `values`.
#' For convenience, `arg` may also be a character vector containing
#' every element of `values`, possibly permuted.
#' In this case, the first element of `arg` is used.
#'
#' @rdname arg_match
#' @param arg_nm Same as `error_arg`.
#' @export
#' @examples
#'
#' # Use the bare-bones version with explicit values for speed:
#' arg_match0("bar", c("foo", "bar", "baz"))
#'
#' # For convenience:
#' fn1 <- function(x = c("bar", "baz", "foo")) fn3(x)
#' fn2 <- function(x = c("baz", "bar", "foo")) fn3(x)
#' fn3 <- function(x) arg_match0(x, c("foo", "bar", "baz"))
#' fn1()
#' fn2("bar")
#' try(fn3("zoo"))
arg_match0 <- function(
  arg,
  values,
  arg_nm = caller_arg(arg),
  error_call = caller_env()
) {
  .External(ffi_arg_match0, arg, values, environment())
}

chr_interpolate <- function(x) {
  paste0(deparse(x), collapse = "")
}

stop_arg_match <- function(arg, values, error_arg, error_call) {
  if (length(arg) > 1) {
    sorted_arg <- sort(unique(arg))
    sorted_values <- sort(unique(values))
    if (!identical(sorted_arg, sorted_values)) {
      msg <- sprintf(
        "%s must be length 1 or a permutation of %s.",
        format_arg("arg"),
        format_code(chr_interpolate(values))
      )
      abort(msg, call = error_call, arg = "arg")
    }
  }

  if (is_na(arg)) {
    check_string(arg, arg = error_arg, call = error_call)
  }

  msg <- arg_match_invalid_msg(arg, values, error_arg)

  # Try suggest the most probable and helpful candidate value
  candidate <- NULL
  i_partial <- pmatch(arg, values)
  if (!is_na(i_partial)) {
    candidate <- values[[i_partial]]
  }

  i_close <- adist(arg, values) / nchar(values)
  if (any(i_close <= 0.5)) {
    candidate <- values[[which.min(i_close)]]
  }

  if (is_null(candidate)) {
    # Make case-insensitive match only after failed case-sensitive one to be
    # more helpful in certain edge cases. For example,
    # `arg_match0("aa", c("AA", "aA"))`: here "aA" is the closest candidate.
    i_close_nocase <- adist(arg, values, ignore.case = TRUE) / nchar(values)
    if (any(i_close_nocase <= 0.5)) {
      candidate <- values[[which.min(i_close_nocase)]]
    }
  }

  if (!is_null(candidate)) {
    candidate <- chr_quoted(candidate, "\"")
    msg <- c(msg, i = paste0("Did you mean ", candidate, "?"))
  }

  abort(msg, call = error_call, arg = error_arg)
}

arg_match_invalid_msg <- function(val, values, error_arg) {
  msg <- paste0(format_arg(error_arg), " must be one of ")
  msg <- paste0(msg, oxford_comma(chr_quoted(values, "\"")))

  if (is_null(val)) {
    msg <- paste0(msg, ".")
  } else {
    msg <- paste0(msg, sprintf(', not "%s\".', val[[1]]))
  }

  msg
}

#' Check that argument is supplied
#'
#' Throws an error if `x` is missing.
#'
#' @param x A function argument. Must be a symbol.
#' @inheritParams args_error_context
#'
#' @seealso [arg_match()]
#' @examples
#' f <- function(x)  {
#'   check_required(x)
#' }
#'
#' # Fails because `x` is not supplied
#' try(f())
#'
#' # Succeeds
#' f(NULL)
#' @export
check_required <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!missing(x)) {
    return(invisible(TRUE))
  }

  arg_expr <- substitute(x)
  if (!is_symbol(arg_expr)) {
    abort(sprintf("%s must be an argument name.", format_arg("x")))
  }

  msg <- sprintf("%s is absent but must be supplied.", format_arg(arg))
  abort(msg, call = call)
}

chr_quoted <- function(chr, type = "`") {
  paste0(type, chr, type)
}

#' Check that arguments are mutually exclusive
#'
#' `check_exclusive()` checks that only one argument is supplied out of
#' a set of mutually exclusive arguments. An informative error is
#' thrown if multiple arguments are supplied.
#'
#' @param ... Function arguments.
#' @param .require Whether at least one argument must be supplied.
#' @param .frame Environment where the arguments in `...` are defined.
#' @inheritParams args_error_context
#' @return The supplied argument name as a string. If `.require` is
#'   `FALSE` and no argument is supplied, the empty string `""` is
#'   returned.
#'
#' @examples
#' f <- function(x, y) {
#'   switch(
#'     check_exclusive(x, y),
#'     x = message("`x` was supplied."),
#'     y = message("`y` was supplied.")
#'   )
#' }
#'
#' # Supplying zero or multiple arguments is forbidden
#' try(f())
#' try(f(NULL, NULL))
#'
#' # The user must supply one of the mutually exclusive arguments
#' f(NULL)
#' f(y = NULL)
#'
#'
#' # With `.require` you can allow zero arguments
#' f <- function(x, y) {
#'   switch(
#'     check_exclusive(x, y, .require = FALSE),
#'     x = message("`x` was supplied."),
#'     y = message("`y` was supplied."),
#'     message("No arguments were supplied")
#'   )
#' }
#' f()
#' @export
check_exclusive <- function(
  ...,
  .require = TRUE,
  .frame = caller_env(),
  .call = .frame
) {
  args <- enexprs(..., .named = TRUE)
  if (length(args) < 2) {
    abort("Must supply at least two arguments.")
  }
  if (!every(args, is_symbol)) {
    abort("`...` must be function arguments.")
  }

  present <- map_lgl(args, ~ inject(!base::missing(!!.x), .frame))
  n_present <- sum(present)

  if (n_present == 0) {
    if (.require) {
      args <- map(names(args), format_arg)
      enum <- oxford_comma(args)
      msg <- sprintf("One of %s must be supplied.", enum)
      abort(msg, call = .call)
    } else {
      return("")
    }
  }

  if (n_present == 1) {
    return(as_string(args[[which(present)]]))
  }

  args <- map_chr(names(args), format_arg)
  enum <- oxford_comma(args)
  msg <- sprintf("Exactly one of %s must be supplied.", enum)

  if (n_present != length(args)) {
    enum <- oxford_comma(args[present], final = "and")
    msg <- c(msg, x = sprintf("%s were supplied together.", enum))
  }

  abort(msg, call = .call)
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
#'   lists. It is also more consistent with default arguments which
#'   are never treated as missing (see section below).
#'
#' * `maybe_missing()` is useful to pass down an input that might be
#'   missing to another function, potentially substituting by a
#'   default value. It avoids triggering an "argument is missing" error.
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
#' @section `is_missing()` and default arguments:
#'
#' The base function [missing()] makes a distinction between default
#' values supplied explicitly and default values generated through a
#' missing argument:
#'
#' ```{r}
#' fn <- function(x = 1) base::missing(x)
#'
#' fn()
#' fn(1)
#' ```
#'
#' This only happens within a function. If the default value has been
#' generated in a calling function, it is never treated as missing:
#'
#' ```{r}
#' caller <- function(x = 1) fn(x)
#' caller()
#' ```
#'
#' `rlang::is_missing()` simplifies these rules by never treating
#' default arguments as missing, even in internal contexts:
#'
#' ```{r}
#' fn <- function(x = 1) rlang::is_missing(x)
#'
#' fn()
#' fn(1)
#' ```
#'
#' This is a little less flexible because you can't specialise
#' behaviour based on implicitly supplied default values. However,
#' this makes the behaviour of `is_missing()` and functions using it
#' simpler to understand.
#'
#'
#' @section Fragility of the missing argument object:
#'
#' The missing argument is an object that triggers an error if and
#' only if it is the result of evaluating a symbol. No error is
#' produced when a function call evaluates to the missing argument
#' object. For instance, it is possible to bind the missing argument
#' to a variable with an expression like `x[[1]] <- missing_arg()`.
#' Likewise, `x[[1]]` is safe to use as argument, e.g. `list(x[[1]])`
#' even when the result is the missing object.
#'
#' However, as soon as the missing argument is passed down between
#' functions through a bare variable, it is likely to cause a missing
#' argument error:
#'
#' ```r
#' x <- missing_arg()
#' list(x)
#' #> Error:
#' #> ! argument "x" is missing, with no default
#' ```
#'
#' To work around this, `is_missing()` and `maybe_missing(x)` use a
#' bit of magic to determine if the input is the missing argument
#' without triggering a missing error.
#'
#' ```r
#' x <- missing_arg()
#' list(maybe_missing(x))
#' #> [[1]]
#' #>
#' ```
#'
#' `maybe_missing()` is particularly useful for prototyping
#' meta-programming algorithms in R. The missing argument is a likely
#' input when computing on the language because it is a standard
#' object in formals lists. While C functions are always allowed to
#' return the missing argument and pass it to other C functions, this
#' is not the case on the R side. If you're implementing your
#' meta-programming algorithm in R, use `maybe_missing()` when an
#' input might be the missing argument object.
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
  .Call(ffi_missing_arg)
}

#' @rdname missing_arg
#' @export
is_missing <- function(x) {
  missing(x) || identical(x, quote(expr = ))
}

#' @rdname missing_arg
#' @param default The object to return if the input is missing,
#'   defaults to `missing_arg()`.
#' @export
maybe_missing <- function(x, default = missing_arg()) {
  if (is_missing(x)) {
    default
  } else {
    x
  }
}
