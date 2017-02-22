#' Create a call by "hand"
#'
#' @param .fn Function to call. For \code{make_call}, either a string,
#'   a symbol or a quoted call. For \code{do_call}, a bare function
#'   name or call.
#' @param ...,.args Arguments to the call either in or out of a list
#' @seealso call_modify
#' @export
#' @examples
#' # fn can either be a string, a symbol or a call
#' new_call("f", a = 1)
#' new_call(quote(f), a = 1)
#' new_call(quote(f()), a = 1)
#'
#' #' Can supply arguments individually or in a list
#' new_call(quote(f), a = 1, b = 2)
#' new_call(quote(f), .args = list(a = 1, b = 2))
new_call <- function(.fn, ..., .args = list()) {
  if (is_character(.fn)) {
    if (length(.fn) != 1) {
      abort("Character `.fn` must be length 1")
    }
    .fn <- as_symbol(.fn)
  }

  args <- c(list(...), as.list(.args))
  as.call(c(.fn, args))
}

#' Modify the arguments of a call.
#'
#' @param .call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression. If not supplied, the calling frame is used.
#' @param ...,.args Named or unnamed expressions (constants, names or
#'   calls) used to modify the call. Use \code{NULL} to remove
#'   arguments.
#' @seealso new_call
#' @export
#' @examples
#' call <- quote(mean(x, na.rm = TRUE))
#'
#' # Modify an existing argument
#' call_modify(call, na.rm = FALSE)
#' call_modify(call, x = quote(y))
#'
#' # Remove an argument
#' call_modify(call, na.rm = NULL)
#'
#' # Add a new argument
#' call_modify(call, trim = 0.1)
#'
#' # Add an explicit missing argument
#' call_modify(call, na.rm = quote(expr = ))
#'
#' # Supply a list of new arguments with .args
#' newargs <- list(na.rm = NULL, trim = 0.1)
#' call_modify(call, .args = newargs)
#'
#' # If the call is missing, the parent frame is used instead.
#' f <- function(bool = TRUE) call_modify(.args = list(bool = FALSE))
#' f()
call_modify <- function(.call = caller_frame(), ..., .args = list()) {
  stopifnot(is_list(.args))
  args <- c(list(...), .args)

  call <- as_tidy_quote(.call, caller_env())
  expr <- f_rhs(call)
  f_rhs(call) <- switchpatch(expr,
    symbol = new_call(expr),
    language = expr,
    abort("`.call` must be a quote of a call or symbol")
  )

  call <- call_standardise(call)

  # Named arguments can be spliced by R
  named <- have_names(args)
  for (nm in names(args)[named]) {
    call[[nm]] <- args[[nm]]
  }

  if (any(!named)) {
    # Duplicate list structure in case it wasn't before
    if (!any(named)) {
      call <- duplicate(call, shallow = TRUE)
    }

    remaining_args <- as.pairlist(args[!named])
    call <- lsp_append(call, remaining_args)
  }

  call
}

#' Standardise a call.
#'
#' This is essentially equivalent to \code{\link[base]{match.call}()},
#' but handles primitive functions more gracefully.
#'
#' @param call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression. If not supplied, the calling frame is used.
#' @seealso \code{\link{call_homogenise}()} for a version more
#'   suitable to language analysis.
#' @export
call_standardise <- function(call = caller_frame()) {
  call <- as_tidy_quote(call, caller_env())

  # The call name might be a literal, not necessarily a symbol
  fn <- call_name(call)
  fn <- switchpatch(fn,
    character = get(fn, envir = f_env(call), mode = "function", inherits = TRUE),
    builtin = ,
    special = ,
    closure = fn,
    abort("Cannot extract a function to compare the call to")
  )

  match.call(as_closure(fn), expr(call))
}

#' Extract function from a call
#'
#' If a frame or formula, the function will be retrieved from their
#' environment. Otherwise, it is looked up in the calling frame.
#'
#' @inheritParams call_standardise
#' @export
#' @seealso \code{\link{call_name}}()
#' @examples
#' # Extract from a quoted call:
#' call_fn(~matrix())
#' call_fn(quote(matrix()))
#'
#' # Extract the calling function
#' test <- function() call_fn()
#' test()
call_fn <- function(call = caller_frame()) {
  if (is_frame(call)) {
    return(call$fn)
  }

  call <- as_tidy_quote(call, caller_env())
  expr <- f_rhs(call)

  switchpatch(expr,
    language = expr_eval(car(expr), f_env(call)),
    abort("`call` must quote a call")
  )
}

#' Extract function name of a call
#'
#' @inheritParams call_standardise
#' @return A string with the function name, or \code{NULL} if the
#'   function is anonymous.
#' @seealso \code{\link{call_fn}}()
#' @export
#' @examples
#' # Extract the function name from quoted calls:
#' call_name(~foo(bar))
#' call_name(quote(foo(bar)))
#'
#' # The calling expression is used as default:
#' foo <- function(bar) call_name()
#' foo(bar)
#'
#' # Namespaced calls are correctly handled:
#' call_name(~base::matrix(baz))
#'
#' # Anonymous and subsetted functions return NULL:
#' call_name(~foo$bar())
#' call_name(~foo[[bar]]())
#' call_name(~foo()())
call_name <- function(call = caller_frame()) {
  call <- as_expr(call)

  if (!is_call(call)) {
    abort("`call` must be a call or a tidy quote of a call")
  }

  fn <- car(call)

  switchpatch(fn,
    symbol =
      as_character(fn),
    language =
      if (identical(fn[[1]], quote(`::`)) ||
          identical(fn[[1]], quote(`:::`))) {
        # Namespaced calls: foo::bar(), foo:::bar()
        as_character(fn[[3]])
      } else {
        # Subsetted calls: foo@bar(), foo$bar()
        # Anomymous calls: foo[[bar]](), foo()()
        NULL
      },
    NULL # Some calls have a literal as CAR
  )
}

#' Extract arguments from a call
#'
#' @inheritParams call_standardise
#' @return A named list of arguments. The \code{_lsp} version returns
#'   a named pairlist.
#' @seealso \code{\link{fn_fmls}()} and
#'   \code{\link{fn_fmls_names}()}
#' @export
#' @examples
#' call <- quote(f(a, b))
#'
#' # Subsetting a call returns the arguments in a language pairlist:
#' call[-1]
#'
#' # Whereas call_args() returns a list:
#' call_args(call)
#'
#' # When the call arguments are supplied without names, a vector of
#' # empty strings is supplied (rather than NULL):
#' call_args_names(call)
call_args <- function(call = caller_frame()) {
  call <- as_expr(call)
  args <- as.list(call_args_lsp(call))
  set_names((args), names2(args))
}

#' @rdname call_args
#' @export
call_args_lsp <- function(call = caller_frame()) {
  call <- as_expr(call)
  stopifnot(is_call(call))
  cdr(call)
}

#' @rdname call_args
#' @export
call_args_names <- function(call = caller_frame()) {
  call <- as_expr(call)
  names2(call_args_lsp(call))
}

#' Inspect a call.
#'
#' This function is useful for quick testing and debugging when you
#' manipulate expressions and calls. It lets you check that a function
#' is called with the right arguments. This can be useful in unit
#' tests for instance. Note that this is just a simple wrapper around
#' \code{\link[base]{match.call}()}.
#'
#' @param ... Arguments to display in the returned call.
#' @export
#' @examples
#' call_inspect(foo(bar), "" %>% identity())
#' invoke(call_inspect, list(a = mtcars, b = letters))
call_inspect <- function(...) match.call()
