#' Standardise a call against formal arguments
#'
#' Compared to \code{\link{match.call}()}, \code{call_standardise()}:
#' \itemize{
#'   \item Always report dotted arguments as such: \code{call(..1, ..2)}.
#'         In comparison, \code{match.call()} inlines literals:
#'         \code{call("foo", 10)}.
#'   \item Provides an argument \code{enum_dots} to impose
#'         enumerated names on dotted arguments. This produces
#'         \code{call(..1 = x, ..2 = ..3)} instead of
#'         \code{call(foo = x, ..3)}.
#'   \item Does not sort arguments according to the order of
#'         appearance in the function definition.
#'   \item Standardises missing arguments as well if you specify
#'         \code{add_missings}: \code{call(x = , y = , )}.
#' }
#' @param call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression. If not supplied, the calling frame is used.
#' @param fn The function against which to standardise the call. If
#'   not supplied, it is either retrieved from \code{call} if the
#'   latter is frame or formula (by looking up the formula's
#'   environment). Alternatively, it is lookep up in the calling frame
#'   if not supplied.
#' @param caller_env Parent frame in which to look up call the
#'   contents of \code{...}. If not supplied and \code{call} is a
#'   frame object, it is retrieved from \code{call}.
#' @param enum_dots Whether to standardise the names of dotted
#'   arguments. If \code{TRUE}, this produces calls such as
#'   \code{f(..1 = ..2)} instead of \code{f(..2)}. The first form is
#'   mostly intended for code analysis tools as it makes it easier to
#'   climb arguments through nested calls. The latter form (the
#'   default) is more faithful to the actual call and is ready to be
#'   evaluated.
#' @param add_missings Whether to standardise missing arguments.
#' @export
call_standardise <- function(call = NULL, fn = NULL,
                             caller_env = NULL,
                             enum_dots = FALSE,
                             add_missings = FALSE) {
  info <- call_info(call, caller_env)
  if (is.null(info$call)) return(NULL)

  fn <- fn %||% info$fn %||% call_fn(info$call, info$env)
  stopifnot(is.call(info$call))
  stopifnot(is.environment(info$env))
  call_standardise_(info$call, fn, info$caller_env, enum_dots, add_missings)
}

call_info <- function(call, caller_env) {
  # Assume call_info() is never called directly
  call <- call %||% call_frame(3)
  fn <- NULL

  if (is_frame(call)) {
    env <- call$env
    caller_env <- caller_env %||% sys_frame(call$caller_pos)
    fn <- call$fn
    call <- call$expr
  } else if (is_formula(call)) {
    env <- environment(call)
    call <- f_rhs(call)
  } else {
    env <- parent.frame(3)
  }

  list(
    call = call,
    env = env,
    fn = fn,
    caller_env = caller_env
  )
}

call_standardise_ <- function(call, fn, caller_env, enum_dots, add_missings) {
  call <- duplicate(call)
  call <- call_inline_dots(call, caller_env, enum_dots)
  call <- call_match_partial(call, fn)
  call <- call_match(call, fn, enum_dots, add_missings)
  call
}

arg_match_partial <- function(arg, formal) {
  formal <- substr(formal, 1, nchar(arg))
  arg == formal
}

call_match_partial <- function(call, fn) {
  actuals_nms <- call_args_names(call)
  formals_nms <- fn_fmls_names(fn)

  is_empty <- actuals_nms == ""
  is_dup <- duplicated(actuals_nms) & !is_empty
  is_dup <- is_dup & actuals_nms %in% formals_nms
  if (any(is_dup)) {
    dups_nms <- actuals_nms[which(is_dup)]
    stop(call. = FALSE,
      "formal arguments matched by multiple actual arguments: ",
      paste0(dups_nms, collapse = ", "))
  }

  dots_pos <- match("...", formals_nms)

  # No partial-matching of args after dots
  if (!is.na(dots_pos)) {
    formals_nms <- formals_nms[seq_len(dots_pos) - 1]
  }
  formals_pool <- setdiff(formals_nms, actuals_nms)

  is_matched <- actuals_nms %in% formals_nms
  actuals_pat <- actuals_nms
  actuals_pat[is_matched | is_empty] <- NA

  matched <- list()
  for (formal in formals_pool) {
    matched_pos <- which(vapply_lgl(actuals_pat, arg_match_partial, formal))

    if (length(matched_pos) > 1) {
      stop(call. = FALSE,
        "formal argument `", formal,
        "` matched by multiple actual arguments")
    }
    if (length(matched_pos) && matched_pos %in% matched) {
      stop(call. = FALSE,
        "actual argument `", actuals_pat[matched_pos],
        "` matches multiple formal arguments")
    }

    matched <- append(matched, matched_pos)
    actuals_nms[matched_pos] <- formal
  }

  if (is.na(dots_pos)) {
    is_unused <- !actuals_nms %in% c(formals_nms, "")
    is_unused <- is_unused & !vapply_lgl(actuals_nms, is_dot_nm)
    if (any(is_unused)) {
      stop(call. = FALSE,
        "unused arguments: ",
        paste0(actuals_nms[is_unused], collapse = ", "))
    }
    if (length(actuals_nms) > length(formals_nms)) {
      stop("unused arguments", call. = FALSE)
    }
  }

  names(call) <- c("", actuals_nms)
  call
}

call_inline_dots <- function(call, caller_env, enum_dots) {
  d <- lsp_walk_nonnull(call, function(arg) {
    if (identical(cadr(arg), quote(...))) arg
  })
  if (is.null(d)) {
    return(call)
  }
  if (is.null(caller_env)) {
    stop(call. = FALSE, "`caller_env` must be supplied to match dots")
  }

  dots <- frame_dots_lsp(caller_env)
  dots <- dots_enumerate_args(dots)

  # Attach remaining args to expanded dots
  remaining_args <- cddr(d)
  lsp_walk_nonnull(dots, function(arg) {
    if (is.null(cdr(arg))) set_cdr(arg, remaining_args)
  })

  # Replace dots symbol with actual dots and remaining args
  set_cdr(d, dots)

  call
}

call_match <- function(call, fn, enum_dots, add_missings) {
  args <- call[-1]
  args_nms <- names2(args)
  fmls_nms <- names2(fn_fmls(fn))

  dots_i <- which(fmls_nms == "...")
  if (length(dots_i)) {
    args_nms <- call_match_dotted(args_nms, fmls_nms, dots_i, enum_dots)
  } else {
    args_nms <- call_match_args(args_nms, fmls_nms)
  }
  names(call) <- c("", args_nms)

  if (add_missings) {
    missing_nms <- setdiff(fmls_nms, c(args_nms, "..."))
    missing_args <- rep(list(arg_missing()), length(missing_nms))
    missing_args <- as.pairlist(set_names(missing_args, missing_nms))
    call <- lsp_append(call, missing_args)
  }

  call
}

call_match_dotted <- function(args_nms, fmls_nms, dots_i, enum_dots) {
  # First match formals on the left of dots
  is_unmatched <- vapply_lgl(args_nms, `==`, "")
  candidates <- fmls_nms[seq_len(dots_i - 1)]
  candidates <- setdiff(candidates, args_nms[!is_unmatched])
  args_nms[is_unmatched] <- call_match_args(args_nms[is_unmatched], candidates)

  if (enum_dots) {
    is_matched <- vapply_lgl(args_nms, `%in%`, fmls_nms)
    n_dots <- sum(!is_matched)
    args_nms[!is_matched] <- paste0("..", seq_len(n_dots))
  }

  args_nms
}

call_match_args <- function(args_nms, fmls_nms) {
  is_unmatched <- vapply_lgl(args_nms, `==`, "")

  # Only match up to the number of formals
  n_fmls <- length(setdiff(fmls_nms, "..."))
  n_args <- length(args_nms)
  if (n_args > n_fmls) {
    is_ignored <- rep(TRUE, n_args)
    is_ignored[seq_len(n_fmls)] <- FALSE
    is_unmatched <- is_unmatched & !is_ignored
  }

  candidates <- setdiff(fmls_nms, args_nms[!is_unmatched])
  args_nms[is_unmatched] <- candidates[seq_len(sum(is_unmatched))]
  args_nms
}

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
  if (is.character(.fn)) {
    if (length(.fn) != 1) {
      stop("Character `.fn` must be length 1", call. = FALSE)
    }
    .fn <- as.name(.fn)
  }

  args <- c(list(...), as.list(.args))
  as.call(c(.fn, args))
}

#' Modify the arguments of a call.
#'
#' @param .call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression. If not supplied, the calling frame is used.
#' @param .env Environment in which to look up call the function
#'   definition and the contents of \code{...}. If not supplied, it is
#'   retrieved from \code{call} if the latter is a frame object or a
#'   formula.
#' @param ...,.args Named expressions (constants, names or
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
call_modify <- function(.call = NULL, ..., .args = list(), .env = NULL) {
  stopifnot(is.list(.args))
  args <- c(list(...), .args)

  call <- .call %||% call_frame(2)
  call <- call_standardise(call, .env)

  if (!is_named(args)) {
    stop("All new arguments must be named", call. = FALSE)
  }

  for (nm in names(args)) {
    call[[nm]] <- args[[nm]]
  }
  call
}

#' Extract function from a call
#'
#' @inheritParams call_standardise
#' @param env Environment in which to look up the function. The
#'   default is the calling frame.
#' @export
#' @seealso \code{\link{call_fn_name}}()
#' @examples
#' # Extract from a quoted call:
#' call_fn(~matrix())
#' call_fn(quote(matrix()))
#'
#' # Extract the calling function
#' test <- function() call_fn()
#' test()
call_fn <- function(call = NULL, env = NULL) {
  if (is_frame(call)) {
    call$fn
  } else {
    info <- call_info(call, NULL)
    expr_eval(info$call[[1]], info$env)
  }
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
#' call_fn_name(~foo(bar))
#' call_fn_name(quote(foo(bar)))
#'
#' # The calling expression is used as default:
#' foo <- function(bar) call_fn_name()
#' foo(bar)
#'
#' # Namespaced calls are correctly handled:
#' call_fn_name(~base::matrix(baz))
#'
#' # Anonymous and subsetted functions return NULL:
#' call_fn_name(~foo$bar())
#' call_fn_name(~foo[[bar]]())
#' call_fn_name(~foo()())
call_fn_name <- function(call = NULL) {
  info <- call_info(call, NULL)
  stopifnot(is.call(info$call))

  fn <- info$call[[1]]

  if (is.call(fn)) {
    if (identical(fn[[1]], quote(`::`)) ||
        identical(fn[[1]], quote(`:::`))) {
      # Namespaced calls: foo::bar(), foo:::bar()
      return(as.character(fn[[3]]))
    } else {
      # Subsetted calls: foo@bar(), foo$bar()
      # Anomymous calls: foo[[bar]](), foo()()
      return(NULL)
    }
  }

  if (!is.symbol(fn)) {
    # Inlined closures: (function() {})()
    return(NULL)
  }

  as.character(fn)
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
call_args <- function(call = NULL) {
  call <- call_info(call, NULL)$call
  args <- as.list(call_args_lsp(call))
  set_names((args), names2(args))
}

#' @rdname call_args
#' @export
call_args_lsp <- function(call = NULL) {
  call <- call_info(call, NULL)$call
  stopifnot(is.call(call))
  cdr(call)
}

#' @rdname call_args
#' @export
call_args_names <- function(call = NULL) {
  call <- call_info(call, NULL)$call
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
