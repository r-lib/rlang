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
#' @export
call_standardise <- function(call = NULL, fn = NULL,
                             caller_env = NULL,
                             enum_dots = FALSE) {
  info <- call_info(call, caller_env)
  fn <- fn %||% info$fn %||% call_fn(info$call, info$env)
  stopifnot(is.environment(info$env))
  call_standardise_(info$call, fn, info$caller_env, enum_dots)
}

call_info <- function(call, caller_env) {
  # Assume call_info() is never called directly
  call <- call %||% call_frame(3)
  fn <- NULL

  if (is_frame(call)) {
    env <- call$env
    caller_env <- caller_env %||% sys.frame(call$caller_pos)
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

call_standardise_ <- function(call, fn, caller_env, enum_dots) {
  call <- duplicate(call)
  call <- call_inline_dots(call, caller_env, enum_dots)
  call <- call_match_partial(call, fn)
  call <- call_match(call, fn, enum_dots)
  call
}

arg_match_partial <- function(arg, formal) {
  formal <- substr(formal, 1, nchar(arg))
  arg == formal
}

call_match_partial <- function(call, fn) {
  actuals_nms <- call_args_names(call)
  if (!length(actuals_nms)) {
    return(call)
  }

  is_empty <- actuals_nms == ""
  is_dup <- duplicated(actuals_nms) & !is_empty
  if (any(is_dup)) {
    dups_nms <- actuals_nms[which(is_dup)]
    stop(call. = FALSE,
      "formal arguments matched by multiple actual arguments: ",
      paste0(dups_nms, collapse = ", "))
  }

  formals_nms <- fn_fmls_names(fn)
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
  if (enum_dots) {
    dots <- dots_enumerate_argnames(dots)
  }

  # Attach remaining args to expanded dots
  remaining_args <- cddr(d)
  lsp_walk_nonnull(dots, function(arg) {
    if (is.null(cdr(arg))) set_cdr(arg, remaining_args)
  })

  # Replace dots symbol with actual dots and remaining args
  set_cdr(d, dots)

  call
}

is_arg_matched <- function(arg, formals, enum_dots) {
  is_empty <- arg == ""
  if (enum_dots) {
    !is_empty && arg %in% formals
  } else {
    !is_empty
  }
}

call_match <- function(call, fn, enum_dots) {
  args <- call[-1]
  args_nms <- names2(args)
  formals_nms <- names2(fn_fmls(fn))

  is_matched <- vapply_lgl(args_nms, is_arg_matched, formals_nms, enum_dots)
  candidates <- setdiff(formals_nms, args_nms[is_matched])
  n_actuals <- sum(!is_matched)

  dots_i <- which(candidates == "...")
  n_dots <- max(0, n_actuals - dots_i + 1)
  if (length(dots_i) && n_dots) {
    # Ignore everything on the right of dots
    candidates <- candidates[seq(1, dots_i)]

    # Expand dots names
    if (enum_dots) {
      dots_nms <- paste0("..", seq_len(n_dots))
    } else {
      dots_nms <- rep("", n_dots)
    }

    candidates[dots_i] <- dots_nms[1]
    candidates <- append(candidates, dots_nms[-1], after = dots_i)
  }

  n_formals <- length(candidates)
  stopifnot(n_formals >= n_actuals)

  args_nms[!is_matched] <- candidates[seq_len(n_actuals)]
  names(call) <- c("", args_nms)

  call
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
#' call_new("f", a = 1)
#' call_new(quote(f), a = 1)
#' call_new(quote(f()), a = 1)
#'
#' #' Can supply arguments individually or in a list
#' call_new(quote(f), a = 1, b = 2)
#' call_new(quote(f), .args = list(a = 1, b = 2))
call_new <- function(.fn, ..., .args = list()) {
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
#' @seealso call_new
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

  if (!all(has_names(args))) {
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
    eval(info$call[[1]], info$env)
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
      fn <- fn[[3]]
    } else {
      # Subsetted calls: foo@bar(), foo$bar()
      # Anomymous calls: foo[[bar]](), foo()()
      return(NULL)
    }
  }

  as.character(fn)
}

#' Extract arguments from a call
#'
#' @inheritParams call_standardise
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
  args <- as.list(call[-1])
  set_names(args, names2(args))
}

#' @rdname call_args
#' @export
call_args_names <- function(call = NULL) {
  call <- call_info(call, NULL)$call
  names(call_args(call))
}
