#' Create a call by "hand"
#'
#' @param .fn Function to call. For \code{make_call}, either a string,
#'   a symbol or a quoted call. For \code{do_call}, a bare function
#'   name or call.
#' @param ...,.args Arguments to the call either in or out of a list
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
#'   \item Does not partial-match arguments.
#' }
#' @param call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression. If not supplied, the calling frame is used.
#' @param env Environment in which to look up call the function
#'   definition and the contents of \code{...}. If not supplied, it is
#'   retrieved from \code{call} if the latter is a frame object or a
#'   formula.
#' @param fn The function against which to standardise the call. It is
#'   lookep up in \code{env} if not supplied, or retrieved from
#'   \code{call} if it is a frame.
#' @param enum_dots Whether to standardise the names of dotted
#'   arguments. If \code{TRUE}, this produces calls such as
#'   \code{f(..1 = ..2)} instead of \code{f(..2)}. The first form is
#'   mostly intended for code analysis tools as it makes it easier to
#'   climb arguments through nested calls. The latter form (the
#'   default) is more faithful to the actual call and is ready to be
#'   evaluated.
#' @export
call_standardise <- function(call = NULL, env = NULL, fn = NULL,
                             enum_dots = FALSE) {
  info <- call_info(call, env, call_frame(2))
  fn <- call_fn(info$call, info$env)

  stopifnot(is.environment(info$env))
  call_validate_args(info$call, info$env, fn)

  call <- duplicate(info$call)
  call <- call_inline_dots(call, info$env, enum_dots)

  call_match(call, fn, enum_dots)
}

call_info <- function(call, env, frame) {
  call <- call %||% frame
  if (is_frame(call)) {
    env <- env %||% call$env
    call <- call$expr
  } else if (is_formula(call)) {
    env <- env %||% environment(call)
    call <- f_rhs(call)
  } else {
    env <- env %||% parent.frame(2)
  }

  list(
    call = call,
    env = env
  )
}

call_inline_dots <- function(call, env, enum_dots) {
  d <- lsp_walk_nonnull(call, function(arg) {
    if (identical(cadr(arg), quote(...))) arg
  })

  if (!is.null(d)) {
    dots <- dots_get(env)
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
  }

  call
}

call_validate_args <- function(call, env, fn) {
  body(fn) <- NULL
  call[[1]] <- quote(fn)

  # Forward dots here
  assign("...", env$`...`)

  # Delegate to R the task of validating arguments
  # This does not force promises since we evaluate a bodyless fn
  eval(call)
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
  formals_nms <- names2(formals(fn))

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

#' Modify the arguments of a call.
#'
#' @inheritParams call_standardise
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
#'
#' # If the call is missing, the parent frame is used instead.
#' f <- function(bool = TRUE) call_modify(new_args = list(bool = FALSE))
#' f()
call_modify <- function(call = NULL, new_args, env = NULL) {
  stopifnot(is.list(new_args))
  call <- call %||% call_frame(2)
  call <- call_standardise(call, env)

  if (!all(has_names(new_args))) {
    stop("All new arguments must be named", call. = FALSE)
  }

  for (nm in names(new_args)) {
    call[[nm]] <- new_args[[nm]]
  }
  call
}

#' Extract function from a call
#'
#' @inheritParams call_standardise
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
    info <- call_info(call, env, call_frame(2))
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
  info <- call_info(call, NULL, call_frame(2))
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
