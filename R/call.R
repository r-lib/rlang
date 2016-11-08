#' Create a call by "hand"
#'
#' @param f Function to call. For \code{make_call}, either a string, a symbol
#'   or a quoted call. For \code{do_call}, a bare function name or call.
#' @param ...,.args Arguments to the call either in or out of a list
#' @export
#' @examples
#' # f can either be a string, a symbol or a call
#' call_new("f", a = 1)
#' call_new(quote(f), a = 1)
#' call_new(quote(f()), a = 1)
#'
#' #' Can supply arguments individually or in a list
#' call_new(quote(f), a = 1, b = 2)
#' call_new(quote(f), .args = list(a = 1, b = 2))
call_new <- function(f, ..., .args = list()) {
  if (is.character(f)) {
    if (length(f) != 1) {
      stop("Character `f` must be length 1", call. = FALSE)
    }
    f <- as.name(f)
  }

  args <- c(list(...), as.list(.args))
  as.call(c(f, args))
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
  call <- call %||% call_frame(2)
  if (is_frame(call)) {
    fn <- fn %||% call$fn
    env <- env %||% sys.frame(call$caller_pos)
    call <- call$expr
  } else if (is_formula(call)) {
    env <- env %||% environment(call)
    call <- f_rhs(call)
  } else {
    env <- env %||% parent.frame()
  }
  stopifnot(is.call(call))
  stopifnot(is.environment(env))

  fn <- call_get_fn(call, env, fn)

  call <- call_inline_dots(call, env, enum_dots)
  call_validate_args(call, fn)
  call_match(call, fn, enum_dots)
}

primitive_eval <- eval(quote(sys.function(0)))
is_primitive_eval <- function(x) identical(x, primitive_eval)

# This predicate handles the fake primitive eval function produced
# when evaluating code with eval()
is_primitive <- function(x) {
  is.primitive(x) || is_primitive_eval(x)
}

call_get_fn <- function(call, env, fn) {
  fn <- fn %||% eval(call[[1]], env)

  if (is_primitive(fn)) {
    fn_chr <- as.character(call[[1]])
    if (fn_chr == "eval") {
      # do_eval() starts a context with a fake primitive function as
      # function definition. We replace it here with the .Internal()
      # wrapper of eval() so we can match the arguments.
      fn <- base::eval
    } else {
      fn <- .ArgsEnv[[fn_chr]] %||% .GenericArgsEnv[[fn_chr]]
    }
  }
  if (typeof(fn) != "closure") {
    stop("`fn` is not a valid function", call. = FALSE)
  }

  fn
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

call_validate_args <- function(call, fn) {
  body(fn) <- NULL
  call[[1]] <- quote(fn)

  # Delegate to R the task of validating arguments
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

get_call <- function(call) {
  if (is_frame(call)) {
    call$expr
  } else if (is_formula(call)) {
    f_rhs(call)
  } else {
    call
  }
}

#' Function name of a call
#'
#' @inheritParams call_standardise
#' @return A string with the function name.
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
#' call_fn_name(~foo$bar(baz))
call_fn_name <- function(call = NULL) {
  call <- call %||% call_frame(2)
  call <- get_call(call)
  stopifnot(is.call(call))
  fn <- call[[1]]

  # Handle namespaced calls: foo::bar(), foo@bar(), foo$bar()
  if (is.call(fn)) {
    fn <- fn[[3]]
  }

  as.character(fn)
}
