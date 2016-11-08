#' Call stack information
#'
#' The \code{eval_} and \code{call_} families of functions provide a
#' replacement for the base R functions prefixed with \code{sys.}
#' (which are all about the context stack), as well as for
#' \code{\link{parent.frame}()} (which is the only base R function for
#' querying the call stack). The context stack includes all R-level
#' evaluation contexts. It is linear in terms of execution history but
#' due to lazy evaluation it is potentially nonlinear in terms of call
#' history. The call stack history, on the other hand, is homogenous.
#' See \code{vignette("stack")} for more information.
#'
#' \code{eval_frame()} and \code{call_frame()} return a \code{frame}
#' object containing the following fields: \code{expr} and \code{env}
#' (call expression and evaluation environment), \code{pos} and
#' \code{caller_pos} (position of current frame in the context stack
#' and position of the caller), and \code{fun} (function of the
#' current frame). \code{eval_stack()} and \code{call_stack()} return
#' a list of all context or call frames on the stack. Finally,
#' \code{eval_depth()} and \code{call_depth()} report the current
#' context position or the number of calling frames on the stack.
#'
#' The base R functions take two sorts of arguments to indicate which
#' frame to query: \code{which} and \code{n}. The \code{n} argument is
#' straightforward: it's the number of frames to go down the stack,
#' with \code{n = 1} referring to the current context. The
#' \code{which} argument is more complicated and changes meaning for
#' values lower than 1. For the sake of consistency, the lazyeval
#' functions all take the same kind of argument \code{n}. This
#' argument has a single meaning (the number of frames to go down the
#' stack) and cannot be lower than 1.
#'
#' @param n The number of frames to go back in the stack.
#' @name stack
#' @examples
#' # Expressions within arguments count as contexts
#' identity(identity(eval_depth())) # returns 2
#'
#' # But they are not part of the call stack because arguments are
#' # evaluated within the calling function (or the global environment
#' # if called at top level)
#' identity(identity(call_depth())) # returns 0
#'
#' # The context stacks includes all intervening execution frames. The
#' # call stack doesn't:
#' f <- function(x) identity(x)
#' f(f(eval_stack()))
#' f(f(call_stack()))
#'
#' g <- function(cmd) cmd()
#' f(g(eval_stack))
#' f(g(call_stack))
#'
#' # The lazyeval _stack() functions return a list of frame
#' # objects. Use purrr::transpose() or index a field with
#' # purrr::map()'s to extract a particular field from a stack:
#' if (requireNamespace("purrr", quietly = TRUE)) {
#'   stack <- f(f(call_stack()))
#'   purrr::map(stack, "env")
#'   purrr::transpose(stack)$expr
#' }
NULL


# Evaluation frames --------------------------------------------------

new_frame <- function(x) {
  structure(x, class = "frame")
}
#' @export
print.frame <- function(x, ...) {
  cat("<frame ", x$pos, "> (", x$caller_pos, ")\n", sep = "")

  expr <- deparse(x$expr)
  if (length(expr) > 1) {
    expr <- paste(expr[[1]], "<...>")
  }
  cat("  expr: ", expr, "\n", sep = "")
  cat("   env: ", format(x$env), "\n", sep = "")
}
#' Is object a frame?
#'
#' @param x Object to test
#' @export
is_frame <- function(x) {
  inherits(x, "frame")
}

#' @rdname stack
#' @export
eval_frame <- function(n = 1) {
  stopifnot(n > 0)
  pos <- sys.nframe() - n
  if (pos < 1) {
    stop("not that many frames on the stack", call. = FALSE)
  }

  new_frame(list(
    pos = pos,
    caller_pos = sys.parent(n + 1),
    expr = sys.call(-n),
    env = sys.frame(-n),
    fn = sys.function(-n),
    fn_name = call_fn(sys.call(-n))
  ))
}

trail_next_caller <- function(callers, i) {
  i <- callers[i]

  # Skip over the fake context created by eval()
  while (length(i) && is_primitive_eval(sys.function(i))) {
    n_ctxt <- length(callers)
    i <- n_ctxt - i + 1

    if (n_ctxt < i) {
      i <- 0
    } else {
      i <- callers[i]
    }
  }

  i
}

# Positions of frames in the call stack up to `n`
make_trail <- function(callers, n = NULL, trim_global = TRUE) {
  n_ctxt <- length(callers)
  if (is.null(n)) {
    n <- n_ctxt
  } else {
    n <- n + 1
  }
  if (n > n_ctxt) {
    stop("not that many frames on the stack", call. = FALSE)
  }

  i <- trail_next_caller(callers, 1)
  j <- 1
  if (!length(i) || i == 0) {
    return(integer(0))
  }

  # Preallocate a sufficiently large vector
  out <- integer(n)
  out[j] <- i

  while (i != 0 && j < n) {
    j <- j + 1
    i <- trail_next_caller(callers, n_ctxt - i + 1)
    out[j] <- i
  }

  # Return relevant subset
  if (trim_global) {
    j <- j - 1
  }
  out[seq_len(j)]
}

#' @rdname stack
#' @export
call_frame <- function(n = 1) {
  stopifnot(n > 0)
  eval_callers <- eval_stack_callers()
  trail <- make_trail(eval_callers, n + 1, trim_global = FALSE)
  pos <- trail[n]

  new_frame(list(
    pos = pos,
    caller_pos = trail[n + 1],
    expr = sys.call(pos),
    env = sys.frame(pos),
    fn = sys.function(pos),
    fn_name = call_fn(sys.call(pos))
  ))
}

#' @rdname stack
#' @export
eval_depth <- function() {
  sys.nframe() - 1
}
#' @rdname stack
#' @export
call_depth <- function() {
  eval_callers <- eval_stack_callers()
  trail <- make_trail(eval_callers)
  length(trail)
}


# Summaries ----------------------------------------------------------

#' @rdname stack
#' @export
eval_stack <- function() {
  stack_data <- list(
    pos = eval_stack_trail(),
    caller_pos = eval_stack_callers(),
    expr = eval_stack_exprs(),
    env = eval_stack_envs(),
    fn = eval_stack_fns()
  )
  stack_data$fn_name <- lapply(stack_data$expr, call_fn)

  # Remove eval_stack() from stack
  stack_data <- lapply(stack_data, drop_first)

  frames <- zip(stack_data)
  lapply(frames, new_frame)
}

eval_stack_trail <- function() {
  pos <- sys.nframe() - 1
  seq(pos, 1)
}
eval_stack_exprs <- function() {
  exprs <- sys.calls()
  rev(drop_last(exprs))
}
eval_stack_envs <- function(n = 1) {
  envs <- sys.frames()
  rev(drop_last(envs))
}
eval_stack_callers <- function() {
  callers <- sys.parents()
  rev(drop_last(callers))
}
eval_stack_fns <- function() {
  pos <- sys.nframe()
  eval_indices <- seq_len(pos - 1)
  lapply(eval_indices, sys.function)
}

#' @rdname stack
#' @export
call_stack <- function() {
  eval_callers <- eval_stack_callers()
  trail <- make_trail(eval_callers)

  stack_data <- list(
    pos = trail,
    caller_pos = c(trail[-1], 0),
    expr = lapply(trail, sys.call),
    env = lapply(trail, sys.frame),
    fn = lapply(trail, sys.function)
  )
  stack_data$fn_name <- lapply(stack_data$expr, call_fn)

  frames <- zip(stack_data)
  lapply(frames, new_frame)
}
