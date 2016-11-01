#' Call stack information
#'
#' The \code{ctxt_} and \code{call_} families of functions provide a
#' replacement for the base R functions prefixed with \code{sys.}
#' (which are all about the context stack), as well as for
#' \code{\link{parent.frame}()} (which is the only base R function for
#' querying the call stack). The context stack includes all R-level
#' evaluation contexts. It is linear in terms of execution history but
#' due to lazy evaluation it is potentially nonlinear in terms of call
#' history. The call stack on the other hand has a homogeneous call
#' history. See the vignette on execution contexts for more
#' information.
#'
#' \code{ctxt_frame()} and \code{call_frame()} return a \code{frame}
#' object containing the following fields: \code{expr} and \code{env}
#' (call expression and evaluation environment), \code{pos} and
#' \code{caller} (position of current frame in the context stack and
#' position of the caller), and \code{fun} (function of the current
#' frame). \code{ctxt_stack()} and \code{call_stack()} return a list
#' of all context or call frames on the stack. Finally,
#' \code{ctxt_depth()} and \code{call_depth()} report the current
#' context position or the number of calling frames on the stack.
#'
#' The base R functions take two sorts of arguments to indicate which
#' frame to query: \code{which} and \code{n}. The \code{n} argument is
#' straightforward: it's the number of frames to go back in the stack,
#' with \code{n = 1} referring to the current context. The
#' \code{which} argument is more complicated and changes meaning for
#' values lower than 1. For the sake of consistency, the lazyeval
#' functions all take the same kind of argument \code{n}. This
#' argument has a single meaning (the number of frames to go back in
#' the stack) and cannot be lower than 1.
#'
#' @param n The number of frames to go back in the stack.
#' @name stack
#' @examples
#' # Expressions within arguments count as contexts
#' identity(identity(ctxt_depth())) # returns 2
#'
#' # But they are not part of the call stack because arguments are
#' # evaluated within the calling function (or the global environment
#' # if called at top level)
#' identity(identity(call_depth())) # returns 0
#'
#' # The context stacks includes all intervening execution frames. The
#' # call stack doesn't:
#' f <- function(x) identity(x)
#' f(f(ctxt_stack()))
#' f(f(call_stack()))
#'
#' g <- function(cmd) cmd()
#' f(g(ctxt_stack))
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
  cat("<frame ", x$pos, "> (", x$caller, ")\n", sep = "")

  expr <- deparse(x$expr)
  if (length(expr) > 1) {
    expr <- paste(expr[[1]], "<...>")
  }
  cat("  expr: ", expr, "\n", sep = "")
  cat("   env: ", format(x$env), "\n", sep = "")
}

#' @rdname stack
#' @export
ctxt_frame <- function(n = 1) {
  stopifnot(n > 0)
  pos <- sys.nframe() - n
  if (pos < 1) {
    stop("not that many frames on the stack", call. = FALSE)
  }

  new_frame(list(
    pos = pos,
    expr = sys.call(-n),
    env = sys.frame(-n),
    caller = sys.parent(n + 1),
    fun = sys.function(-n)
  ))
}

# Positions of frames in the call stack up to `n`
make_trail <- function(callers, n = NULL) {
  n_ctxt <- length(callers)
  if (is.null(n)) {
    n <- n_ctxt
  } else {
    n <- n + 1
  }
  if (n > n_ctxt) {
    stop("not that many frames on the stack", call. = FALSE)
  }

  i <- callers[1]
  j <- 1
  if (!length(i) || i == 0) {
    return(integer(0))
  }

  # Preallocate a sufficiently large vector
  out <- integer(n)
  out[j] <- i

  while (i != 0 && j < n) {
    j <- j + 1
    i <- callers[n_ctxt - i + 1]
    out[j] <- i
  }

  # Return relevant subset, ignoring global frame
  out[seq_len(j - 1)]
}

#' @rdname stack
#' @export
call_frame <- function(n = 1) {
  stopifnot(n > 0)
  ctxt_callers <- ctxt_stack_callers()
  trail <- make_trail(ctxt_callers, n)
  pos <- trail[n]

  new_frame(list(
    pos = pos,
    expr = sys.call(pos),
    env = parent.frame(n + 1),
    caller = sys.parent(pos),
    fun = sys.function(pos)
  ))
}

#' @rdname stack
#' @export
ctxt_depth <- function() {
  sys.nframe() - 1
}
#' @rdname stack
#' @export
call_depth <- function() {
  ctxt_callers <- ctxt_stack_callers()
  trail <- make_trail(ctxt_callers)
  length(trail)
}


# Summaries ----------------------------------------------------------

#' @rdname stack
#' @export
ctxt_stack <- function() {
  contexts <- list(
    expr = ctxt_stack_exprs(),
    env = ctxt_stack_envs(),
    pos = ctxt_stack_trail(),
    caller = ctxt_stack_callers(),
    fun = ctxt_stack_funs()
  )

  # Remove ctxt_stack() from stack
  contexts <- lapply(contexts, drop_first)

  frames <- zip(contexts)
  lapply(frames, new_frame)
}

ctxt_stack_trail <- function() {
  pos <- sys.nframe() - 1
  seq(pos, 1)
}
ctxt_stack_exprs <- function() {
  exprs <- sys.calls()
  rev(drop_last(exprs))
}
ctxt_stack_envs <- function(n = 1) {
  envs <- sys.frames()
  rev(drop_last(envs))
}
ctxt_stack_callers <- function() {
  if (identical(parent.frame(1), globalenv())) {
    # sys.parents() returns integer(0) at top level. Avoid this
    # inconsistency with ctxt_caller().
    #
    # Note that this cannot be unit-tested because there is no way to
    # simulate a top-level call from testthat. The following called at
    # top level should return 0 0 0:
    #
    #   identity(identity(identity(ctxt_stack_callers())))
    rep(0L, sys.nframe() - 1)
  } else {
    callers <- sys.parents()
    rev(drop_last(callers))
  }
}
ctxt_stack_funs <- function() {
  pos <- sys.nframe()
  ctxt_indices <- seq_len(pos - 1)
  lapply(ctxt_indices, sys.function)
}

#' @rdname stack
#' @export
call_stack <- function() {
  ctxt_callers <- ctxt_stack_callers()
  trail <- make_trail(ctxt_callers)

  info <- list(
    expr = lapply(trail, sys.call),
    env = lapply(trail, sys.frame),
    pos = trail,
    caller = c(trail[-1], 0),
    fun = lapply(trail, sys.function)
  )

  frames <- zip(info)
  lapply(frames, new_frame)
}
