#' Call stack information
#'
#' The \code{ctxt_} and \code{call_} families of functions provide a
#' replacement for the base R functions prefixed with \code{sys.}, as
#' well as for \code{\link{parent.frame}()}. While the base
#' \code{sys.___()} functions are all about the context stack,
#' \code{\link{parent.frame}()} is the only base R function for
#' querying the call stack. The context stack includes all R-level
#' evaluation contexts. It is linear in terms of execution history but
#' due to lazy evaluation it is potentially nonlinear in terms of call
#' history. The call stack on the other hand is homogeneous in this
#' regard. See the vignette on execution contexts for more
#' information.
#'
#' \itemize{
#'   \item The \code{_expr()}, \code{_env()}, \code{_caller()} and
#'         \code{_fun()} functions report about the call expression,
#'         the calling environment, and the caller function
#'         respectively.
#'   \item The \code{_pos()} and \code{_caller()} functions report
#'         about the callee and caller positions in the stack.
#'   \item The plural \code{_stack_} functions such as
#'         \code{call_stack_exprs()} report about the whole call stack
#'         from the global context to the currently active
#'         context. The \code{_trail} suffix is the plural of
#'         \code{_pos} (e.g., \code{call_stack_trail()} is the plural
#'         of \code{call_pos()}).
#'   \item Finally, the \code{ctxt_stack()} and \code{call_stack()}
#'         methods provide a summary of the context and call stacks
#'         with a handy \code{print()} method.
#' }
#'
#' The base R functions take two sorts of arguments to indicate which
#' frame to query: \code{which} and \code{n}. The \code{n} argument is
#' straightforward: it's the number of frames to go back in the stack,
#' with \code{n = 1} referring to the current context. The
#' \code{which} argument are more complicated and changes meaning for
#' values lower than 1. For the sake of consistency, the lazyeval
#' functions all take the same kind of argument \code{n}. This
#' argument has a single meaning (the number of frames to go back in
#' the stack) and cannot be lower than 1.
#'
#' @param n The number of frames to go back in the stack.
#' @name stack
#' @examples
#' # Expressions within arguments count as contexts
#' identity(identity(ctxt_pos())) # returns 2
#'
#' # But they are not part of the call stack because arguments are
#' # evaluated within the calling function (or the global environment
#' # if called at top level)
#' identity(identity(call_pos())) # returns 0
#'
#' # The context stacks includes all intervening execution frames. The
#' # call stack doesn't:
#' f <- function(x) identity(x)
#' g <- function(cmd) cmd()
#' f(g(ctxt_stack))
#' f(g(call_stack))
NULL


# Context stack ------------------------------------------------------

#' @rdname stack
#' @export
ctxt_stack_trail <- function() {
  pos <- sys.nframe() - 1
  seq(pos, 1)
}
#' @rdname stack
#' @export
ctxt_stack_exprs <- function() {
  exprs <- sys.calls()
  rev(drop_last(exprs))
}
#' @rdname stack
#' @export
ctxt_stack_envs <- function(n = 1) {
  envs <- sys.frames()
  rev(drop_last(envs))
}
#' @rdname stack
#' @export
#' @examples
#'
#' # When called at top level, the output of ctxt_stack_callers() is
#' # more consistent than that of sys.parents():
#' identity(identity(identity(sys.parents())))
#' identity(identity(identity(ctxt_stack_callers())))
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
#' @rdname stack
#' @export
ctxt_stack_funs <- function() {
  pos <- ctxt_pos()
  ctxt_indices <- seq_len(pos - 1)
  lapply(ctxt_indices, sys.function)
}

#' @rdname stack
#' @export
ctxt_pos <- function() {
  sys.nframe() - 1
}
#' @rdname stack
#' @export
ctxt_expr <- function(n = 1) {
  stopifnot(n > 0)
  sys.call(-n)
}
#' @rdname stack
#' @export
ctxt_env <- function(n = 1) {
  stopifnot(n > 0)
  sys.frame(-n)
}
#' @rdname stack
#' @export
ctxt_caller <- function(n = 1) {
  stopifnot(n > 0)
  sys.parent(n + 1)
}
#' @rdname stack
#' @export
ctxt_fun <- function(n = 1) {
  stopifnot(n > 0)
  sys.function(-n)
}


# Call stack ---------------------------------------------------------

# Positions of frames in the call stack up to `n`
make_trail <- function(callers, n = NULL) {
  n_ctxt <- length(callers)
  if (is.null(n)) {
    n <- n_ctxt
  } else {
    n <- n + 1
  }
  if (n > n_ctxt) {
    stop("Not that many frames", call. = FALSE)
  }

  i <- callers[1]
  j <- 1
  if (!length(i) || i == 0) {
    return(0)
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
call_stack_trail <- function() {
  callers <- ctxt_stack_callers()
  make_trail(callers)
}
#' @rdname stack
#' @export
call_stack_envs <- function() {
  trail <- call_stack_trail()
  trail <- drop_first(trail)
  lapply(trail, sys.frame)
}
#' @rdname stack
#' @export
call_stack_exprs <- function() {
  trail <- call_stack_trail()
  trail <- drop_first(trail)
  lapply(trail, sys.call)
}
#' @rdname stack
#' @export
call_stack_callers <- function() {
  trail <- call_stack_trail()
  trail <- drop_first(trail)
  c(trail[-1], 0)
}
#' @rdname stack
#' @export
call_stack_funs <- function() {
  trail <- call_stack_trail()
  trail <- drop_first(trail)
  lapply(trail, sys.function)
}

#' @rdname stack
#' @export
call_pos <- function() {
  length(call_stack_trail()) - 1
}
#' @rdname stack
#' @export
call_env <- function(n = 1) {
  stopifnot(n > 0)
  parent.frame(n + 1)
}
#' @rdname stack
#' @export
call_expr <- function(n = 1) {
  stopifnot(n > 0)
  callers <- ctxt_stack_callers()
  trail <- make_trail(callers, n)
  sys.call(trail[n])
}
#' @rdname stack
#' @export
call_caller <- function(n = 1) {
  stopifnot(n > 0)
  callers <- ctxt_stack_callers()
  trail <- make_trail(callers, n)
  sys.parent(trail[n])
}
#' @rdname stack
#' @export
call_fun <- function(n = 1) {
  stopifnot(n > 0)
  callers <- ctxt_stack_callers()
  trail <- make_trail(callers, n)
  sys.function(trail[n])
}


# Summaries ----------------------------------------------------------

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
ctxt_stack <- function() {
  contexts <- list(
    expr = ctxt_stack_exprs(),
    env = ctxt_stack_envs(),
    pos = ctxt_stack_trail(),
    caller = ctxt_stack_callers()
  )

  # Remove ctxt_stack() from stack
  contexts <- lapply(contexts, drop_first)

  frames <- zip(contexts)
  lapply(frames, new_frame)
}

#' @rdname stack
#' @export
call_stack <- function() {
  contexts <- list(
    expr = call_stack_exprs(),
    env = call_stack_envs(),
    pos = call_stack_trail(),
    caller = call_stack_callers()
  )

  # Remove call_stack() from stack
  contexts <- lapply(contexts, drop_first)

  frames <- zip(contexts)
  lapply(frames, new_frame)
}
