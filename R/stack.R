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
#' Note finally that \code{parent.frame(1)} corresponds to
#' \code{call_frame(2)$env}, as \code{n = 1} always refers to the
#' current frame. This makes the \code{_frame()} and \code{_stack()}
#' functions consistent: \code{eval_frame(2)} is the same as
#' \code{eval_stack()[[2]]}. Also, \code{eval_depth()} returns one
#' more frame than \code{\link[base]{sys.nframe}()} because it counts
#' the global frame. That is consistent with the \code{_stack()}
#' functions which return the global frame as well. This way,
#' \code{call_stack(call_depth())} is the same as
#' \code{global_frame()}.
#'
#' @param n The number of frames to go back in the stack.
#' @param clean Whether to post-process the call stack to clean
#'   non-standard frames. If \code{TRUE}, the two frames created by
#'   the base function \code{\link[base]{eval}()} are merged together.
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
  cat("<frame ", x$pos, ">", sep = "")
  if (!x$pos) {
    cat(" [global]\n")
  } else {
    cat(" (", x$caller_pos, ")\n", sep = "")
  }

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
global_frame <- function() {
  new_frame(list(
    pos = 0L,
    caller_pos = NA_integer_,
    expr = NULL,
    env = globalenv(),
    fn = NULL,
    fn_name = NULL
  ))
}

#' @rdname stack
#' @export
eval_frame <- function(n = 1) {
  stopifnot(n > 0)
  pos <- sys.nframe() - n

  if (pos < 0L) {
    stop("not that many frames on the stack", call. = FALSE)
  } else if (pos == 0L) {
    global_frame()
  } else {
    new_frame(list(
      pos = pos,
      caller_pos = sys.parent(n + 1),
      expr = sys.call(-n),
      env = sys.frame(-n),
      fn = sys.function(-n),
      fn_name = call_fn_name(sys.call(-n))
    ))
  }
}

# Positions of frames in the call stack up to `n`
trail_make <- function(callers, n = NULL, clean = TRUE) {
  n_ctxt <- length(callers)
  if (is.null(n)) {
    n_max <- n_ctxt
  } else {
    if (n > n_ctxt) {
      stop("not that many frames on the evaluation stack", call. = FALSE)
    }
    n_max <- n + 1
  }

  state <- trail_next(callers, 1, clean)
  if (!length(state$i) || state$i == 0) {
    return(0L)
  }
  j <- 1

  # Preallocate a sufficiently large vector
  out <- integer(n_max)
  out[j] <- state$i

  while (state$i != 0 && j < n_max) {
    j <- j + 1
    n_ctxt <- length(state$callers)
    next_pos <- n_ctxt - state$i + 1
    state <- trail_next(state$callers, next_pos, clean)
    out[j] <- state$i
  }

  # Return relevant subset
  if (!is.null(n) && n > j) {
    stop("not that many frames on the call stack", call. = FALSE)
  }
  out[seq_len(j)]
}

trail_next <- function(callers, i, clean) {
  if (i == 0L) {
    return(list(callers = callers, i = 0L))
  }

  i <- callers[i]

  if (clean) {
    # base::Recall() creates a custom context with the wrong sys.parent()
    if (identical(sys.function(i - 1L), base::Recall)) {
      i_pos <- trail_index(callers, i)
      callers[i_pos] <- i - 1L
    }

    # The R-level eval() creates two contexts. We skip the second one
    if (length(i) && is_prim_eval(sys.function(i))) {
      n_ctxt <- length(callers)
      special_eval_pos <- trail_index(callers, i)
      callers <- callers[-special_eval_pos]
      i <- i - 1L
    }

  }

  list(callers = callers, i = i)
}

trail_index <- function(callers, i) {
  n_ctxt <- length(callers)
  n_ctxt - i + 1L
}

#' @rdname stack
#' @export
call_frame <- function(n = 1, clean = TRUE) {
  stopifnot(n > 0)

  eval_callers <- eval_stack_callers()
  trail <- trail_make(eval_callers, n, clean = clean)
  pos <- trail[n]

  if (identical(pos, 0L)) {
    return(global_frame())
  }

  frame <- new_frame(list(
    pos = pos,
    caller_pos = trail[n + 1],
    expr = sys.call(pos),
    env = sys.frame(pos),
    fn = sys.function(pos),
    fn_name = call_fn_name(sys.call(pos))
  ))

  if (clean) {
    frame <- frame_clean_eval(frame)
  }
  frame
}

# The _depth() functions count the global frame as well

#' @rdname stack
#' @export
eval_depth <- function() {
  sys.nframe()
}
#' @rdname stack
#' @export
call_depth <- function() {
  eval_callers <- eval_stack_callers()
  trail <- trail_make(eval_callers)
  length(trail)
}


# Summaries ----------------------------------------------------------

#' @rdname stack
#' @export
eval_stack <- function(n = NULL) {
  stack_data <- list(
    pos = eval_stack_trail(),
    caller_pos = eval_stack_callers(),
    expr = eval_stack_exprs(),
    env = eval_stack_envs(),
    fn = eval_stack_fns()
  )

  # Remove eval_stack() from stack
  stack_data <- lapply(stack_data, drop_first)

  stack_data <- stack_subset(stack_data, n)
  stack_data$fn_name <- lapply(stack_data$expr, call_fn_name)

  stack <- zip(stack_data)
  stack <- lapply(stack, new_frame)

  if (is.null(n) || (length(n) && n > length(stack))) {
    stack <- c(stack, list(global_frame()))
  }

  structure(stack, class = c("eval_stack", "stack"))
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
  pos <- sys.nframe() - 1
  lapply(seq(pos, 1), sys.function)
}

stack_subset <- function(stack_data, n) {
  if (length(n)) {
    stopifnot(n > 0)
    n_stack <- length(stack_data[[1]])
    if (n == n_stack + 1) {
      # We'll add the global frame later
      n <- n <- n - 1
    } else if (n > n_stack + 1) {
      stop("not that many frames on the stack", call. = FALSE)
    }
    stack_data <- lapply(stack_data, `[`, seq_len(n))
  }
  stack_data
}

#' @rdname stack
#' @export
call_stack <- function(n = NULL, clean = TRUE) {
  eval_callers <- eval_stack_callers()
  trail <- trail_make(eval_callers, n, clean = clean)

  stack_data <- list(
    pos = drop_last(trail),
    caller_pos = drop_first(trail),
    expr = lapply(trail, sys.call),
    env = lapply(trail, sys.frame),
    fn = lapply(trail, sys.function)
  )
  stack_data$fn_name <- lapply(stack_data$expr, call_fn_name)

  stack <- zip(stack_data)
  stack <- lapply(stack, new_frame)
  if (clean) {
    stack <- lapply(stack, frame_clean_eval)
    stack <- lapply_around(stack, "right", frame_clean_Recall)
  }

  if (trail[length(trail)] == 0L) {
    stack <- c(stack, list(global_frame()))
  }

  structure(stack, class = c("call_stack", "stack"))
}

frame_clean_eval <- function(frame) {
  if (identical(frame$fn, base::eval)) {
    # Use the environment from the context created in do_eval()
    # (the context with the fake primitive call)
    stopifnot(is_prim_eval(sys.function(frame$pos + 1)))
    frame$env <- sys.frame(frame$pos + 1)
  }

  frame
}
frame_clean_Recall <- function(frame, next_frame) {
  if (!is_missing(next_frame) && identical(next_frame$fn, base::Recall)) {
    call_recalled <- call_standardise(frame, enum_dots = TRUE, add_missings = TRUE)
    args_recalled <- call_args(call_recalled)
    args_Recall <- call_args(next_frame)

    if (!length(args_Recall)) {
      args_Recall <- lapply(names(args_recalled), as.symbol)
      names(args_Recall) <- dots_enumerate(args_Recall)
      set_cdr(next_frame$expr, as.pairlist(args_Recall))
    }

    args_recalled <- dots_enumerate_args(as.pairlist(args_recalled))
    set_cdr(frame$expr, args_recalled)
  }

  frame
}

#' Is object a stack?
#' @param x An object to test
#' @export
is_stack <- function(x) inherits(x, "stack")

#' @rdname is_stack
#' @export
is_eval_stack <- function(x) inherits(x, "eval_stack")

#' @rdname is_stack
#' @export
is_call_stack <- function(x) inherits(x, "call_stack")

#' @export
`[.stack` <- function(x, i) {
  structure(NextMethod(), class = class(x))
}

# Handles global_frame() whose `caller_pos` is NA
sys_frame <- function(n) {
  if (is.na(n)) {
    NULL
  } else {
    sys.frame(n)
  }
}

#' Find the position of a frame on the evaluation stack.
#'
#' The position is the number of frames on the evaluation stack
#' (\code{\link{eval_stack}()}), counting from the global frame (whose
#' position is zero). See also \code{\link{frame_distance}()} for a
#' function that counts frames from the opposite side (the current
#' context).
#'
#' While this function returns the position of the frame on the
#' evaluation stack, it can safely be called with intervening frames
#' as those will be discarded.
#'
#' @param frame The environment of a frame. Can be any object with a
#'   \code{\link{env}()} method. Note that for frame objects, this
#'   simply returns \code{frame$pos}.
#' @seealso \code{\link{frame_distance}()}
#' @export
#' @examples
#' fn <- function() g(environment())
#' g <- function(env) frame_position(env)
#'
#' # frame_position() returns the position of the frame on the evaluation stack:
#' fn()
#' identity(identity(fn()))
#'
#' # Note that it trims off intervening calls before counting so you
#' # can safely nest it within other calls:
#' g <- function(env) identity(identity(frame_position(env)))
#' fn()
frame_position <- function(frame) {
  if (is_frame(frame)) {
    return(frame$pos)
  }
  frame <- env(frame)

  stack <- stack_trim(eval_stack(), n = 2)
  envs <- pluck(stack, "env")

  i <- 1
  for (env in envs) {
    if (identical(env, frame)) {
      return(length(envs) - i)
    }
    i <- i + 1
  }

  stop("this environment is not on the stack", call. = FALSE)
}

#' Compute the distance between a frame and the current context.
#'
#' Whereas \code{\link{frame_position}()} returns the number of
#' evaluation contexts counting from the global frame,
#' \code{frame_distance()} counts from the currently active frame. The
#' returned distance can be directly supplied to
#' \code{\link{eval_frame}()} to get the corresponding frame object.
#'
#' @param frame Either a frame object whose \code{pos} field will be
#'   extracted, an object with an \code{\link{env}()} method whose
#'   environment will be looked up on the evaluation stack with
#'   \code{\link{frame_position}()}, or an integer representing the
#'   position of the frame on the stack.
#' @seealso \code{\link{frame_position}()}
#' @export
#' @examples
#' fn <- function() g(environment())
#' g <- function(env) h(env)
#' h <- function(env) frame_distance(env)
#' fn()
#'
#' # frame_distance() takes care of intervening contexts and can be
#' # safely nested in other calls:
#' h <- function(env) identity(identity(frame_distance(env)))
#' fn()
frame_distance <- function(frame) {
  if (is_numeric(frame)) {
    pos <- frame
  } else {
    pos <- frame_position(frame)
  }
  caller_pos <- call_frame(2)$pos
  caller_pos - pos + 1
}


#' Trim top call layers from the evaluation stack.
#'
#' \code{\link{eval_stack}()} can be tricky to use in real code
#' because all intervening frames are returned with the stack,
#' including those at \code{eval_stack()} own call
#' site. \code{stack_trim()} makes it easy to remove layers of
#' intervening calls.
#'
#' @param stack An evaluation stack.
#' @param n The number of call frames (not eval frames) to trim off
#'   the top of the stack. In other words, the number of layers of
#'   intervening frames to trim.
#' @export
#' @examples
#' # Intervening frames appear on the evaluation stack:
#' identity(identity(eval_stack()))
#'
#' # stack_trim() will trim the first n layers of calls:
#' stack_trim(identity(identity(eval_stack())))
#'
#' # Note that it also takes care of calls intervening at its own call
#' # site:
#' identity(identity(
#'   stack_trim(identity(identity(eval_stack())))
#' ))
#'
#' # It is especially useful when used within a function that needs to
#' # inspect the evaluation stack but should nonetheless be callable
#' # within nested calls without side effects:
#' stack_util <- function() {
#'   # n = 2 means that two layers of intervening calls should be removed
#'   stack <- stack_trim(eval_stack(), n = 2)
#'   stack
#' }
#' user_fn <- function() {
#'   # A user calls your stack utility with intervening frames:
#'   identity(identity(stack_util()))
#' }
#' # These intervening frames won't appear in the evaluation stack
#' identity(user_fn())
stack_trim <- function(stack, n = 1) {
  # Add 1 to discard stack_trim()'s own intervening frames
  caller_pos <- call_frame(n + 1, clean = FALSE)$pos

  n_frames <- length(stack)
  n_skip <- n_frames - caller_pos
  stack[seq(n_skip, n_frames)]
}
