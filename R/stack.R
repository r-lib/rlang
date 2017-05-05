#' Call stack information
#'
#' The `eval_` and `call_` families of functions provide a replacement
#' for the base R functions prefixed with `sys.` (which are all about
#' the context stack), as well as for [parent.frame()] (which is the
#' only base R function for querying the call stack). The context
#' stack includes all R-level evaluation contexts. It is linear in
#' terms of execution history but due to lazy evaluation it is
#' potentially nonlinear in terms of call history. The call stack
#' history, on the other hand, is homogenous. See `vignette("stack")`
#' for more information.
#'
#' `ctxt_frame()` and `call_frame()` return a `frame` object
#' containing the following fields: `expr` and `env` (call expression
#' and evaluation environment), `pos` and `caller_pos` (position of
#' current frame in the context stack and position of the caller), and
#' `fun` (function of the current frame). `ctxt_stack()` and
#' `call_stack()` return a list of all context or call frames on the
#' stack. Finally, `ctxt_depth()` and `call_depth()` report the
#' current context position or the number of calling frames on the
#' stack.
#'
#' The base R functions take two sorts of arguments to indicate which
#' frame to query: `which` and `n`. The `n` argument is
#' straightforward: it's the number of frames to go down the stack,
#' with `n = 1` referring to the current context. The `which` argument
#' is more complicated and changes meaning for values lower than
#' 1. For the sake of consistency, the lazyeval functions all take the
#' same kind of argument `n`. This argument has a single meaning (the
#' number of frames to go down the stack) and cannot be lower than 1.
#'
#' Note finally that `parent.frame(1)` corresponds to
#' `call_frame(2)$env`, as `n = 1` always refers to the current
#' frame. This makes the `_frame()` and `_stack()` functions
#' consistent: `ctxt_frame(2)` is the same as `ctxt_stack()[[2]]`.
#' Also, `ctxt_depth()` returns one more frame than
#' [base::sys.nframe()] because it counts the global frame. That is
#' consistent with the `_stack()` functions which return the global
#' frame as well. This way, `call_stack(call_depth())` is the same as
#' `global_frame()`.
#'
#' @param n The number of frames to go back in the stack.
#' @param clean Whether to post-process the call stack to clean
#'   non-standard frames. If `TRUE`, suboptimal call-stack entries by
#'   [base::eval()] will be cleaned up: the duplicate frame created by
#'   `eval()` is eliminated.
#' @param trim The number of layers of intervening frames to trim off
#'   the stack. See [stack_trim()] and examples.
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
#'
#' # stack <- f(f(call_stack()))
#' # purrr::map(stack, "env")
#' # purrr::transpose(stack)$expr
#'
#' # current_frame() is an alias for ctxt_frame(1)
#' fn <- function() list(current = current_frame(), first = ctxt_frame(1))
#' fn()
#'
#' # While current_frame() is the top of the stack, global_frame() is
#' # the bottom:
#' fn <- function() {
#'   n <- ctxt_depth()
#'   ctxt_frame(n)
#' }
#' identical(fn(), global_frame())
#'
#'
#' # ctxt_stack() returns a stack with all intervening frames. You can
#' # trim layers of intervening frames with the trim argument:
#' identity(identity(ctxt_stack()))
#' identity(identity(ctxt_stack(trim = 1)))
#'
#' # ctxt_stack() is called within fn() with intervening frames:
#' fn <- function(trim) identity(identity(ctxt_stack(trim = trim)))
#' fn(0)
#'
#' # We can trim the first layer of those:
#' fn(1)
#'
#' # The outside intervening frames (at the fn() call site) are still
#' # returned, but can be trimmed as well:
#' identity(identity(fn(1)))
#' identity(identity(fn(2)))
#'
#' g <- function(trim) identity(identity(fn(trim)))
#' g(2)
#' g(3)
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
  cat("expr: ", expr, "\n", sep = "")
  cat("env:  [", env_format(x$env), "]\n", sep = "")
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
current_frame <- function() {
  ctxt_frame(2)
}

#' @rdname stack
#' @export
ctxt_frame <- function(n = 1) {
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
      fn_name = lang_name(sys.call(-n))
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

  eval_callers <- ctxt_stack_callers()
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
    fn_name = lang_name(sys.call(pos))
  ))

  if (clean) {
    frame <- frame_clean_eval(frame)
  }
  frame
}

#' Get the environment of the caller frame
#'
#' `caller_frame()` is a shortcut for `call_frame(2)` and
#' `caller_fn()` and `caller_env()` are shortcuts for
#' `call_frame(2)$env` `call_frame(2)$fn`.
#'
#' @param n The number of generation to go back. Note that contrarily
#'   to [call_frame()], 1 represents the parent frame rather than the
#'   current frame.
#' @seealso [call_frame()]
#' @export
caller_env <- function(n = 1) {
  parent.frame(n + 1)
}
#' @rdname caller_env
#' @export
caller_frame <- function(n = 1) {
  call_frame(n + 2)
}
#' @rdname caller_env
#' @export
caller_fn <- function(n = 1) {
  call_frame(n + 2)$fn
}


# The _depth() functions count the global frame as well

#' @rdname stack
#' @export
ctxt_depth <- function() {
  sys.nframe()
}
#' @rdname stack
#' @export
call_depth <- function() {
  eval_callers <- ctxt_stack_callers()
  trail <- trail_make(eval_callers)
  length(trail)
}


# Summaries ----------------------------------------------------------

#' @rdname stack
#' @export
ctxt_stack <- function(n = NULL, trim = 0) {
  stack_data <- list(
    pos = ctxt_stack_trail(),
    caller_pos = ctxt_stack_callers(),
    expr = ctxt_stack_exprs(),
    env = ctxt_stack_envs(),
    fn = ctxt_stack_fns()
  )

  # Remove ctxt_stack() from stack
  stack_data <- map(stack_data, drop_first)

  stack_data <- stack_subset(stack_data, n)
  stack_data$fn_name <- map(stack_data$expr, lang_name)

  stack <- transpose(stack_data)
  stack <- map(stack, new_frame)

  if (is.null(n) || (length(n) && n > length(stack))) {
    stack <- c(stack, list(global_frame()))
  }
  if (trim > 0) {
    stack <- stack_trim(stack, n = trim + 1)
  }

  structure(stack, class = c("ctxt_stack", "stack"))
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
  callers <- sys.parents()
  rev(drop_last(callers))
}
ctxt_stack_fns <- function() {
  pos <- sys.nframe() - 1
  map(seq(pos, 1), sys.function)
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
    stack_data <- map(stack_data, `[`, seq_len(n))
  }
  stack_data
}

#' @rdname stack
#' @export
call_stack <- function(n = NULL, clean = TRUE) {
  eval_callers <- ctxt_stack_callers()
  trail <- trail_make(eval_callers, n, clean = clean)

  stack_data <- list(
    pos = drop_last(trail),
    caller_pos = drop_first(trail),
    expr = map(trail, sys.call),
    env = map(trail, sys.frame),
    fn = map(trail, sys.function)
  )
  stack_data$fn_name <- map(stack_data$expr, lang_name)

  stack <- transpose(stack_data)
  stack <- map(stack, new_frame)
  if (clean) {
    stack <- map(stack, frame_clean_eval)
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

#' Is object a stack?
#' @param x An object to test
#' @export
is_stack <- function(x) inherits(x, "stack")

#' @rdname is_stack
#' @export
is_eval_stack <- function(x) inherits(x, "ctxt_stack")

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

#' Find the position or distance of a frame on the evaluation stack
#'
#' The frame position on the stack can be computed by counting frames
#' from the global frame (the bottom of the stack, the default) or
#' from the current frame (the top of the stack).
#'
#' While this function returns the position of the frame on the
#' evaluation stack, it can safely be called with intervening frames
#' as those will be discarded.
#'
#' @param frame The environment of a frame. Can be any object with a
#'   [get_env()] method. Note that for frame objects, the position from
#'   the global frame is simply `frame$pos`. Alternatively, `frame`
#'   can be an integer that represents the position on the stack (and
#'   is thus returned as is if `from` is "global".
#' @param from Whether to compute distance from the global frame (the
#'   bottom of the evaluation stack), or from the current frame (the
#'   top of the evaluation stack).
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
#'
#' # You can also ask for the position from the current frame rather
#' # than the global frame:
#' fn <- function() g(environment())
#' g <- function(env) h(env)
#' h <- function(env) frame_position(env, from = "current")
#' fn()
frame_position <- function(frame, from = c("global", "current")) {
  stack <- stack_trim(ctxt_stack(), n = 2)

  if (arg_match(from) == "global") {
    frame_position_global(frame, stack)
  } else {
    caller_pos <- call_frame(2)$pos
    frame_position_current(frame, stack, caller_pos)
  }
}

frame_position_global <- function(frame, stack = NULL) {
  if (is_frame(frame)) {
    return(frame$pos)
  } else if (is_integerish(frame)) {
    return(frame)
  }

  frame <- get_env(frame)
  stack <- stack %||% stack_trim(ctxt_stack(), n = 2)
  envs <- pluck(stack, "env")

  i <- 1
  for (env in envs) {
    if (identical(env, frame)) {
      return(length(envs) - i)
    }
    i <- i + 1
  }

  abort("`frame` not found on evaluation stack")
}

frame_position_current <- function(frame, stack = NULL,
                                   caller_pos = NULL) {
  if (is_integerish(frame)) {
    pos <- frame
  } else {
    stack <- stack %||% stack_trim(ctxt_stack(), n = 2)
    pos <- frame_position_global(frame, stack)
  }
  caller_pos <- caller_pos %||% call_frame(2)$pos
  caller_pos - pos + 1
}


#' Trim top call layers from the evaluation stack
#'
#' [ctxt_stack()] can be tricky to use in real code because all
#' intervening frames are returned with the stack, including those at
#' `ctxt_stack()` own call site. `stack_trim()` makes it easy to
#' remove layers of intervening calls.
#'
#' @param stack An evaluation stack.
#' @param n The number of call frames (not eval frames) to trim off
#'   the top of the stack. In other words, the number of layers of
#'   intervening frames to trim.
#' @export
#' @examples
#' # Intervening frames appear on the evaluation stack:
#' identity(identity(ctxt_stack()))
#'
#' # stack_trim() will trim the first n layers of calls:
#' stack_trim(identity(identity(ctxt_stack())))
#'
#' # Note that it also takes care of calls intervening at its own call
#' # site:
#' identity(identity(
#'   stack_trim(identity(identity(ctxt_stack())))
#' ))
#'
#' # It is especially useful when used within a function that needs to
#' # inspect the evaluation stack but should nonetheless be callable
#' # within nested calls without side effects:
#' stack_util <- function() {
#'   # n = 2 means that two layers of intervening calls should be
#'   # removed: The layer at ctxt_stack()'s call site (including the
#'   # stack_trim() call), and the layer at stack_util()'s call.
#'   stack <- stack_trim(ctxt_stack(), n = 2)
#'   stack
#' }
#' user_fn <- function() {
#'   # A user calls your stack utility with intervening frames:
#'   identity(identity(stack_util()))
#' }
#' # These intervening frames won't appear in the evaluation stack
#' identity(user_fn())
stack_trim <- function(stack, n = 1) {
  if (n < 1) {
    return(stack)
  }

  # Add 1 to discard stack_trim()'s own intervening frames
  caller_pos <- call_frame(n + 1, clean = FALSE)$pos

  n_frames <- length(stack)
  n_skip <- n_frames - caller_pos
  stack[seq(n_skip, n_frames)]
}

is_frame_env <- function(env) {
  for (frame in sys.frames()) {
    if (identical(env, frame)) {
      return(TRUE)
    }
  }
  FALSE
}


#' Jump to or from a frame
#'
#' While [base::return()] can only return from the current local
#' frame, these two functions will return from any frame on the
#' current evaluation stack, between the global and the currently
#' active context. They provide a way of performing arbitrary
#' non-local jumps out of the function currently under evaluation.
#'
#' `return_from()` will jump out of `frame`. `return_to()` is a bit
#' trickier. It will jump out of the frame located just before `frame`
#' in the evaluation stack, so that control flow ends up in `frame`,
#' at the location where the previous frame was called from.
#'
#' These functions should only be used rarely. These sort of non-local
#' gotos can be hard to reason about in casual code, though they can
#' sometimes be useful. Also, consider to use the condition system to
#' perform non-local jumps.
#'
#' @param frame An environment, a frame object, or any object with an
#'   [get_env()] method. The environment should be an evaluation
#'   environment currently on the stack.
#' @param value The return value.
#' @export
#' @examples
#' # Passing fn() evaluation frame to g():
#' fn <- function() {
#'   val <- g(get_env())
#'   cat("g returned:", val, "\n")
#'   "normal return"
#' }
#' g <- function(env) h(env)
#'
#' # Here we return from fn() with a new return value:
#' h <- function(env) return_from(env, "early return")
#' fn()
#'
#' # Here we return to fn(). The call stack unwinds until the last frame
#' # called by fn(), which is g() in that case.
#' h <- function(env) return_to(env, "early return")
#' fn()
return_from <- function(frame, value = NULL) {
  if (is_integerish(frame)) {
    frame <- ctxt_frame(frame)
  }

  exit_env <- get_env(frame)
  expr <- expr(return(!!value))
  eval_bare(expr, exit_env)
}

#' @rdname return_from
#' @export
return_to <- function(frame, value = NULL) {
  if (is_integerish(frame)) {
    prev_pos <- frame - 1
  } else {
    env <- get_env(frame)
    distance <- frame_position_current(env)
    prev_pos <- distance - 1
  }

  prev_frame <- ctxt_frame(prev_pos)
  return_from(prev_frame, value)
}


#' Inspect a call
#'
#' This function is useful for quick testing and debugging when you
#' manipulate expressions and calls. It lets you check that a function
#' is called with the right arguments. This can be useful in unit
#' tests for instance. Note that this is just a simple wrapper around
#' [base::match.call()].
#'
#' @param ... Arguments to display in the returned call.
#' @export
#' @examples
#' call_inspect(foo(bar), "" %>% identity())
#' invoke(call_inspect, list(a = mtcars, b = letters))
call_inspect <- function(...) match.call()
