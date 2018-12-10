
#  Stack and frames  -------------------------------------------------

#' Get caller frame
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' @param n Number of frames to go back.
#' @keywords internal
#' @export
caller_frame <- function(n = 1) {
  signal_soft_deprecated("`caller_frame()` is soft-deprecated as of rlang 0.3.0.")
  call_frame(n + 2)
}

#' Call stack information
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' The `eval_` and `call_` families of functions provide a replacement
#' for the base R functions prefixed with `sys.` (which are all about
#' the context stack), as well as for [parent.frame()] (which is the
#' only base R function for querying the call stack). The context
#' stack includes all R-level evaluation contexts. It is linear in
#' terms of execution history but due to lazy evaluation it is
#' potentially nonlinear in terms of call history. The call stack
#' history, on the other hand, is homogenous.
#'
#' @details
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
#' is more complicated and changes meaning for values lower than 1.
#' For the sake of consistency, the rlang functions all take the
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
#'
#' @section Life cycle:
#'
#' These functions are soft-deprecated and replaced by [trace_back()].
#'
#' @param n The number of frames to go back in the stack.
#' @param clean Whether to post-process the call stack to clean
#'   non-standard frames. If `TRUE`, suboptimal call-stack entries by
#'   [base::eval()] will be cleaned up: the duplicate frame created by
#'   `eval()` is eliminated.
#' @param trim The number of layers of intervening frames to trim off
#'   the stack. See [stack_trim()] and examples.
#' @name stack
#' @keywords internal
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
#' # The rlang _stack() functions return a list of frame
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' @param x Object to test
#' @keywords internal
#' @export
is_frame <- function(x) {
  inherits(x, "frame")
}

#' @rdname stack
#' @export
global_frame <- function() {
  signal_soft_deprecated("`global_frame()` is soft-deprecated as of rlang 0.3.0.")
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
  signal_soft_deprecated("`current_frame()` is soft-deprecated as of rlang 0.3.0.")
  ctxt_frame(2)
}

#' @rdname stack
#' @export
ctxt_frame <- function(n = 1) {
  signal_soft_deprecated("`ctxt_frame()` is soft-deprecated as of rlang 0.3.0.")
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
      fn_name = call_name(sys.call(-n))
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
    fn_name = call_name(sys.call(pos))
  ))

  if (clean) {
    frame <- frame_clean_eval(frame)
  }
  frame
}


# The _depth() functions count the global frame as well

#' @rdname stack
#' @export
ctxt_depth <- function() {
  signal_soft_deprecated("`ctxt_depth()` is soft-deprecated as of rlang 0.3.0.")
  sys.nframe()
}
#' @rdname stack
#' @export
call_depth <- function() {
  signal_soft_deprecated("`call_depth()` is soft-deprecated as of rlang 0.3.0.")
  eval_callers <- ctxt_stack_callers()
  trail <- trail_make(eval_callers)
  length(trail)
}


# Summaries ----------------------------------------------------------

#' @rdname stack
#' @export
ctxt_stack <- function(n = NULL, trim = 0) {
  signal_soft_deprecated("`ctxt_stack()` is soft-deprecated as of rlang 0.3.0.")

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
  stack_data$fn_name <- map(stack_data$expr, call_name)

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
  signal_soft_deprecated("`call_stack()` is soft-deprecated as of rlang 0.3.0.")

  eval_callers <- ctxt_stack_callers()
  trail <- trail_make(eval_callers, n, clean = clean)

  stack_data <- list(
    pos = drop_last(trail),
    caller_pos = drop_first(trail),
    expr = map(trail, sys.call),
    env = map(trail, sys.frame),
    fn = map(trail, sys.function)
  )
  stack_data$fn_name <- map(stack_data$expr, call_name)

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
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' @param x An object to test
#' @export
is_stack <- function(x) {
  signal_soft_deprecated("`is_stack()` is soft-deprecated as of rlang 0.3.0.")
  inherits(x, "stack")
}

#' @rdname is_stack
#' @export
is_eval_stack <- function(x) {
  signal_soft_deprecated("`is_eval_stack()` is soft-deprecated as of rlang 0.3.0.")
  inherits(x, "ctxt_stack")
}

#' @rdname is_stack
#' @export
is_call_stack <- function(x) {
  signal_soft_deprecated("`is_call_stack()` is soft-deprecated as of rlang 0.3.0.")
  inherits(x, "call_stack")
}

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
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' The frame position on the stack can be computed by counting frames
#' from the global frame (the bottom of the stack, the default) or
#' from the current frame (the top of the stack).
#'
#' @details
#'
#' While this function returns the position of the frame on the
#' evaluation stack, it can safely be called with intervening frames
#' as those will be discarded.
#'
#'
#' @section Life cycle:
#'
#' These functions are soft-deprecated and replaced by [trace_back()].
#'
#' @param frame The environment of a frame. Can be any object with a
#'   [get_env()] method. Note that for frame objects, the position from
#'   the global frame is simply `frame$pos`. Alternatively, `frame`
#'   can be an integer that represents the position on the stack (and
#'   is thus returned as is if `from` is "global".
#' @param from Whether to compute distance from the global frame (the
#'   bottom of the evaluation stack), or from the current frame (the
#'   top of the evaluation stack).
#'
#' @keywords internal
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
  signal_soft_deprecated("`frame_position()` is soft-deprecated as of rlang 0.3.0.")

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
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' [ctxt_stack()] can be tricky to use in real code because all
#' intervening frames are returned with the stack, including those at
#' `ctxt_stack()` own call site. `stack_trim()` makes it easy to
#' remove layers of intervening calls.
#'
#'
#' @section Life cycle:
#'
#' These functions are soft-deprecated and replaced by [trace_back()].
#'
#' @param stack An evaluation stack.
#' @param n The number of call frames (not eval frames) to trim off
#'   the top of the stack. In other words, the number of layers of
#'   intervening frames to trim.
#' @export
#' @keywords internal
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
  signal_soft_deprecated("`stack_trim()` is soft-deprecated as of rlang 0.3.0.")

  if (n < 1) {
    return(stack)
  }

  # Add 1 to discard stack_trim()'s own intervening frames
  caller_pos <- call_frame(n + 1, clean = FALSE)$pos

  n_frames <- length(stack)
  n_skip <- n_frames - caller_pos
  stack[seq(n_skip, n_frames)]
}


#  Tidy eval  --------------------------------------------------------

#' Unquote as a bare expression
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("defunct")}
#'
#' `UQE()` is defunct as of rlang 0.3.0 in order to simplify the
#' quasiquotation syntax. You can replace its use by a combination of
#' `!!` and `get_expr()`: `!!get_expr(x)` is equivalent to `UQE(x)`.
#'
#' @param x Object to unquote.
#' @keywords internal
#' @export
UQE <- function(x) {
  stop_defunct(msg = "`UQE()` is defunct. Please use `!!get_expr(x)`")
}

#' Parse text into a quosure
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' These functions were soft-deprecated and renamed to [parse_quo()]
#' and [parse_quos()] in rlang 0.2.0. This is for consistency with the
#' convention that suffixes indicating return types are not
#' abbreviated.
#'
#' @inheritParams parse_expr
#' @keywords internal
#' @export
parse_quosure <- function(x, env = caller_env()) {
  signal_soft_deprecated(paste_line(
    "`parse_quosure()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `parse_quo()` instead"
  ))
  parse_quo(x, env = env)
}
#' @rdname parse_quosure
#' @export
parse_quosures <- function(x, env = caller_env()) {
  signal_soft_deprecated(paste_line(
    "`parse_quosures()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `parse_quos()` instead"
  ))
  parse_quos(x, env = env)
}

#' Squash a quosure
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' This function is soft-deprecated, please use [quo_squash()] instead.
#'
#' @inheritParams quo_squash
#' @keywords internal
#' @export
quo_expr <- function(quo, warn = FALSE) {
  signal_soft_deprecated(paste_line(
    "`quo_expr()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `quo_squash()` instead"
  ))
  quo_squash(quo, warn = warn)
}

#' Create an overscope
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' These functions have been soft-deprecated in rlang 0.2.0. Please
#' use [as_data_mask()] and [new_data_mask()] instead. We no longer
#' require the mask to be cleaned up so `overscope_clean()` does not
#' have a replacement.
#'
#' @inheritParams as_data_mask
#' @param quo A [quosure][quotation].
#'
#' @keywords internal
#' @export
as_overscope <- function(quo, data = NULL) {
  signal_soft_deprecated(paste_line(
    "`as_overscope()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `as_data_mask()` instead"
  ))
  as_data_mask(data, quo_get_env(quo))
}
#' @rdname as_overscope
#' @param enclosure The `parent` argument of [new_data_mask()].
#' @export
new_overscope <- function(bottom, top = NULL, enclosure = NULL) {
  signal_soft_deprecated(paste_line(
    "`new_overscope()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_data_mask()` instead"
  ))
  new_data_mask(bottom, top, enclosure)
}
#' @rdname as_overscope
#' @param overscope A data mask.
#' @export
overscope_clean <- function(overscope) {
  signal_soft_deprecated("`overscope_clean()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_data_mask_clean, overscope))
}

#' Tidy evaluation in a custom environment
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("defunct")}
#'
#' This function is defunct as of rlang 0.3.0.
#'
#' @inheritParams eval_tidy
#' @inheritParams as_data_mask
#'
#' @keywords internal
#' @export
eval_tidy_ <- function(expr, bottom, top = NULL, env = caller_env()) {
  stop_defunct("`eval_tidy_()` is defunct as of rlang 0.3.0. Use `eval_tidy()` instead.")
}
#' Evaluate next quosure in a data mask
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' `overscope_eval_next()` is soft-deprecated as of rlang
#' 0.2.0. Please use `eval_tidy()` to which you can now supply an
#' overscope.
#'
#' @param quo A quosure.
#' @param overscope A valid overscope containing bindings for `~`,
#'   `.top_env` and `_F` and whose parents contain overscoped bindings
#'   for tidy evaluation.
#' @param env The lexical enclosure in case `quo` is not a validly
#'   scoped quosure. This is the [base environment][base_env] by
#'   default.
#'
#' @keywords internal
#' @export
overscope_eval_next <- function(overscope, quo, env = base_env()) {
  signal_soft_deprecated(paste_line(
    "`overscope_eval_next()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `eval_tidy()` with a data mask instead"
  ))
  .Call(rlang_eval_tidy, quo, overscope, env)
}


#' Create a dictionary
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("defunct")}
#'
#' The dictionary class is defunct as of rlang 0.2.0. It was
#' trying to be too general and did not prove useful. Please use
#' [as_data_pronoun()] or your own pronoun class instead.
#'
#' @param x An object for which you want to find associated data.
#' @param lookup_msg An error message when your data source is
#'   accessed inappropriately (by position rather than name).
#' @param read_only Whether users can replace elements of the
#'   dictionary.
#'
#' @name dictionary
#' @keywords internal
#' @export
as_dictionary <- function(x, lookup_msg = NULL, read_only = FALSE) {
  stop_defunct(paste_line(
    "`as_dictionary()` is defunct as of rlang 0.3.0.",
    "Please use `as_data_pronoun()` instead"
  ))
}
#' @rdname dictionary
#' @export
is_dictionary <- function(x) {
  stop_defunct("`is_dictionary()` is defunct as of rlang 0.3.0.")
}

#' Test for or coerce to quosure-like objects
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("defunct")}
#'
#' These functions are deprecated as of rlang 0.2.0 because they make
#' the assumption that quosures are a subtype of formula, which we are
#' now considering to be an implementation detail.
#'
#' @inheritParams is_formula
#' @inheritParams as_quosure
#'
#' @keywords internal
#' @export
is_quosureish <- function(x, scoped = NULL) {
  stop_defunct("`is_quosureish()` is defunct as of rlang 0.3.0")
}
#' @rdname is_quosureish
#' @export
as_quosureish <- function(x, env = caller_env()) {
  stop_defunct("`as_quosureish()` is defunct as of rlang 0.3.0")
}



#  Expressions  ------------------------------------------------------

#' Create a call
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' These functions are soft-deprecated, please use [call2()] and
#' [new_call()] instead.
#'
#' @inheritParams call2
#' @keywords internal
#' @export
lang <- function(.fn, ..., .ns = NULL) {
  signal_soft_deprecated(paste_line(
    "`lang()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call2()` instead"
  ))
  call2(.fn, ..., .ns = .ns)
}
#' @rdname lang
#' @inheritParams new_call
#' @export
new_language <- function(head, tail = NULL) {
  signal_soft_deprecated(paste_line(
    "`new_language()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_call()` instead"
  ))
  new_call(head, tail)
}

#' Is object a call?
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' These functions are soft-deprecated, please use [is_call()] and its
#' `n` argument instead.
#'
#' @inheritParams is_call
#' @keywords internal
#' @export
is_lang <- function(x, name = NULL, n = NULL, ns = NULL) {
  signal_soft_deprecated(paste_line(
    "`is_lang()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_call()` instead"
  ))
  is_call(x, name, n, ns)
}
#' @rdname is_lang
#' @export
is_unary_lang <- function(x, name = NULL, ns = NULL) {
  signal_soft_deprecated(paste_line(
    "`is_unary_lang()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_call()` instead"
  ))
  is_call(x, name, n = 1L, ns = ns)
}
#' @rdname is_lang
#' @export
is_binary_lang <- function(x, name = NULL, ns = NULL) {
  signal_soft_deprecated(paste_line(
    "`is_binary_lang()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_call()` instead"
  ))
  is_call(x, name, n = 2L, ns = ns)
}
#' @rdname is_lang
#' @param quo A quosure to test.
#' @export
quo_is_lang <- function(quo) {
  signal_soft_deprecated(paste_line(
    "`quo_is_lang()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `quo_is_call()` instead"
  ))
  .Call(rlang_quo_is_call, quo)
}

#' Manipulate or access a call
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' These functions are soft-deprecated, please use [call_modify()],
#' [call_standardise()], or [call_fn()] instead.
#'
#' @inheritParams call_modify
#' @param lang,.lang The `call` or `.call` argument of the renamed
#'   functions.
#' @keywords internal
#' @export
lang_modify <- function(.lang, ..., .standardise = FALSE) {
  signal_soft_deprecated(paste_line(
    "`lang_modify()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_modify()` instead"
  ))
  if (.standardise) {
    .lang <- call_standardise(.lang, caller_env())
  }
  call_modify(.lang, ...)
}
#' @rdname lang_modify
#' @export
lang_standardise <- function(lang) {
  signal_soft_deprecated(paste_line(
    "`lang_standardise()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_standardise()` instead"
  ))
  call_standardise(lang, env = caller_env())
}
#' @rdname lang_modify
#' @export
lang_fn <- function(lang) {
  signal_soft_deprecated(paste_line(
    "`lang_fn()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_fn()` instead"
  ))
  call_fn(lang, caller_env())
}
#' @rdname lang_modify
#' @export
lang_name <- function(lang) {
  signal_soft_deprecated(paste_line(
    "`lang_name()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_name()` instead"
  ))
  call_name(lang)
}
#' @rdname lang_modify
#' @export
lang_args <- function(lang) {
  signal_soft_deprecated(paste_line(
    "`lang_args()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_args()` instead"
  ))
  call_args(lang)
}
#' @rdname lang_modify
#' @export
lang_args_names <- function(lang) {
  signal_soft_deprecated(paste_line(
    "`lang_args_names()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_args_names()` instead"
  ))
  call_args_names(lang)
}


#' Return the head or tail of a call
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' As of rlang 0.2.0 these functions are retired (soft-deprecated for
#' now) because they are low level accessors that are rarely needed
#' for end users.
#'
#' @param lang A call.
#' @export
lang_head <- function(lang) {
  signal_soft_deprecated("`lang_head()` is soft-deprecated as of rlang 0.2.0.")
  call <- get_expr(lang)
  stopifnot(is_call(call))
  node_car(call)
}
#' @rdname lang_head
#' @export
lang_tail <- function(lang) {
  signal_soft_deprecated("`lang_tail()` is soft-deprecated as of rlang 0.2.0.")
  call <- get_expr(lang)
  stopifnot(is_call(call))
  node_cdr(call)
}

#' Is an object an expression?
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' This function was soft-deprecated and renamed to [is_expression()]
#' in rlang 0.2.0. This is for consistency with other type predicates
#' which are not abbreviated.
#'
#' @inheritParams is_expression
#' @keywords internal
#' @export
is_expr <- function(x) {
  signal_soft_deprecated(paste_line(
    "`is_expr()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_expression()` instead"
  ))
  is_expression(x)
}


#  Nodes  ------------------------------------------------------------

#' Mutate node components
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' These functions were soft-deprecated and renamed with `node_poke_`
#' prefix in rlang 0.2.0. This change follows a new naming convention
#' where mutation is referred to as "poking".
#'
#' @inheritParams new_node
#'
#' @keywords internal
#' @export
mut_node_car <- function(x, newcar) {
  signal_soft_deprecated("`mut_node_car()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_car, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cdr <- function(x, newcdr) {
  signal_soft_deprecated("`mut_node_cdr()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cdr, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_caar <- function(x, newcar) {
  signal_soft_deprecated("`mut_node_caar()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_caar, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cadr <- function(x, newcar) {
  signal_soft_deprecated("`mut_node_cadr()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cadr, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cdar <- function(x, newcdr) {
  signal_soft_deprecated("`mut_node_cdar()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cdar, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_cddr <- function(x, newcdr) {
  signal_soft_deprecated("`mut_node_cddr()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cddr, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_tag <- function(x, newtag) {
  signal_soft_deprecated("`mut_node_tag()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_tag, x, newtag))
}

#' @rdname vector-old-ctors
#' @export
node <- function(car, cdr = NULL) {
  signal_soft_deprecated(paste_line(
    "`node()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_node()` instead"
  ))
  new_node(car, cdr)
}


#  Environments  -----------------------------------------------------

#' Coerce to an environment
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' This function is soft-deprecated as it was renamed to
#' [as_environment()] in rlang 0.2.0.
#'
#' @keywords internal
#' @export
as_env <- function(x, parent = NULL) {
  signal_soft_deprecated("`as_env()` is soft-deprecated as of rlang 0.2.0.")
  as_environment(x, parent)
}

#' Is an object an environment?
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' These functions were soft-deprecated and renamed to
#' [is_environment()] and [is_bare_environment()] in rlang 0.2.0. This
#' is for consistency with other type predicates which are not
#' abbreviated.
#'
#' @inheritParams is_environment
#' @keywords internal
#' @export
is_env <- function(x) {
  signal_soft_deprecated(paste_line(
    "`is_env()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_environment()` instead"
  ))
  is_environment(x)
}
#' @rdname is_env
#' @export
is_bare_env <- function(x) {
  signal_soft_deprecated(paste_line(
    "`is_bare_env()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_bare_environment()` instead"
  ))
  is_bare_environment(x)
}

#' Bind a promise or active binding
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' As of rlang 0.3.0, `env_bind_exprs()` and `env_bind_fns()` have
#' been renamed to [env_bind_lazy()] and [env_bind_active()] for
#' consistency.
#'
#' @inheritParams env_bind
#'
#' @keywords internal
#' @export
env_bind_exprs <- function(.env, ..., .eval_env = caller_env()) {
  signal_soft_deprecated(paste_line(
    "`env_bind_exprs()` is soft-deprecated as of rlang 0.3.0.",
    "Please use `env_bind_lazy()` instead."
  ))
  env_bind_lazy(.env = .env, ..., .eval_env = .eval_env)
}
#' @rdname env_bind_exprs
#' @export
env_bind_fns <- function(.env, ...) {
  signal_soft_deprecated(paste_line(
    "`env_bind_fns()` is soft-deprecated as of rlang 0.3.0.",
    "Please use `env_bind_active()` instead."
  ))
  env_bind_active(.env = .env, ...)
}

#' Retired `scoped` functions
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' These functions are soft-deprecated as of rlang 0.3.0. They are
#' replaced by [is_attached()], ...
#'
#' @param nm The name of an environment attached to the search
#'   path. Call [base::search()] to see what is currently on the path.
#'
#' @keywords internal
#' @export
scoped_env <- function(nm) {
  signal_soft_deprecated(paste_line(
    "`scoped_env()` is soft-deprecated as of rlang 0.3.0.",
    "Please use `search_env()` instead."
  ))
  scoped_options(lifecycle_disable_warnings = TRUE)

  if (identical(nm, "NULL")) {
    return(empty_env())
  }
  if (!is_scoped(nm)) {
    stop(paste0(nm, " is not in scope"), call. = FALSE)
  }
  as.environment(nm)
}
#' @rdname scoped_env
#' @export
is_scoped <- function(nm) {
  signal_soft_deprecated(paste_line(
    "`is_scoped()` is soft-deprecated as of rlang 0.3.0.",
    "Please use `is_attached()` instead."
  ))
  scoped_options(lifecycle_disable_warnings = TRUE)

  if (!is_scalar_character(nm)) {
    stop("`nm` must be a string", call. = FALSE)
  }
  nm %in% scoped_names()
}
#' @rdname scoped_env
#' @export
scoped_envs <- function() {
  signal_soft_deprecated(paste_line(
    "`scoped_envs()` is soft-deprecated as of rlang 0.3.0.",
    "Please use `search_envs()` instead."
  ))
  scoped_options(lifecycle_disable_warnings = TRUE)

  envs <- c(list(.GlobalEnv), env_parents(.GlobalEnv))
  set_names(envs, scoped_names())
}
#' @rdname scoped_env
#' @export
scoped_names <- function() {
  signal_soft_deprecated(paste_line(
    "`scoped_names()` is soft-deprecated as of rlang 0.3.0.",
    "Please use `base::search()` instead."
  ))
  c(search(), "NULL")
}


#  Vectors  ----------------------------------------------------------

#' Retired vector construction by length
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' These functions were soft-deprecated and renamed with `new_` prefix
#' in rlang 0.2.0. This is for consistency with other non-variadic
#' object constructors.
#'
#' @param .x A vector.
#' @inheritParams new-vector
#' @inheritParams new-vector-along-retired
#' @name vector-old-ctors
#' @keywords internal
NULL

#' @rdname vector-old-ctors
#' @export
lgl_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`lgl_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_logical()` instead"
  ))
  new_logical(.n)
}
#' @rdname vector-old-ctors
#' @export
int_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`int_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_integer()` instead"
  ))
  new_integer(.n)
}
#' @rdname vector-old-ctors
#' @export
dbl_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`dbl_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_double()` instead"
  ))
  new_double(.n)
}
#' @rdname vector-old-ctors
#' @export
chr_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`chr_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_character()` instead"
  ))
  new_character(.n)
}
#' @rdname vector-old-ctors
#' @export
cpl_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`cpl_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_complex()` instead"
  ))
  new_complex(.n)
}
#' @rdname vector-old-ctors
#' @export
raw_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`raw_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_raw()` instead"
  ))
  new_raw(.n)
}
#' @rdname vector-old-ctors
#' @export
bytes_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`bytes_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_raw()` instead"
  ))
  new_raw(.n)
}
#' @rdname vector-old-ctors
#' @export
list_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`list_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_list()` instead"
  ))
  new_list(.n)
}

#' @rdname vector-old-ctors
#' @export
lgl_along <- function(.x) {
  signal_soft_deprecated("`lgl_along()` is soft-deprecated as of rlang 0.2.0.")
  scoped_options(lifecycle_disable_warnings = TRUE)
  new_logical_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
int_along <- function(.x) {
  signal_soft_deprecated("`int_along()` is soft-deprecated as of rlang 0.2.0.")
  scoped_options(lifecycle_disable_warnings = TRUE)
  new_integer_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
dbl_along <- function(.x) {
  signal_soft_deprecated("`dbl_along()` is soft-deprecated as of rlang 0.2.0.")
  scoped_options(lifecycle_disable_warnings = TRUE)
  new_double_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
chr_along <- function(.x) {
  signal_soft_deprecated("`chr_along()` is soft-deprecated as of rlang 0.2.0.")
  scoped_options(lifecycle_disable_warnings = TRUE)
  new_character_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
cpl_along <- function(.x) {
  signal_soft_deprecated("`cpl_along()` is soft-deprecated as of rlang 0.2.0.")
  scoped_options(lifecycle_disable_warnings = TRUE)
  new_complex_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
raw_along <- function(.x) {
  signal_soft_deprecated("`raw_along()` is soft-deprecated as of rlang 0.2.0.")
  scoped_options(lifecycle_disable_warnings = TRUE)
  new_raw_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
bytes_along <- function(.x) {
  signal_soft_deprecated("`bytes_along()` is soft-deprecated as of rlang 0.2.0.")
  scoped_options(lifecycle_disable_warnings = TRUE)
  new_raw_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
list_along <- function(.x) {
  signal_soft_deprecated("`list_along()` is soft-deprecated as of rlang 0.2.0.")
  scoped_options(lifecycle_disable_warnings = TRUE)
  new_list_along(.x, NULL)
}

#' Create vectors matching the length of a given vector
#'
#' These functions are soft-deprecated as of rlang 0.3.0 because they
#' are longer to type than the equivalent [rep_along()] or
#' [rep_named()] calls without added clarity.
#'
#' @param x A vector.
#' @param names Names for the new vector.
#' @name new-vector-along-retired

#' @export
#' @rdname new-vector-along-retired
new_logical_along <- function(x, names = base::names(x)) {
  signal_soft_deprecated_along("logical", "NA")
  set_names_impl(rep_len(na_lgl, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_integer_along <- function(x, names = base::names(x)) {
  signal_soft_deprecated_along("integer", "na_int")
  set_names_impl(rep_len(na_int, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_double_along <- function(x, names = base::names(x)) {
  signal_soft_deprecated_along("double", "na_dbl")
  set_names_impl(rep_len(na_dbl, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_character_along <- function(x, names = base::names(x)) {
  signal_soft_deprecated_along("character", "na_chr")
  set_names_impl(rep_len(na_chr, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_complex_along <- function(x, names = base::names(x)) {
  signal_soft_deprecated_along("complex", "na_cpl")
  set_names_impl(rep_len(na_cpl, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_raw_along <- function(x, names = base::names(x)) {
  signal_soft_deprecated_along("raw", "new_raw(1)")
  set_names_impl(vector("raw", length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_list_along <- function(x, names = base::names(x)) {
  signal_soft_deprecated_along("list", "list(NULL)")
  set_names_impl(vector("list", length(x)), x, names)
}
signal_soft_deprecated_along <- function(type, na, env = caller_env(2)) {
  signal_soft_deprecated(env = env, paste_line(
    sprintf("`new_%s_along()` is soft-deprecated as of rlang 0.3.0.", type),
    sprintf("Please use `rep_along(x, %s)` or `rep_named(nms, %s)` instead.", na, na)
  ))
}


#  Attributes  -------------------------------------------------------

#' Add attributes to an object
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' `set_attrs()` adds, changes, or zaps attributes of objects. Pass a
#' single unnamed `NULL` argument to zap all attributes. For
#' [uncopyable][is_copyable] types, use `mut_attrs()`.
#'
#' @details
#'
#' Unlike [structure()], these setters have no special handling of
#' internal attributes names like `.Dim`, `.Dimnames` or `.Names`.
#'
#'
#' @section Life cycle:
#'
#' These functions are soft-deprecated since rlang 0.3.0.
#'
#' @param .x An object to decorate with attributes.
#' @param ... A list of named attributes. These have [explicit
#'   splicing semantics][tidy-dots]. Pass a single unnamed `NULL` argument to
#'   zap all attributes from `.x`.
#' @return `set_attrs()` returns a modified [shallow copy][duplicate]
#'   of `.x`. `mut_attrs()` invisibly returns the original `.x`
#'   modified in place.
#'
#' @keywords internal
#' @export
#' @examples
#' set_attrs(letters, names = 1:26, class = "my_chr")
#'
#' # Splice a list of attributes:
#' attrs <- list(attr = "attr", names = 1:26, class = "my_chr")
#' obj <- set_attrs(letters, splice(attrs))
#' obj
#'
#' # Zap attributes by passing a single unnamed NULL argument:
#' set_attrs(obj, NULL)
#' set_attrs(obj, !!! list(NULL))
#'
#' # Note that set_attrs() never modifies objects in place:
#' obj
#'
#' # For uncopyable types, mut_attrs() lets you modify in place:
#' env <- env()
#' mut_attrs(env, foo = "bar")
#' env
set_attrs <- function(.x, ...) {
  signal_soft_deprecated("`set_attrs()` is soft-deprecated as of rlang 0.3.0")

  if (!is_copyable(.x)) {
    abort("`.x` is uncopyable: use `mut_attrs()` to change attributes in place")
  }
  set_attrs_impl(.x, ...)
}
#' @rdname set_attrs
#' @export
mut_attrs <- function(.x, ...) {
  signal_soft_deprecated("`set_attrs()` is soft-deprecated as of rlang 0.3.0")

  if (is_copyable(.x)) {
    abort("`.x` is copyable: use `set_attrs()` to change attributes without side effect")
  }
  invisible(set_attrs_impl(.x, ...))
}
set_attrs_impl <- function(.x, ...) {
  attrs <- dots_list(...)

  # If passed a single unnamed NULL, zap attributes
  if (identical(attrs, set_attrs_null)) {
    attributes(.x) <- NULL
  } else {
    attributes(.x) <- c(attributes(.x), attrs)
  }

  .x
}
set_attrs_null <- list(NULL)
names(set_attrs_null) <- ""
