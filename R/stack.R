#' Get properties of the current or caller frame
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' * The current frame is the execution context of the function that
#'   is currently being evaluated.
#'
#' * The caller frame is the execution context of the function that
#'   called the function currently being evaluated.
#'
#' See the [call stack][stack] topic for more information.
#'
#'
#' @section Life cycle:
#'
#' These functions are experimental.
#'
#' @param n The number of generations to go back.
#'
#' @seealso [caller_env()] and [current_env()]
#' @export
caller_fn <- function(n = 1) {
  with_options(lifecycle_disable_warnings = TRUE,
    call_frame(n + 2)$fn
  )
}
#' @rdname caller_fn
#' @export
current_fn <- function() {
  with_options(lifecycle_disable_warnings = TRUE,
    call_frame(2)$fn
  )
}

#' Jump to or from a frame
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' While [base::return()] can only return from the current local
#' frame, these two functions will return from any frame on the
#' current evaluation stack, between the global and the currently
#' active context. They provide a way of performing arbitrary
#' non-local jumps out of the function currently under evaluation.
#'
#' @section Life cycle:
#'
#' The support for `frame` object is soft-deprecated.  Please pass
#' simple environments to `return_from()` and `return_to()`.
#'
#' These functions are in the questioning lifecycle because we are
#' considering simpler alternatives.
#'
#' @param frame An environment, a frame object, or any object with an
#'   [get_env()] method. The environment should be an evaluation
#'   environment currently on the stack.
#' @param value The return value.
#'
#' @details
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
#'
#' @export
#' @examples
#' # Passing fn() evaluation frame to g():
#' fn <- function() {
#'   val <- g(current_env())
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
  with_options(lifecycle_disable_warnings = TRUE, {
    if (is_integerish(frame)) {
      prev_pos <- frame - 1
    } else {
      env <- get_env(frame)
      distance <- frame_position_current(env)
      prev_pos <- distance - 1
    }

    prev_frame <- ctxt_frame(prev_pos)
  })
  return_from(prev_frame, value)
}

is_frame_env <- function(env) {
  for (frame in sys.frames()) {
    if (identical(env, frame)) {
      return(TRUE)
    }
  }
  FALSE
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
call_inspect <- function(...) match.call()
