#' Get properties of the current or caller frame
#'
#' @description
#' These accessors retrieve properties of frames on the call stack.
#' The prefix indicates for which frame a property should be accessed:
#'
#' * From the current frame with `current_` accessors.
#' * From a calling frame with `caller_` accessors.
#' * From a matching frame with `frame_` accessors.
#'
#' The suffix indicates which property to retrieve:
#'
#' * `_fn` accessors return the function running in the frame.
#' * `_call` accessors return the defused call with which the function
#'   running in the frame was invoked.
#' * `_env` accessors return the execution environment of the function
#'   running in the frame.
#'
#' @param n The number of callers to go back.
#' @param frame A frame environment of a currently running function,
#'   as returned by [caller_env()]. `NULL` is returned if the
#'   environment does not exist on the stack.
#'
#' @seealso [caller_env()] and [current_env()]
#' @name stack
NULL

#' @rdname stack
#' @export
current_call <- function() {
  caller_call()
}
#' @rdname stack
#' @export
current_fn <- function() {
  caller_fn()
}
#' @rdname stack
#' @export
current_env <- function() {
  parent.frame()
}

#' @rdname stack
#' @export
caller_call <- function(n = 1) {
  check_number_whole(n)
  frame_call(caller_env(n + 1))
}
#' @rdname stack
#' @export
caller_fn <- function(n = 1) {
  check_number_whole(n)
  frame_fn(caller_env(n + 1))
}
#' @rdname stack
#' @export
caller_env <- function(n = 1) {
  parent.frame(n + 1)
}

#' @rdname stack
#' @export
frame_call <- function(frame = caller_env()) {
  check_environment(frame)
  frame_get(frame, sys.call)
}
#' @rdname stack
#' @export
frame_fn <- function(frame = caller_env()) {
  check_environment(frame)
  frame_get(frame, sys.function)
}

frame_get <- function(frame, accessor) {
  if (identical(frame, global_env())) {
    return(NULL)
  }

  # Match the oldest frame to find an actual execution environment if
  # it exists. Using the `eval_bare(call2(accessor), frame)` trick
  # would match from the bottom and might encounter `eval()` frames.
  frames <- eval_bare(call2(sys.frames), frame)

  for (i in seq_along(frames)) {
    if (identical(frames[[i]], frame)) {
      return(accessor(i))
    }
  }

  NULL
}

# Respects the invariant: caller_env2() === evalq(caller_env2())
caller_env2 <- function(n = 1, error_call = caller_env()) {
  # Start from `current_env()` with `n + 1` because `caller_env()`
  # might not be on the stack
  parent <- sys_parent(
    n + 1,
    patch_eval = TRUE,
    frame = current_env(),
    error_call = error_call
  )
  sys.frame(parent)
}

sys_parent <- function(n,
                       patch_eval = FALSE,
                       frame = caller_env(),
                       error_call = caller_env()) {
  parents <- sys_parents(frame = frame)
  if (n > length(parents)) {
    msg <- sprintf(
      "%s can't be larger than the number of calling frames.",
      format_arg("n")
    )
    abort(msg, call = error_call)
  }

  if (!length(parents)) {
    return(0L)
  }

  out <- length(parents)

  while (n && out) {
    if (patch_eval && identical(sys.function(out), prim_eval)) {
      out <- parents[[out - 1]]
    }
    out <- parents[[out]]
    n <- n - 1L
  }

  out
}

sys_parents <- function(frame = caller_env(), match_oldest = TRUE) {
  parents <- eval_bare(call2(sys.parents), frame)

  # Fix infloop parents caused by evaluation in non-frame environments
  parents[parents == seq_along(parents)] <- 0L

  if (match_oldest) {
    return(parents)
  }

  # Patch callers of frames that have the same environment which can
  # happens with frames created by `eval()`. When duplicates
  # environments are on the stack, `sys.parents()` returns the number
  # of the oldest frame instead of the youngest. We fix this here when
  # requested to be consistent with `parent.frame()`.
  frames <- as.list(sys.frames())
  remaining_dups <- which(duplicated(frames) | duplicated(frames, fromLast = TRUE))

  while (length(remaining_dups)) {
    dups <- which(map_lgl(frames, identical, frames[[remaining_dups[[1]]]]))
    remaining_dups <- setdiff(remaining_dups, dups)

    # We're going to patch the callers of duplicate frames so discard
    # any duplicate that doesn't have a caller
    dups <- dups[dups < length(parents)]

    parents[dups + 1L] <- dups
  }

  parents
}

#' Jump to or from a frame
#'
#' @description
#' `r lifecycle::badge("questioning")`
#'
#' While [base::return()] can only return from the current local
#' frame, `return_from()` will return from any frame on the
#' current evaluation stack, between the global and the currently
#' active context.
#'
#' @param frame An execution environment of a currently running
#'   function.
#' @param value The return value.
#'
#' @keywords internal
#' @export
#' @examples
#' fn <- function() {
#'   g(current_env())
#'   "ignored"
#' }
#' g <- function(env) {
#'   h(env)
#'   "ignored"
#' }
#' h <- function(env) {
#'   return_from(env, "early return")
#'   "ignored"
#' }
#'
#' fn()
return_from <- function(frame, value = NULL) {
  eval_bare(expr(return(!!value)), frame)
}

#' Inspect a call
#'
#' This function is a wrapper around [base::match.call()]. It returns
#' its own function call.
#'
#' @param ... Arguments to display in the returned call.
#' @export
#' @examples
#' # When you call it directly, it simply returns what you typed
#' call_inspect(foo(bar), "" %>% identity())
#'
#' # Pass `call_inspect` to functionals like `lapply()` or `map()` to
#' # inspect the calls they create around the supplied function
#' lapply(1:3, call_inspect)
call_inspect <- function(...) match.call()
