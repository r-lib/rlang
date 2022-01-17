#' Get properties of the current or caller frame
#'
#' @description
#' * `current_fn()` returns the function of the current frame.
#' * `caller_fn()` returns the function of the calling frame.
#' * `frame_fn()` returns the function of the supplied frame.
#'
#' @param n The number of callers to go back.
#' @param frame A frame environment of a currently running function,
#'   as returned by [caller_env()].
#'
#' @seealso [caller_env()] and [current_env()]
#' @export
caller_fn <- function(n = 1) {
  check_number(n)
  frame_fn(caller_env(n + 1))
}
#' @rdname caller_fn
#' @export
current_fn <- function() {
  caller_fn()
}
#' @rdname caller_fn
#' @export
frame_fn <- function(frame = caller_env()) {
  check_environment(frame)

  # Match the oldest frame to find an actual execution environment if
  # it exists rather than `eval()` frames
  frames <- eval_bare(call2(sys.frames), frame)
  if (i <- detect_index(frames, identical, frame)) {
    return(sys.function(i))
  }

  msg <- sprintf(
    "%s must be the environment of a currently running function.",
    format_arg("frame")
  )
  abort(msg)
}

sys_parent <- function(n, frame = caller_env()) {
  parents <- sys_parents(frame = frame)

  if (!length(parents)) {
    return(0L)
  }

  out <- last(parents)
  n <- n - 1L

  while (n && out) {
    out <- parents[[out]]
    n <- n - 1L
  }

  out
}
sys_parents <- function(frame = caller_env()) {
  parents <- eval_bare(call2(sys.parents), frame)

  # Fix infloop parents caused by evaluation in non-frame environments
  parents[parents == seq_along(parents)] <- 0L

  # Patch callers of frames that have the same environment which can
  # happens with frames created by `eval()`. When duplicates
  # environments are on the stack, `sys.parents()` returns the number
  # of the oldest frame instead of the youngest. We fix this here to
  # be consistent with `parent.frame()`.
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

caller_call <- function(n = 1L) {
  if (is_environment(n)) {
    parent <- detect_index(sys.frames(), identical, n, .right = TRUE)
  } else {
    parent <- sys_parent(n + 1L)
  }

  if (parent) {
    sys.call(parent)
  } else {
    NULL
  }
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
