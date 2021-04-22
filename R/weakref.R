#' Create a weak reference
#'
#' @description
#'
#' A weak reference is a special R object which makes it possible to keep a
#' reference to an object without preventing garbage collection of that object.
#' It can also be used to keep data about an object without preventing GC of the
#' object, similar to WeakMaps in JavaScript.
#'
#' Objects in R are considered _reachable_ if they can be accessed by following
#' a chain of references, starting from a _root node_; root nodes are
#' specially-designated R objects, and include the global environment and base
#' environment. As long as the key is reachable, the value will not be garbage
#' collected. This is true even if the weak reference object becomes
#' unreachable. The key effectively prevents the weak reference and its value
#' from being collected, according to the following chain of ownership:
#' `weakref <- key -> value`.
#'
#' When the key becomes unreachable, the key and value in the weak reference
#' object are replaced by `NULL`, and the finalizer is scheduled to execute.
#'
#' @param key The key for the weak reference. Must be a reference object -- that
#'   is, an environment or external pointer.
#' @param value The value for the weak reference. This can be `NULL`, if you
#'   want to use the weak reference like a weak pointer.
#' @param finalizer A function that is run after the key becomes unreachable.
#' @param on_quit Should the finalizer be run when R exits?
#'
#' @keywords experimental
#' @seealso [is_weakref()], [wref_key()] and [wref_value()].
#' @export
#' @examples
#' e <- env()
#'
#' # Create a weak reference to e
#' w <- new_weakref(e, finalizer = function(e) message("finalized"))
#'
#' # Get the key object from the weak reference
#' identical(wref_key(w), e)
#'
#' # When the regular reference (the `e` binding) is removed and a GC occurs,
#' # the weak reference will not keep the object alive.
#' rm(e)
#' gc()
#' identical(wref_key(w), NULL)
#'
#'
#' # A weak reference with a key and value. The value contains data about the
#' # key.
#' k <- env()
#' v <- list(1, 2, 3)
#' w <- new_weakref(k, v)
#'
#' identical(wref_key(w), k)
#' identical(wref_value(w), v)
#'
#' # When v is removed, the weak ref keeps it alive because k is still reachable.
#' rm(v)
#' gc()
#' identical(wref_value(w), list(1, 2, 3))
#'
#' # When k is removed, the weak ref does not keep k or v alive.
#' rm(k)
#' gc()
#' identical(wref_key(w), NULL)
#' identical(wref_value(w), NULL)
new_weakref <- function(key, value = NULL, finalizer = NULL, on_quit = FALSE) {
  .Call(ffi_new_weakref, key, value, finalizer, on_quit)
}

#' Get key/value from a weak reference object
#'
#' @param x A weak reference object.
#'
#' @seealso [is_weakref()] and [new_weakref()].
#'
#' @export
wref_key <- function(x) {
  .Call(ffi_wref_key, x)
}

#' @rdname wref_key
#' @export
wref_value <- function(x) {
  .Call(ffi_wref_value, x)
}

#' Is object a weak reference?
#' @param x An object to test.
#' @export
is_weakref <- function(x) {
  .Call(ffi_is_weakref, x)
}
