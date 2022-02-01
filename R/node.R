#' Helpers for pairlist and language nodes
#'
#' @description
#'
#' **Important**: These functions are for expert R programmers only.
#' You should only use them if you feel comfortable manipulating low
#' level R data structures at the C level. We export them at the R level
#' in order to make it easy to prototype C code. They don't perform
#' any type checking and can crash R very easily (try to take the CAR
#' of an integer vector --- save any important objects beforehand!).
#'
#' @param x A language or pairlist node. Note that these functions are
#'   barebones and do not perform any type checking.
#' @param car,newcar,cdr,newcdr The new CAR or CDR for the node. These
#'   can be any R objects.
#' @param newtag The new tag for the node. This should be a symbol.
#' @return Setters like `node_poke_car()` invisibly return `x` modified
#'   in place. Getters return the requested node component.
#' @seealso [duplicate()] for creating copy-safe objects and
#'   [base::pairlist()] for an easier way of creating a linked list of
#'   nodes.
#' @keywords internal
#' @export
new_node <- function(car, cdr = NULL) {
  .Call(ffi_new_node, car, cdr)
}

#' @rdname new_node
#' @export
node_car <- function(x) {
  .Call(ffi_node_car, x)
}
#' @rdname new_node
#' @export
node_cdr <- function(x) {
  .Call(ffi_node_cdr, x)
}
#' @rdname new_node
#' @export
node_caar <- function(x) {
  .Call(ffi_node_caar, x)
}
#' @rdname new_node
#' @export
node_cadr <- function(x) {
  .Call(ffi_node_cadr, x)
}
#' @rdname new_node
#' @export
node_cdar <- function(x) {
  .Call(ffi_node_cdar, x)
}
#' @rdname new_node
#' @export
node_cddr <- function(x) {
  .Call(ffi_node_cddr, x)
}

#' @rdname new_node
#' @export
node_poke_car <- function(x, newcar) {
  invisible(.Call(ffi_node_poke_car, x, newcar))
}
#' @rdname new_node
#' @export
node_poke_cdr <- function(x, newcdr) {
  invisible(.Call(ffi_node_poke_cdr, x, newcdr))
}
#' @rdname new_node
#' @export
node_poke_caar <- function(x, newcar) {
  invisible(.Call(ffi_node_poke_caar, x, newcar))
}
#' @rdname new_node
#' @export
node_poke_cadr <- function(x, newcar) {
  invisible(.Call(ffi_node_poke_cadr, x, newcar))
}
#' @rdname new_node
#' @export
node_poke_cdar <- function(x, newcdr) {
  invisible(.Call(ffi_node_poke_cdar, x, newcdr))
}
#' @rdname new_node
#' @export
node_poke_cddr <- function(x, newcdr) {
  invisible(.Call(ffi_node_poke_cddr, x, newcdr))
}

node_get <- function(node, i) {
  if (i < 1L) {
    abort("`i` must be an integer greater than 0.")
  }
  while (i > 1L) {
    node <- node_cdr(node)
    i <- i - 1L
  }
  node
}
node_get_car <- function(node, i) {
  node_car(node_get(node, i))
}

#' @rdname new_node
#' @export
node_tag <- function(x) {
  .Call(ffi_node_tag, x)
}
#' @rdname new_node
#' @export
node_poke_tag <- function(x, newtag) {
  invisible(.Call(ffi_node_poke_tag, x, newtag))
}

#' Is object a node or pairlist?
#'
#' @description
#'
#' * `is_pairlist()` checks that `x` has type `pairlist`.
#'
#' * `is_node()` checks that `x` has type `pairlist` or `language`.
#'    It tests whether `x` is a node that has a CAR and a CDR,
#'    including callable nodes (language objects).
#'
#' * `is_node_list()` checks that `x` has type `pairlist` or `NULL`.
#'   `NULL` is the empty node list.
#'
#'
#' @section Life cycle:
#'
#' These functions are experimental. We are still figuring out a good
#' naming convention to refer to the different lisp-like lists in R.
#'
#' @param x Object to test.
#' @seealso [is_call()] tests for language nodes.
#' @keywords internal
#' @export
is_pairlist <- function(x) {
  typeof(x) == "pairlist"
}
#' @rdname is_pairlist
#' @export
is_node <- function(x) {
  typeof(x) %in% c("pairlist", "language")
}
#' @rdname is_pairlist
#' @export
is_node_list <- function(x) {
  typeof(x) %in% c("pairlist", "NULL")
}

# Shallow copy of node trees
node_tree_clone <- function(x) {
  .Call(ffi_node_tree_clone, x);
}

node_walk <- function(.x, .f, ...) {
  cur <- .x
  while (!is.null(cur)) {
    .f(cur, ...)
    cur <- node_cdr(cur)
  }
  NULL
}
node_walk_nonnull <- function(.x, .f, ...) {
  cur <- .x
  out <- NULL
  while (!is.null(cur) && is.null(out)) {
    out <- .f(cur, ...)
    cur <- node_cdr(cur)
  }
  out
}
node_walk_last <- function(.x, .f, ...) {
  cur <- .x
  while (!is.null(node_cdr(cur))) {
    cur <- node_cdr(cur)
  }
  .f(cur, ...)
}

node_append <- function(.x, .y) {
  node_walk_last(.x, function(l) node_poke_cdr(l, .y))
  .x
}

node_list_reverse <- function(x) {
  .Call(ffi_pairlist_rev, x)
}


#' Create a new call from components
#'
#' @param car The head of the call. It should be a
#'   [callable][is_callable] object: a symbol, call, or literal
#'   function.
#' @param cdr The tail of the call, i.e. a [pairlist][new_node] of
#'   arguments.
#'
#' @keywords internal
#' @export
new_call <- function(car, cdr = NULL) {
  .Call(ffi_new_call, car, cdr)
}
