#' Helpers for pairlist and language nodes
#'
#' @description
#'
#' These functions are mostly useful to navigate ASTs which are
#' organised as binary trees of [cons
#' cells](https://en.wikipedia.org/wiki/CAR_and_CDR). They are low
#' level getters and setters that don't perform any type checking. As
#' such, they can easily make R crash. Their main purpose is to
#' prototype C code for computing on the language and they are meant
#' for R experts only.
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
#' @examples
#' # Changing a node component happens in place and can have side
#' # effects. Let's create a language object and a copy of it:
#' lang <- quote(foo(bar))
#' copy <- lang
#'
#' # Using R's builtin operators to change the language tree does not
#' # create side effects:
#' copy[[2]] <- quote(baz)
#' copy
#' lang
#'
#' # On the other hand, the CAR and CDR operators operate in-place. Let's
#' # create new objects since the previous examples triggered a copy:
#' lang <- quote(foo(bar))
#' copy <- lang
#'
#' # Now we change the argument pairlist of `copy`, making sure the new
#' # arguments are NULL-terminated:
#' node_poke_cdr(copy, node(quote(BAZ), NULL))
#'
#' # Or equivalently:
#' node_poke_cdr(copy, pairlist(quote(BAZ)))
#' copy
#'
#' # The original object has been changed in place:
#' lang
#' @name node
NULL

#' @rdname node
#' @export
node <- function(car, cdr = NULL) {
  .Call(rlang_new_node, car, cdr)
}

#' @rdname node
#' @export
node_car <- function(x) {
  .Call(rlang_node_car, x)
}
#' @rdname node
#' @export
node_cdr <- function(x) {
  .Call(rlang_node_cdr, x)
}
#' @rdname node
#' @export
node_caar <- function(x) {
  .Call(rlang_node_caar, x)
}
#' @rdname node
#' @export
node_cadr <- function(x) {
  .Call(rlang_node_cadr, x)
}
#' @rdname node
#' @export
node_cdar <- function(x) {
  .Call(rlang_node_cdar, x)
}
#' @rdname node
#' @export
node_cddr <- function(x) {
  .Call(rlang_node_cddr, x)
}

#' @rdname node
#' @export
node_poke_car <- function(x, newcar) {
  invisible(.Call(rlang_node_poke_car, x, newcar))
}
#' @rdname node
#' @export
node_poke_cdr <- function(x, newcdr) {
  invisible(.Call(rlang_node_poke_cdr, x, newcdr))
}
#' @rdname node
#' @export
node_poke_caar <- function(x, newcar) {
  invisible(.Call(rlang_node_poke_caar, x, newcar))
}
#' @rdname node
#' @export
node_poke_cadr <- function(x, newcar) {
  invisible(.Call(rlang_node_poke_cadr, x, newcar))
}
#' @rdname node
#' @export
node_poke_cdar <- function(x, newcdr) {
  invisible(.Call(rlang_node_poke_cdar, x, newcdr))
}
#' @rdname node
#' @export
node_poke_cddr <- function(x, newcdr) {
  invisible(.Call(rlang_node_poke_cddr, x, newcdr))
}

#' @rdname node
#' @export
node_tag <- function(x) {
  .Call(rlang_node_tag, x)
}
#' @rdname node
#' @export
node_poke_tag <- function(x, newtag) {
  invisible(.Call(rlang_node_poke_tag, x, newtag))
}

#' Coerce to pairlist
#'
#' This transforms vector objects to a linked pairlist of nodes. See
#' the [pairlist][node] type help page.
#'
#' @param x An object to coerce.
#' @export
as_pairlist <- function(x) {
  if (!is_vector(x)) {
    abort_coercion(x, "pairlist")
  }
  as.vector(x, "pairlist")
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


#' Duplicate an R object
#'
#' In R semantics, objects are copied by value. This means that
#' modifying the copy leaves the original object intact. Since,
#' copying data in memory is an expensive operation, copies in R are
#' as lazy as possible. They only happen when the new object is
#' actually modified. However, some operations (like [node_poke_car()]
#' or [node_poke_cdr()]) do not support copy-on-write. In those cases,
#' it is necessary to duplicate the object manually in order to
#' preserve copy-by-value semantics.
#'
#' Some objects are not duplicable, like symbols and environments.
#' `duplicate()` returns its input for these unique objects.
#'
#' @param x Any R object. However, uncopyable types like symbols and
#'   environments are returned as is (just like with `<-`).
#' @param shallow This is relevant for recursive data structures like
#'   lists, calls and pairlists. A shallow copy only duplicates the
#'   top-level data structure. The objects contained in the list are
#'   still the same.
#' @seealso pairlist
#' @keywords internal
#' @export
duplicate <- function(x, shallow = FALSE) {
  .Call(rlang_duplicate, x, shallow)
}

# Shallow copy of node trees
node_tree_clone <- function(x) {
  .Call(rlang_node_tree_clone, x);
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


#' Create a new call from components
#'
#' @param car The head of the call. It should be a
#'   [callable][is_callable] object: a symbol, call, or literal
#'   function.
#' @param cdr The tail of the call, i.e. a [node list][node] of
#'   arguments.
#'
#' @keywords internal
#' @export
call_node <- function(car, cdr = NULL) {
  .Call(rlang_new_call, car, cdr)
}
