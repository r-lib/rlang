#' Does an object inherit from a set of classes?
#'
#' @description
#'
#' * `inherits_any()` is like [base::inherits()] but is more explicit
#'   about its behaviour with multiple classes. If `classes` contains
#'   several elements and the object inherits from at least one of
#'   them, `inherits_any()` returns `TRUE`.
#'
#' * `inherits_all()` tests that an object inherits from all of the
#'   classes in the supplied order. This is usually the best way to
#'   test for inheritance of multiple classes.
#'
#' * `inherits_only()` tests that the class vectors are identical. It
#'   is a shortcut for `identical(class(x), class)`.
#'
#' @param x An object to test for inheritance.
#' @param class A character vector of classes.
#'
#' @export
#' @examples
#' obj <- structure(list(), class = c("foo", "bar", "baz"))
#'
#' # With the _any variant only one class must match:
#' inherits_any(obj, c("foobar", "bazbaz"))
#' inherits_any(obj, c("foo", "bazbaz"))
#'
#' # With the _all variant all classes must match:
#' inherits_all(obj, c("foo", "bazbaz"))
#' inherits_all(obj, c("foo", "baz"))
#'
#' # The order of classes must match as well:
#' inherits_all(obj, c("baz", "foo"))
#'
#' # inherits_only() checks that the class vectors are identical:
#' inherits_only(obj, c("foo", "baz"))
#' inherits_only(obj, c("foo", "bar", "baz"))
inherits_any <- function(x, class) {
  if (is_empty(class)) {
    abort("`class` can't be empty")
  }
  inherits(x, class)
}
#' @rdname inherits_any
#' @export
inherits_all <- function(x, class) {
  if (is_empty(class)) {
    abort("`class` can't be empty")
  }

  idx <- inherits(x, class, which = TRUE)
  cummax <- cummax(idx)

  cummax[[1]] != 0L && all(idx == cummax)
}
#' @rdname inherits_any
#' @export
inherits_only <- function(x, class) {
  identical(class(x), class)
}

#' Box a value
#'
#' `new_box()` is similar to [base::I()] but it protects a value by
#' wrapping it in a scalar list rather than by adding an attribute.
#' `unbox()` retrieves the boxed value. `is_box()` tests whether an
#' object is boxed with optional class. `as_box()` ensures that a
#' value is wrapped in a box. `as_box_if()` does the same but only if
#' the value matches a predicate.
#'
#' @name box
#' @param x,.x An R object.
#' @param class,.class For `new_box()`, an additional class for the
#'   boxed value (in addition to `rlang_box`). For `is_box()`,
#'   `as_box()` and `as_box_if()`, a class (or vector of classes) to
#'   be passed to [inherits_all()].
#' @export
#' @examples
#' boxed <- new_box(letters, "mybox")
#' is_box(boxed)
#' is_box(boxed, "mybox")
#' is_box(boxed, "otherbox")
#'
#' unbox(boxed)
#'
#' # as_box() avoids double-boxing:
#' boxed2 <- as_box(boxed, "mybox")
#' boxed2
#' unbox(boxed2)
#'
#' # Compare to:
#' boxed_boxed <- new_box(boxed, "mybox")
#' boxed_boxed
#' unbox(unbox(boxed_boxed))
#'
#' # Use `as_box_if()` with a predicate if you need to ensure a box
#' # only for a subset of values:
#' as_box_if(NULL, is_null, "null_box")
#' as_box_if("foo", is_null, "null_box")
new_box <- function(x, class = NULL) {
  set_class(list(x), c(class, "rlang_box"))
}
#' @rdname box
#' @export
is_box <- function(x, class = NULL) {
  inherits_all(x, c(class, "rlang_box"))
}
#' @rdname box
#' @export
as_box <- function(x, class = NULL) {
  if (is_box(x, class)) {
    x
  } else {
    new_box(x, class)
  }
}
#' @rdname box
#' @param .p A predicate function.
#' @param ... Arguments passed to `.p`.
#' @export
as_box_if <- function(.x, .p, .class = NULL, ...) {
  if (is_box(.x, .class) || !.p(.x, ...)) {
    .x
  } else {
    new_box(.x, .class)
  }
}
#' @rdname box
#' @param box A boxed value to unbox.
#' @export
unbox <- function(box) {
  if (!inherits(box, "rlang_box")) {
    abort("`box` must be a box")
  }
  box[[1]]
}
print.box <- function(x, ...) {
  meow("<box>")
  print(unbox(x))
}
