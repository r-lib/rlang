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
#' @param x An object to test for inheritance.
#' @param classes A character vector of at least one class.
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
inherits_any <- function(x, classes) {
  if (is_empty(classes)) {
    abort("`classes` can't be empty")
  }
  inherits(x, classes)
}
#' @rdname inherits_any
#' @export
inherits_all <- function(x, classes) {
  if (is_empty(classes)) {
    abort("`classes` can't be empty")
  }

  idx <- inherits(x, classes, which = TRUE)
  cummax <- cummax(idx)

  cummax[[1]] != 0 && all(idx == cummax(idx))
}


#' Box a value
#'
#' `box()` is similar to [base::I()] but it protects a value by
#' wrapping it in a scalar list rather than by adding an attribute.
#' `unbox()` retrieves the boxed value. `is_box()` tests whether an
#' object is boxed with optional class. `as_box()` ensures that a
#' value is wrapped in a box.
#'
#' @param x An R object.
#' @param class For `box()`, an additional class for the boxed value
#'   (which comes in addition to `box`). For `is_box()` and
#'   `as_box()`, a class (or vector of classes) to be passed to
#'   [inherits_all()].
#' @export
#' @examples
#' boxed <- box(letters, "mybox")
#' is_box(boxed)
#' is_box(boxed, "mybox")
#' is_box(boxed, "otherbox")
#'
#' unbox(boxed)
box <- function(x, class = NULL) {
  set_attrs(list(x), class = c(class, "box"))
}
#' @rdname box
#' @export
is_box <- function(x, class = NULL) {
  inherits_all(x, c(class, "box"))
}
#' @rdname box
#' @export
as_box <- function(x, class = NULL) {
  if (is_box(x, class)) {
    x
  } else {
    box(x, class)
  }
}
#' @rdname box
#' @param box A boxed value to unbox.
#' @export
unbox <- function(box) {
  stopifnot(inherits(box, "box"))
  box[[1]]
}
print.box <- function(x, ...) {
  meow("<box>")
  print(unbox(x))
}
