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
#' @param class For `new_box()`, an additional class for the
#'   boxed value (in addition to `rlang_box`). For `is_box()`, a class
#'   or vector of classes passed to [inherits_all()].
#' @param ... Additional attributes passed to [base::structure()].
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
new_box <- function(.x, class = NULL, ...) {
  structure(
    list(.x),
    class = c(class, "rlang_box"),
    ...
  )
}
#' @rdname box
#' @export
is_box <- function(x, class = NULL) {
  inherits_all(x, c(class, "rlang_box"))
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
  cat_line("<box>")
  print(unbox(x))
}

#' Convert object to a box
#'
#' @description
#'
#' * `as_box()` boxes its input only if it is not already a box. The
#'   class is also checked if supplied.
#'
#' * `as_box_if()` boxes its input only if it not already a box, or if
#'   the predicate `.p` returns `TRUE`.
#'
#' @inheritParams box
#' @param class,.class A box class. If the input is already a box of
#'   that class, it is returned as is. If the input needs to be boxed,
#'   `class` is passed to [new_box()].
#'
#' @export
as_box <- function(x, class = NULL) {
  if (is_box(x, class)) {
    x
  } else {
    new_box(x, class)
  }
}
#' @rdname as_box
#' @param .p A predicate function.
#' @param ... Arguments passed to `.p`.
#' @export
as_box_if <- function(.x, .p, .class = NULL, ...) {
  .p <- as_predicate(.p)
  if (is_box(.x, .class) || !.p(.x, ...)) {
    .x
  } else {
    new_box(.x, .class)
  }
}

#' Box a final value for early termination
#'
#' @description
#'
#' A value boxed with `done()` signals to its caller that it
#' should stop iterating. Use it to shortcircuit a loop.
#'
#' @param x For `done()`, a value to box. For `is_done_box()`, a
#'   value to test.
#' @return A [boxed][new_box] value.
#'
#' @examples
#' done(3)
#'
#' x <- done(3)
#' is_done_box(x)
#' @export
done <- function(x) {
  new_box(
    maybe_missing(x),
    class = "rlang_box_done",
    empty = missing(x)
  )
}
#' @rdname done
#' @param empty Whether the box is empty. If `NULL`, `is_done_box()`
#'   returns `TRUE` for all done boxes. If `TRUE`, it returns `TRUE`
#'   only for empty boxes. Otherwise it returns `TRUE` only for
#'   non-empty boxes.
#' @export
is_done_box <- function(x, empty = NULL) {
  if (!inherits(x, "rlang_box_done")) {
    return(FALSE)
  }

  if (is_null(empty)) {
    return(TRUE)
  }

  attr(x, "empty") == empty
}
#' @export
print.rlang_box_done <- function(x, ...) {
  cat_line("<done>")
  print(unbox(x))
}

#' Create zap objects
#'
#' @description
#'
#' `zap()` creates a sentinel object that indicates that an object
#' should be removed. For instance, named zaps instruct [env_bind()]
#' and [call_modify()] to remove those objects from the environment or
#' the call.
#'
#' The advantage of zap objects is that they unambiguously signal the
#' intent of removing an object. Sentinels like `NULL` or
#' [missing_arg()] are ambiguous because they represent valid R
#' objects.
#'
#' @param x An object to test.
#'
#' @export
#' @examples
#' # Create one zap object:
#' zap()
#'
#' # Create a list of zaps:
#' rep(list(zap()), 3)
#' rep_named(c("foo", "bar"), list(zap()))
zap <- function() {
  `zap!`
}
#' @rdname zap
#' @export
is_zap <- function(x) {
  inherits(x, "rlang_zap")
}

`zap!` <- structure(list(), class = "rlang_zap")

#' @export
print.rlang_zap <- function(x, ...) {
  cat_line("<zap>")
}
