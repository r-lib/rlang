#' Box a value
#'
#' `box()` is similar to [base::I()] but it protects a value by
#' wrapping it in a scalar list rather than by adding an attribute.
#' `unbox()` retrieves the boxed value. `is_box()` tests whether an
#' object is boxed with optional class.
#'
#' @param x An R object.
#' @param class For `box()`, an additional class for the boxed value
#'   (which comes in addition to `box`). For `is_box()`, a class that
#'   is passed to [inherits()].
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
  if (!inherits(x, "box")) {
    return(FALSE)
  }

  is_null(class) || inherits(x, class)
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
