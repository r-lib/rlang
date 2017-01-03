#' Helper to create vectors with matching length.
#'
#' These functions take the idea of \code{\link{seq_along}} and
#' generalise it to creating lists (\code{list_along}) and repeating
#' values (\code{rep_along}). The dots are forwarded to
#' \code{\link[base]{structure}()} to make it easy to add attributes.
#'
#' @inheritParams with_attributes
#' @param .x A vector.
#' @param .y Values to repeat.
#' @examples
#' x <- 0:5
#' rep_along(x, 1:2)
#' rep_along(x, 1)
#' list_along(x)
#'
#' # You can also add attributes with additional arguments:
#' rep_along(x, 1, class = "my_class")
#' double_along(x, class = "my_class")
#' @name along
#' @seealso new-vectors
NULL

#' @export
#' @rdname along
logical_along <- function(.x, ..., .attrs = list()) {
  with_attributes(vector("logical", length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
integer_along <- function(.x, ..., .attrs = list()) {
  with_attributes(vector("integer", length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
double_along <- function(.x, ..., .attrs = list()) {
  with_attributes(vector("double", length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
character_along <- function(.x, ..., .attrs = list()) {
  with_attributes(vector("character", length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
list_along <- function(.x, ..., .attrs = list()) {
  with_attributes(vector("list", length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
complex_along <- function(.x, ..., .attrs = list()) {
  with_attributes(vector("complex", length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
raw_along <- function(.x, ..., .attrs = list()) {
  with_attributes(vector("raw", length(.x)), ..., .attrs = .attrs)
}

#' @export
#' @rdname along
rep_along <- function(.x, .y, ..., .attrs = list()) {
  with_attributes(rep(.y, length.out = length(.x)), ..., .attrs = .attrs)
}


#' Create new vectors.
#'
#' These functions construct vectors of given length, with attributes
#' specified via dots.
#'
#' @inheritParams with_attributes
#' @param .n The vector length.
#' @examples
#' list_until(10)
#'
#' # Add attributes, including the S3 class:
#' integer_until(0, index = 1)
#' double_until(10, class = "my_class")
#' @name new-vectors
#' @seealso along
NULL

#' @export
#' @rdname new-vectors
logical_until <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("logical", .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
integer_until <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("integer", .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
double_until <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("double", .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
character_until <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("character", .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
list_until <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("list", .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
complex_until <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("complex", .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
raw_until <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("raw", .n), ..., .attrs = .attrs)
}
