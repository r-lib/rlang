#' Helper to create vectors with matching length.
#'
#' These functions take the idea of \code{\link{seq_along}} and
#' generalise it to creating lists (\code{list_along}) and repeating
#' values (\code{rep_along}). The dots are forwarded to
#' \code{\link[base]{structure}()} to make it easy to add attributes.
#'
#' @param ... Optional attributes for the vector. These are passed to
#'   \code{\link[base]{structure}()}.
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
logical_along <- function(.x, ...) {
  structure(vector("logical", length(.x)), ...)
}
#' @export
#' @rdname along
integer_along <- function(.x, ...) {
  structure(vector("integer", length(.x)), ...)
}
#' @export
#' @rdname along
double_along <- function(.x, ...) {
  structure(vector("double", length(.x)), ...)
}
#' @export
#' @rdname along
character_along <- function(.x, ...) {
  structure(vector("character", length(.x)), ...)
}
#' @export
#' @rdname along
list_along <- function(.x, ...) {
  structure(vector("list", length(.x)), ...)
}
#' @export
#' @rdname along
complex_along <- function(.x, ...) {
  structure(vector("complex", length(.x)), ...)
}
#' @export
#' @rdname along
raw_along <- function(.x, ...) {
  structure(vector("raw", length(.x)), ...)
}

#' @export
#' @rdname along
rep_along <- function(.x, .y, ...) {
  structure(rep(.y, length.out = length(.x)), ...)
}


#' Create new vectors.
#'
#' These functions construct vectors of given length, with attributes
#' specified via dots.
#'
#' @inheritParams along
#' @param .n The vector length.
#' @examples
#' new_list(10)
#'
#' # Add attributes, including the S3 class:
#' new_integer(0, index = 1)
#' new_double(10, class = "my_class")
#' @name new-vectors
#' @seealso along
NULL

#' @export
#' @rdname new-vectors
new_logical <- function(.n = 0, ...) {
  structure(vector("logical", .n), ...)
}
#' @export
#' @rdname new-vectors
new_integer <- function(.n = 0, ...) {
  structure(vector("integer", .n), ...)
}
#' @export
#' @rdname new-vectors
new_double <- function(.n = 0, ...) {
  structure(vector("double", .n), ...)
}
#' @export
#' @rdname new-vectors
new_character <- function(.n = 0, ...) {
  structure(vector("character", .n), ...)
}
#' @export
#' @rdname new-vectors
new_list <- function(.n = 0, ...) {
  structure(vector("list", .n), ...)
}
#' @export
#' @rdname new-vectors
new_complex <- function(.n = 0, ...) {
  structure(vector("complex", .n), ...)
}
#' @export
#' @rdname new-vectors
new_raw <- function(.n = 0, ...) {
  structure(vector("raw", .n), ...)
}
