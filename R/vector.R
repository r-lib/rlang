#' Splice objects and lists of objects into a list
#'
#' This splices all arguments into a list. Non-list objects and lists
#' with a S3 class are encapsulated in a list before concatenation.
#' @param ... Objects to concatenate.
#' @export
#' @examples
#' inputs <- list(arg1 = "a", arg2 = "b")
#'
#' # splice() concatenates the elements of inputs with arg3
#' str(splice(inputs, arg3 = c("c1", "c2")))
#' str(list(inputs, arg3 = c("c1", "c2")))
#' str(c(inputs, arg3 = c("c1", "c2")))
splice <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) {
    return(dots)
  }

  names <- Map(function(dot, name) {
    if (is_bare_list(dot)) {
      names2(dot)
    } else {
      name
    }
  }, dots, names2(dots))
  names <- unlist(names, recursive = FALSE)

  is_not_list <- vapply_lgl(dots, function(x) !is_bare_list(x))
  dots[is_not_list] <- lapply(dots[is_not_list], list)

  out <- unlist(dots, recursive = FALSE)
  set_names(out, names)
}


#' Prepend a vector.
#'
#' This is a companion to \code{\link[base]{append}()} to help merging
#' two lists or atomic vectors. \code{prepend()} is a clearer semantic
#' signal than `c()` that a vector is to be merged at the beginning of
#' another, especially in a pipe chain.
#'
#' @param x the vector to be modified.
#' @param values to be included in the modified vector.
#' @param before a subscript, before which the values are to be appended.
#' @return A merged vector.
#' @export
#' @examples
#' x <- as.list(1:3)
#'
#' append(x, "a")
#' prepend(x, "a")
#' prepend(x, list("a", "b"), before = 3)
prepend <- function(x, values, before = 1) {
  n <- length(x)
  stopifnot(before > 0 && before <= n)

  if (before == 1) {
    c(values, x)
  } else {
    c(x[1:(before - 1)], values, x[before:n])
  }
}
