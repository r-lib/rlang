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

  is_not_list <- map_lgl(dots, function(x) !is_bare_list(x))
  dots[is_not_list] <- map(dots[is_not_list], list)

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

#' Modify a vector.
#'
#' This function merges a list of arguments into a vector. It always
#' returns a list.
#'
#' @param .x A vector to modify.
#' @param ...,.elts List of elements to merge into \code{.x}. Named
#'   elements already existing in \code{.x} are used as
#'   replacements. Elements that have new or no names are inserted at
#'   the end.
#' @return A modified vector upcasted to a list.
#' @export
#' @examples
#' modify(c(1, b = 2, 3), 4, b = "foo")
#'
#' x <- list(a = 1, b = 2)
#' y <- list(b = 3, c = 4)
#' modify(x, .elts = y)
modify <- function(.x, ..., .elts = list()) {
  out <- as.list(.x)
  args <- c(list(...), .elts)

  args_nms <- names(args)
  exists <- has_names(args) & args_nms %in% names(out)

  for (nm in args_nms[exists]) {
    out[[nm]] <- args[[nm]]
  }

  c(out, args[!exists])
}


#' Missing values.
#'
#' Missing values are represented in R with the general symbol
#' \code{NA}. They can be inserted in almost all data containers: all
#' atomic vectors except raw vectors can contain missing values. To
#' achieve this, R automatically converts the general \code{NA} symbol
#' to a typed missing value appropriate for the target vector. The
#' objects provided here are aliases for those typed \code{NA}
#' objects.
#'
#' Typed missing values are necessary because R needs sentinel values
#' of the same type (i.e. the same machine representation of the data)
#' as the containers into which they are inserted. The official typed
#' missing values are \code{NA_integer_}, \code{NA_real_},
#' \code{NA_character_} and \code{NA_complex_}. The missing value for
#' logical vectors is simply the default \code{NA}. The aliases
#' provided in rlang are consistently named and thus simpler to
#' remember. Also, \code{na_lgl} is provided as an alias to \code{NA}
#' that makes intent clearer.
#'
#' Since \code{na_lgl} is the default \code{NA}, expressions such as
#' \code{c(NA, NA)} yield logical vectors as no data is available to
#' give a clue of the target type. In the same way, since lists and
#' environments can contain any types, expressions like
#' \code{list(NA)} store a logical \code{NA}.
#'
#' @seealso The \code{\link{along}} family to create typed vectors
#'   filled with missing values.
#' @examples
#' typeof(NA)
#' typeof(na_lgl)
#' typeof(na_int)
#'
#' # Note that while the base R missing symbols cannot be overwritten,
#' # that's not the case for rlang's aliases:
#' na_dbl <- NA
#' typeof(na_dbl)
#' @name missing
NULL

#' @rdname missing
#' @export
na_lgl <- NA
#' @rdname missing
#' @export
na_int <- NA_integer_
#' @rdname missing
#' @export
na_dbl <- NA_real_
#' @rdname missing
#' @export
na_chr <- NA_character_
#' @rdname missing
#' @export
na_cpl <- NA_complex_


#' Helper to create vectors with matching length.
#'
#' These functions take the idea of \code{\link{seq_along}} and
#' generalise it to creating lists (\code{lst_along}) and repeating
#' values (\code{rep_along}). The dots and \code{.attrs} are forwarded
#' to \code{\link{with_attributes}()} to make it easy to add
#' attributes. Except for \code{lst_along()} and \code{raw_along()},
#' the empty vectors are filled with typed \link{missing} values.
#'
#' @inheritParams with_attributes
#' @param .x A vector.
#' @param .y Values to repeat.
#' @examples
#' x <- 0:5
#' rep_along(x, 1:2)
#' rep_along(x, 1)
#' lst_along(x)
#'
#' # You can also add attributes with additional arguments:
#' rep_along(x, 1, class = "my_class")
#' dbl_along(x, class = "my_class")
#' @name along
#' @seealso new-vectors
NULL

#' @export
#' @rdname along
lgl_along <- function(.x, ..., .attrs = list()) {
  with_attributes(rep_len(na_lgl, length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
int_along <- function(.x, ..., .attrs = list()) {
  with_attributes(rep_len(na_int, length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
dbl_along <- function(.x, ..., .attrs = list()) {
  with_attributes(rep_len(na_dbl, length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
chr_along <- function(.x, ..., .attrs = list()) {
  with_attributes(rep_len(na_chr, length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
lst_along <- function(.x, ..., .attrs = list()) {
  with_attributes(vector("list", length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
cpl_along <- function(.x, ..., .attrs = list()) {
  with_attributes(rep_len(na_cpl, length(.x)), ..., .attrs = .attrs)
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
#' specified via dots. Except for \code{new_lst()} and
#' \code{new_raw()}, the empty vectors are filled with typed
#' \link{missing} values. This is in contrast to the base function
#' \code{\link[base]{vector}()} which creates zero-filled vectors.
#'
#' @inheritParams with_attributes
#' @param .n The vector length.
#' @examples
#' new_lst(10)
#'
#' # Add attributes, including the S3 class:
#' new_int(0, index = 1)
#' new_dbl(10, class = "my_class")
#' @name new-vectors
#' @seealso along
NULL

#' @export
#' @rdname new-vectors
new_lgl <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(rep_len(na_lgl, .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
new_int <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(rep_len(na_int, .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
new_dbl <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(rep_len(na_dbl, .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
new_chr <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(rep_len(na_chr, .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
new_lst <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("list", .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
new_cpl <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(rep_len(na_cpl, .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
new_raw <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("raw", .n), ..., .attrs = .attrs)
}
