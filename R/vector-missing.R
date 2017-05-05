#' Missing values
#'
#' Missing values are represented in R with the general symbol
#' `NA`. They can be inserted in almost all data containers: all
#' atomic vectors except raw vectors can contain missing values. To
#' achieve this, R automatically converts the general `NA` symbol to a
#' typed missing value appropriate for the target vector. The objects
#' provided here are aliases for those typed `NA` objects.
#'
#' Typed missing values are necessary because R needs sentinel values
#' of the same type (i.e. the same machine representation of the data)
#' as the containers into which they are inserted. The official typed
#' missing values are `NA_integer_`, `NA_real_`, `NA_character_` and
#' `NA_complex_`. The missing value for logical vectors is simply the
#' default `NA`. The aliases provided in rlang are consistently named
#' and thus simpler to remember. Also, `na_lgl` is provided as an
#' alias to `NA` that makes intent clearer.
#'
#' Since `na_lgl` is the default `NA`, expressions such as `c(NA, NA)`
#' yield logical vectors as no data is available to give a clue of the
#' target type. In the same way, since lists and environments can
#' contain any types, expressions like `list(NA)` store a logical
#' `NA`.
#'
#' @seealso The [vector-along] family to create typed vectors filled
#'   with missing values.
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


#' Test for missing values
#'
#' `are_na()` checks for missing values in a vector and is equivalent
#' to [base::is.na()]. It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' `is_na()` is a scalar predicate and always returns a scalar
#' boolean, `TRUE` or `FALSE`. If its input is not scalar, it returns
#' `FALSE`. Finally, there are typed versions that check for
#' particular [missing types][missing].
#'
#' The scalar predicates accept non-vector inputs. They are equivalent
#' to [is_null()] in that respect. In contrast the vectorised
#' predicate `are_na()` requires a vector input since it is defined
#' over vector values.
#'
#' @param x An object to test
#' @export
#' @examples
#' # are_na() is vectorised and works regardless of the type
#' are_na(c(1, 2, NA))
#' are_na(c(1L, NA, 3L))
#'
#' # is_na() checks for scalar input and works for all types
#' is_na(NA)
#' is_na(na_dbl)
#' is_na(character(0))
#'
#' # There are typed versions as well:
#' is_lgl_na(NA)
#' is_lgl_na(na_dbl)
are_na <- function(x) {
  if (!is_vector(x)) {
    abort("`x` must be a vector")
  }
  is.na(x)
}
#' @rdname are_na
#' @export
is_na <- function(x) {
  is_scalar_vector(x) && is.na(x)
}

#' @rdname are_na
#' @export
is_lgl_na <- function(x) {
  identical(x, na_lgl)
}
#' @rdname are_na
#' @export
is_int_na <- function(x) {
  identical(x, na_int)
}
#' @rdname are_na
#' @export
is_dbl_na <- function(x) {
  identical(x, na_dbl)
}
#' @rdname are_na
#' @export
is_chr_na <- function(x) {
  identical(x, na_chr)
}
#' @rdname are_na
#' @export
is_cpl_na <- function(x) {
  identical(x, na_cpl)
}
