#' Coerce an object to a base type
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' These are equivalent to the base functions (e.g. [as.logical()],
#' [as.list()], etc), but perform coercion rather than conversion.
#' This means they are not generic and will not call S3 conversion
#' methods. They only attempt to coerce the base type of their
#' input. In addition, they have stricter implicit coercion rules and
#' will never attempt any kind of parsing. E.g. they will not try to
#' figure out if a character vector represents integers or booleans.
#' Finally, they treat attributes consistently, unlike the base R
#' functions: all attributes except names are removed.
#'
#'
#' @section Coercion to logical and numeric atomic vectors:
#'
#' * To logical vectors: Integer and integerish double vectors. See
#'   [is_integerish()].
#' * To integer vectors: Logical and integerish double vectors.
#' * To double vectors: Logical and integer vectors.
#' * To complex vectors: Logical, integer and double vectors.
#'
#'
#' @section Coercion to character vectors:
#'
#' `as_character()` and `as_string()` have an optional `encoding`
#' argument to specify the encoding. R uses this information for
#' internal handling of strings and character vectors. Note that this
#' is only declarative, no encoding conversion is attempted. See
#' [as_utf8_character()] and [as_native_character()] for coercing to a
#' character vector and attempt encoding conversion.
#'
#' See also [set_chr_encoding()] and [mut_utf8_locale()] for
#' information about encodings and locales in R, and [string()] and
#' [chr()] for other ways of creating strings and character vectors.
#'
#' Note that only `as_string()` can coerce symbols to a scalar
#' character vector. This makes the code more explicit and adds an
#' extra type check.
#'
#'
#' @section Coercion to lists:
#'
#' `as_list()` only coerces vector and dictionary types (environments
#' are an example of dictionary type). Unlike [base::as.list()],
#' `as_list()` removes all attributes except names.
#'
#'
#' @section Effects of removing attributes:
#'
#' A technical side-effect of removing the attributes of the input is
#' that the underlying objects has to be copied. This has no
#' performance implications in the case of lists because this is a
#' shallow copy: only the list structure is copied, not the contents
#' (see [duplicate()]). However, be aware that atomic vectors
#' containing large amounts of data will have to be copied.
#'
#' In general, any attribute modification creates a copy, which is why
#' it is better to avoid using attributes with heavy atomic vectors.
#' Uncopyable objects like environments and symbols are an exception
#' to this rule: in this case, attributes modification happens in
#' place and has side-effects.
#'
#' @inheritParams string
#' @param x An object to coerce to a base type.
#'
#' @keywords internal
#' @examples
#' # Coercing atomic vectors removes attributes with both base R and rlang:
#' x <- structure(TRUE, class = "foo", bar = "baz")
#' as.logical(x)
#'
#' # But coercing lists preserves attributes in base R but not rlang:
#' l <- structure(list(TRUE), class = "foo", bar = "baz")
#' as.list(l)
#' as_list(l)
#'
#' # Implicit conversions are performed in base R but not rlang:
#' as.logical(l)
#' \dontrun{
#' as_logical(l)
#' }
#'
#' # Conversion methods are bypassed, making the result of the
#' # coercion more predictable:
#' as.list.foo <- function(x) "wrong"
#' as.list(l)
#' as_list(l)
#'
#' # The input is never parsed. E.g. character vectors of numbers are
#' # not converted to numeric types:
#' as.integer("33")
#' \dontrun{
#' as_integer("33")
#' }
#'
#'
#' # With base R tools there is no way to convert an environment to a
#' # list without either triggering method dispatch, or changing the
#' # original environment. as_list() makes it easy:
#' x <- structure(as_environment(mtcars[1:2]), class = "foobar")
#' as.list.foobar <- function(x) abort("dont call me")
#' as_list(x)
#' @name vector-coercion
NULL

#' @rdname vector-coercion
#' @export
as_logical <- function(x) {
  coerce_type_vec(x, friendly_type("logical"),
    logical = { attributes(x) <- NULL; x },
    integer = as_base_type(x, as.logical),
    double = as_integerish_type(x, as.logical, "logical")
  )
}
#' @rdname vector-coercion
#' @export
as_integer <- function(x) {
  coerce_type_vec(x, friendly_type("integer"),
    logical = as_base_type(x, as.integer),
    integer = { attributes(x) <- NULL; x },
    double = as_integerish_type(x, as.integer, "integer")
  )
}
#' @rdname vector-coercion
#' @export
as_double <- function(x) {
  coerce_type_vec(x, friendly_type("double"),
    logical = ,
    integer = as_base_type(x, as.double),
    double = { attributes(x) <- NULL; x }
  )
}
#' @rdname vector-coercion
#' @export
as_complex <- function(x) {
  coerce_type_vec(x, friendly_type("complex"),
    logical = ,
    integer = ,
    double = as_base_type(x, as.complex),
    complex = { attributes(x) <- NULL; x }
  )
}
#' @rdname vector-coercion
#' @export
as_character <- function(x, encoding = NULL) {
  coerce_type_vec(x, friendly_type("character"),
    string = ,
    character = {
      attributes(x) <- NULL
      set_chr_encoding(x, encoding)
    }
  )
}
#' @rdname vector-coercion
#' @export
as_list <- function(x) {
  switch_type(x,
    environment = env_as_list(x),
    vec_as_list(x)
  )
}
env_as_list <- function(x) {
  names_x <- names(x)
  x <- as_base_type(x, as.list)
  set_names(x, .Call(rlang_unescape_character, names_x))
}
vec_as_list <- function(x) {
  coerce_type_vec(x, friendly_type("list"),
    logical = ,
    integer = ,
    double = ,
    string = ,
    character = ,
    complex = ,
    raw = as_base_type(x, as.list),
    list = { attributes(x) <- NULL; x }
  )
}

as_base_type <- function(x, as_type) {
  # Zap attributes temporarily instead of unclassing. We want to avoid
  # method dispatch, but we also want to avoid an extra copy of atomic
  # vectors: the first when unclassing, the second when coercing. This
  # is also useful for uncopyable types like environments.
  attrs <- .Call(rlang_get_attributes, x)
  .Call(rlang_poke_attributes, x, NULL)

  # This function assumes that the target type is different than the
  # input type, otherwise no duplication is done and the output will
  # be modified by side effect when we restore the input attributes.
  on.exit(.Call(rlang_poke_attributes, x, attrs))

  as_type(x)
}
as_integerish_type <- function(x, as_type, to) {
  if (is_integerish(x)) {
    as_base_type(x, as_type)
  } else {
    abort(paste0(
      "Can't convert a fractional double vector to ", friendly_type(to), ""
    ))
  }
}

coerce_type_vec <- function(.x, .to, ...) {
  # Cannot reuse coerce_type() because switch() has a bug with
  # fallthrough and multiple levels of dots forwarding.
  out <- switch(type_of(.x), ..., abort_coercion(.x, .to))

  if (!is_null(names(.x))) {
    # Avoid a copy of `out` when we restore the names, since it could be
    # a heavy atomic vector. We own `out`, so it is ok to change its
    # attributes inplace.
    .Call(rlang_poke_attributes, out, pairlist(names = names(.x)))
  }

  out
}


vec_coerce <- function(x, type) {
  .Call(rlang_vec_coerce, x, type)
}
