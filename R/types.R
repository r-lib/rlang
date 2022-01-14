#' Type predicates
#'
#' These type predicates aim to make type testing in R more
#' consistent. They are wrappers around [base::typeof()], so operate
#' at a level beneath S3/S4 etc.
#'
#' Compared to base R functions:
#'
#' * The predicates for vectors include the `n` argument for
#'   pattern-matching on the vector length.
#'
#' * Unlike `is.atomic()`, `is_atomic()` does not return `TRUE` for
#'   `NULL`.
#'
#' * Unlike `is.vector()`, `is_vector()` tests if an object is an
#'   atomic vector or a list. `is.vector` checks for the presence of
#'   attributes (other than name).
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @param finite Whether all values of the vector are finite. The
#'   non-finite values are `NA`, `Inf`, `-Inf` and `NaN`. Setting this
#'   to something other than `NULL` can be expensive because the whole
#'   vector needs to be traversed and checked.
#' @seealso [bare-type-predicates] [scalar-type-predicates]
#' @name type-predicates
NULL

#' @export
#' @rdname type-predicates
is_list <- function(x, n = NULL) {
  .Call(ffi_is_list, x, n)
}

parsable_atomic_types <- c("logical", "integer", "double", "complex", "character")
atomic_types <- c(parsable_atomic_types, "raw")
#' @export
#' @rdname type-predicates
is_atomic <- function(x, n = NULL) {
  .Call(ffi_is_atomic, x, n)
}
#' @export
#' @rdname type-predicates
is_vector <- function(x, n = NULL) {
  .Call(ffi_is_vector, x, n)
}

# Mostly for unit testing
is_finite <- function(x) {
  .Call(ffi_is_finite, x)
}

#' @export
#' @rdname type-predicates
is_integer <- function(x, n = NULL) {
  .Call(ffi_is_integer, x, n)
}
#' @export
#' @rdname type-predicates
is_double <- function(x, n = NULL, finite = NULL) {
  .Call(ffi_is_double, x, n, finite)
}
#' @export
#' @rdname type-predicates
is_complex <- function(x, n = NULL, finite = NULL) {
  .Call(ffi_is_complex, x, n, finite)
}
#' @export
#' @rdname type-predicates
is_character <- function(x, n = NULL) {
  .Call(ffi_is_character, x, n, NULL, NULL)
}
is_character2 <- function(x,
                          n = NULL,
                          ...,
                          missing = TRUE,
                          empty = TRUE) {
  check_dots_empty0(...)

  # FIXME: Change API at C-level so that `TRUE` means no restriction
  if (is_true(missing)) {
    missing <- NULL
  }
  if (is_true(empty)) {
    empty <- NULL
  }

  .Call(ffi_is_character, x, n, missing, empty)
}
#' @export
#' @rdname type-predicates
is_logical <- function(x, n = NULL) {
  .Call(ffi_is_logical, x, n)
}
#' @export
#' @rdname type-predicates
is_raw <- function(x, n = NULL) {
  .Call(ffi_is_raw, x, n)
}
#' @export
#' @rdname type-predicates
is_bytes <- is_raw

#' @export
#' @usage is_null(x)
#' @rdname type-predicates
is_null <- is.null

#' Scalar type predicates
#'
#' @description
#'
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#'
#' In addition to the length check, `is_string()` and `is_bool()`
#' return `FALSE` if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single `TRUE` or `FALSE`.
#'
#' @param x object to be tested.
#' @seealso [type-predicates], [bare-type-predicates]
#' @name scalar-type-predicates
NULL

#' @export
#' @rdname scalar-type-predicates
is_scalar_list <- function(x) {
  .Call(ffi_is_list, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_atomic <- function(x) {
  .Call(ffi_is_atomic, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_vector <- function(x) {
  .Call(ffi_is_vector, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_integer <- function(x) {
  .Call(ffi_is_integer, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_double <- function(x) {
  .Call(ffi_is_double, x, 1L, NULL)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_complex <- function(x) {
  .Call(ffi_is_complex, x, 1L, NULL)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_character <- function(x) {
  is_character(x, n = 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_logical <- function(x) {
  .Call(ffi_is_logical, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_raw <- function(x) {
  .Call(ffi_is_raw, x, 1L)
}
#' @export
#' @param string A string to compare to `x`. If a character vector,
#'   returns `TRUE` if at least one element is equal to `x`.
#' @rdname scalar-type-predicates
is_string <- function(x, string = NULL) {
  .Call(ffi_is_string, x, string, NULL)
}
is_string2 <- function(x, string = NULL, ..., empty = NULL) {
  check_dots_empty0(...)
  .Call(ffi_is_string, x, string, empty)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_bytes <- is_scalar_raw
#' @export
#' @rdname scalar-type-predicates
is_bool <- function(x) {
  is_logical(x, n = 1) && !is.na(x)
}

is_number <- function(x) {
  is_integerish(x, n = 1, finite = TRUE)
}

#' Bare type predicates
#'
#' These predicates check for a given type but only return `TRUE` for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#'
#' * The predicates for vectors include the `n` argument for
#'   pattern-matching on the vector length.
#'
#' * Like [is_atomic()] and unlike base R `is.atomic()`,
#'   `is_bare_atomic()` does not return `TRUE` for `NULL`.
#'
#' * Unlike base R `is.numeric()`, `is_bare_double()` only returns
#'   `TRUE` for floating point numbers.
#' @inheritParams type-predicates
#' @seealso [type-predicates], [scalar-type-predicates]
#' @name bare-type-predicates
NULL

#' @export
#' @rdname bare-type-predicates
is_bare_list <- function(x, n = NULL) {
  !is.object(x) && is_list(x, n)
}
#' @export
#' @rdname bare-type-predicates
is_bare_atomic <- function(x, n = NULL) {
  !is.object(x) && is_atomic(x, n)
}
#' @export
#' @rdname bare-type-predicates
is_bare_vector <- function(x, n = NULL) {
  is_bare_atomic(x) || is_bare_list(x, n)
}
#' @export
#' @rdname bare-type-predicates
is_bare_double <- function(x, n = NULL) {
  !is.object(x) && is_double(x, n)
}
#' @export
#' @rdname bare-type-predicates
is_bare_complex <- function(x, n = NULL) {
  !is.object(x) && is_complex(x, n)
}
#' @export
#' @rdname bare-type-predicates
is_bare_integer <- function(x, n = NULL) {
  !is.object(x) && is_integer(x, n)
}
#' @export
#' @rdname bare-type-predicates
is_bare_numeric <- function(x, n = NULL) {
  if (!is_null(n) && length(x) != n) return(FALSE)
  !is.object(x) && typeof(x) %in% c("double", "integer")
}
#' @export
#' @rdname bare-type-predicates
is_bare_character <- function(x, n = NULL) {
  !is.object(x) && is_character(x, n)
}
#' @export
#' @rdname bare-type-predicates
is_bare_logical <- function(x, n = NULL) {
  !is.object(x) && is_logical(x, n)
}
#' @export
#' @rdname bare-type-predicates
is_bare_raw <- function(x, n = NULL) {
  !is.object(x) && is_raw(x, n)
}
#' @export
#' @rdname bare-type-predicates
is_bare_string <- function(x, n = NULL) {
  !is.object(x) && is_string(x, n)
}
#' @export
#' @rdname bare-type-predicates
is_bare_bytes <- is_bare_raw


#' Is object an empty vector or NULL?
#'
#' @param x object to test
#' @export
#' @examples
#' is_empty(NULL)
#' is_empty(list())
#' is_empty(list(NULL))
is_empty <- function(x) length(x) == 0

#' Is object an environment?
#'
#' `is_bare_environment()` tests whether `x` is an environment without a s3 or
#' s4 class.
#'
#' @inheritParams is_empty
#' @export
is_environment <- function(x) {
  typeof(x) == "environment"
}
#' @rdname is_environment
#' @export
is_bare_environment <- function(x) {
  !is.object(x) && typeof(x) == "environment"
}

#' Is object identical to TRUE or FALSE?
#'
#' These functions bypass R's automatic conversion rules and check
#' that `x` is literally `TRUE` or `FALSE`.
#' @inheritParams is_empty
#' @export
#' @examples
#' is_true(TRUE)
#' is_true(1)
#'
#' is_false(FALSE)
#' is_false(0)
is_true <- function(x) {
  identical(x, TRUE)
}
#' @rdname is_true
#' @export
is_false <- function(x) {
  identical(x, FALSE)
}

#' Is a vector integer-like?
#'
#' @description
#'
#' These predicates check whether R considers a number vector to be
#' integer-like, according to its own tolerance check (which is in
#' fact delegated to the C library). This function is not adapted to
#' data analysis, see the help for [base::is.integer()] for examples
#' of how to check for whole numbers.
#'
#' Things to consider when checking for integer-like doubles:
#'
#' * This check can be expensive because the whole double vector has
#'   to be traversed and checked.
#'
#' * Large double values may be integerish but may still not be
#'   coercible to integer. This is because integers in R only support
#'   values up to `2^31 - 1` while numbers stored as double can be
#'   much larger.
#'
#' @seealso [is_bare_numeric()] for testing whether an object is a
#'   base numeric type (a bare double or integer vector).
#' @inheritParams type-predicates
#' @export
#' @examples
#' is_integerish(10L)
#' is_integerish(10.0)
#' is_integerish(10.0, n = 2)
#' is_integerish(10.000001)
#' is_integerish(TRUE)
is_integerish <- function(x, n = NULL, finite = NULL) {
  .Call(ffi_is_integerish, x, n, finite)
}
#' @rdname is_integerish
#' @export
is_bare_integerish <- function(x, n = NULL, finite = NULL) {
  !is.object(x) && is_integerish(x, n, finite)
}
#' @rdname is_integerish
#' @export
is_scalar_integerish <- function(x, finite = NULL) {
  .Call(ffi_is_integerish, x, 1L, finite)
}

type_of_ <- function(x) {
  type <- typeof(x)
  if (is_formula(x)) {
    if (identical(node_car(x), colon_equals_sym)) {
      "definition"
    } else {
      "formula"
    }
  } else if (type == "character") {
    if (length(x) == 1) "string" else "character"
  } else if (type %in% c("builtin", "special")) {
    "primitive"
  } else {
    type
  }
}

#' Is an object copyable?
#'
#' When an object is modified, R generally copies it (sometimes
#' lazily) to enforce [value
#' semantics](https://en.wikipedia.org/wiki/Value_semantics).
#' However, some internal types are uncopyable. If you try to copy
#' them, either with `<-` or by argument passing, you actually create
#' references to the original object rather than actual
#' copies. Modifying these references can thus have far reaching side
#' effects.
#'
#' @param x An object to test.
#' @keywords internal
#' @export
#' @examples
#' # Let's add attributes with structure() to uncopyable types. Since
#' # they are not copied, the attributes are changed in place:
#' env <- env()
#' structure(env, foo = "bar")
#' env
#'
#' # These objects that can only be changed with side effect are not
#' # copyable:
#' is_copyable(env)
#'
#' structure(base::list, foo = "bar")
#' str(base::list)
is_copyable <- function(x) {
  switch(typeof(x),
    NULL = ,
    char = ,
    symbol = ,
    special = ,
    builtin = ,
    environment = ,
    externalptr =
      FALSE,
    TRUE
  )
}

is_equal <- function(x, y) {
  identical(x, y)
}

#' Is an object referencing another?
#'
#' @description
#'
#' There are typically two situations where two symbols may refer to
#' the same object.
#'
#' * R objects usually have copy-on-write semantics. This is an
#'   optimisation that ensures that objects are only copied if
#'   needed. When you copy a vector, no memory is actually copied
#'   until you modify either the original object or the copy is
#'   modified.
#'
#'   Note that the copy-on-write optimisation is an implementation
#'   detail that is not guaranteed by the specification of the R
#'   language.
#'
#' * Assigning an [uncopyable][is_copyable] object (like an
#'   environment) creates a reference. These objects are never copied
#'   even if you modify one of the references.
#'
#' @param x,y R objects.
#' @keywords internal
#' @export
#' @examples
#' # Reassigning an uncopyable object such as an environment creates a
#' # reference:
#' env <- env()
#' ref <- env
#' is_reference(ref, env)
#'
#' # Due to copy-on-write optimisation, a copied vector can
#' # temporarily reference the original vector:
#' vec <- 1:10
#' copy <- vec
#' is_reference(copy, vec)
#'
#' # Once you modify on of them, the copy is triggered in the
#' # background and the objects cease to reference each other:
#' vec[[1]] <- 100
#' is_reference(copy, vec)
is_reference <- function(x, y) {
  .Call(ffi_is_reference, x, y)
}


# Use different generic name to avoid import warnings when loading
# packages that import all of rlang after it has been load_all'd
rlang_type_sum <- function(x) {
  if (is_installed("pillar")) {
    pillar::type_sum(x)
  } else {
    UseMethod("rlang_type_sum")
  }
}

#' @export
rlang_type_sum.ordered <- function(x) "ord"
#' @export
rlang_type_sum.factor <- function(x) "fct"
#' @export
rlang_type_sum.POSIXct <- function(x) "dttm"
#' @export
rlang_type_sum.difftime <- function(x) "time"
#' @export
rlang_type_sum.Date <- function(x) "date"
#' @export
rlang_type_sum.data.frame <- function(x) class(x)[[1]]

#' @export
rlang_type_sum.default <- function(x) {
  if (!is.object(x)) {
    switch(typeof(x),
      logical = "lgl",
      integer = "int",
      double = "dbl",
      character = "chr",
      complex = "cpl",
      builtin = ,
      special = ,
      closure = "fn",
      environment = "env",
      symbol =
        if (is_missing(x)) {
          "missing"
        } else {
          "sym"
        },
      typeof(x)
    )
  } else if (!isS4(x)) {
    paste0("S3: ", class(x)[[1]])
  } else {
    paste0("S4: ", methods::is(x)[[1]])
  }
}
