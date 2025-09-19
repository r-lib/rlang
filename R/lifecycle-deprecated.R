# rlang 0.2.0: 2018-02
# rlang 0.3.0: 2018-10
# rlang 0.4.0: 2019-06
# rlang 0.4.2: 2019-11
# rlang 1.0.0: 2022-01
# rlang 1.1.0: 2023-02

#  Deprecated in rlang 1.1.0

# rlang 1.1.0: silent deprecation.

#' Create a child environment
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' [env()] now supports creating child environments, please use it
#' instead.
#'
#' @keywords internal
#' @export
child_env <- function(.parent, ...) {
  env <- new.env(parent = as_environment(.parent))
  env_bind0(env, list2(...))
  env
}

# rlang 1.1.0: soft-deprecation

#' Flatten or squash a list of lists into a simpler vector
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated in favour of `purrr::list_c()` and
#' `purrr::list_flatten()`.
#'
#' `flatten()` removes one level hierarchy from a list, while
#' `squash()` removes all levels. These functions are similar to
#' [unlist()] but they are type-stable so you always know what the
#' type of the output is.
#'
#' @param x A list to flatten or squash. The contents of the list can
#'   be anything for unsuffixed functions `flatten()` and `squash()`
#'   (as a list is returned), but the contents must match the type for
#'   the other functions.
#' @return `flatten()` returns a list, `flatten_lgl()` a logical
#'   vector, `flatten_int()` an integer vector, `flatten_dbl()` a
#'   double vector, and `flatten_chr()` a character vector. Similarly
#'   for `squash()` and the typed variants (`squash_lgl()` etc).
#' @export
#' @keywords internal
#' @examples
#' x <- replicate(2, sample(4), simplify = FALSE)
#' x
#'
#' flatten(x)
#' flatten_int(x)
#'
#' # With flatten(), only one level gets removed at a time:
#' deep <- list(1, list(2, list(3)))
#' flatten(deep)
#' flatten(flatten(deep))
#'
#' # But squash() removes all levels:
#' squash(deep)
#' squash_dbl(deep)
#'
#' # The typed flatten functions remove one level and coerce to an atomic
#' # vector at the same time:
#' flatten_dbl(list(1, list(2)))
#'
#' # Only bare lists are flattened, but you can splice S3 lists
#' # explicitly:
#' foo <- set_attrs(list("bar"), class = "foo")
#' str(flatten(list(1, foo, list(100))))
#' str(flatten(list(1, splice(foo), list(100))))
#'
#' # Instead of splicing manually, flatten_if() and squash_if() let
#' # you specify a predicate function:
#' is_foo <- function(x) inherits(x, "foo") || is_bare_list(x)
#' str(flatten_if(list(1, foo, list(100)), is_foo))
#'
#' # squash_if() does the same with deep lists:
#' deep_foo <- list(1, list(foo, list(foo, 100)))
#' str(deep_foo)
#'
#' str(squash(deep_foo))
#' str(squash_if(deep_foo, is_foo))
flatten <- function(x) {
  deprecate_soft(c(
    "`flatten()` is deprecated as of rlang 1.1.0.",
    "i" = "Please use `purrr::list_flatten()` or `purrr::list_c()`."
  ))
  .Call(ffi_squash, x, "list", is_spliced_bare, 1L)
}
# rlang 1.1.0: Soft deprecation.

#' @rdname flatten
#' @export
flatten_lgl <- function(x) {
  deprecate_soft(c(
    "`flatten_lgl()` is deprecated as of rlang 1.1.0.",
    "i" = "Please use `purrr::list_flatten()` and/or `purrr::list_c()`."
  ))
  .Call(ffi_squash, x, "logical", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_int <- function(x) {
  deprecate_soft(c(
    "`flatten_int()` is deprecated as of rlang 1.1.0.",
    "i" = "Please use `purrr::list_flatten()` and/or `purrr::list_c()`."
  ))
  .Call(ffi_squash, x, "integer", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_dbl <- function(x) {
  deprecate_soft(c(
    "`flatten_dbl()` is deprecated as of rlang 1.1.0.",
    "i" = "Please use `purrr::list_flatten()` and/or `purrr::list_c()`."
  ))
  .Call(ffi_squash, x, "double", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_cpl <- function(x) {
  deprecate_soft(c(
    "`flatten_cpl()` is deprecated as of rlang 1.1.0.",
    "i" = "Please use `purrr::list_flatten()` and/or `purrr::list_c()`."
  ))
  .Call(ffi_squash, x, "complex", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_chr <- function(x) {
  deprecate_soft(c(
    "`flatten_chr()` is deprecated as of rlang 1.1.0.",
    "i" = "Please use `purrr::list_flatten()` and/or `purrr::list_c()`."
  ))
  .Call(ffi_squash, x, "character", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_raw <- function(x) {
  deprecate_soft(c(
    "`flatten_raw()` is deprecated as of rlang 1.1.0.",
    "i" = "Please use `purrr::list_flatten()` and/or `purrr::list_c()`."
  ))
  .Call(ffi_squash, x, "raw", is_spliced_bare, 1L)
}

#' @rdname flatten
#' @export
squash <- function(x) {
  deprecate_soft("`squash()` is deprecated as of rlang 1.1.0.")
  .Call(ffi_squash, x, "list", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_lgl <- function(x) {
  deprecate_soft("`squash_lgl()` is deprecated as of rlang 1.1.0.")
  .Call(ffi_squash, x, "logical", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_int <- function(x) {
  deprecate_soft("`squash_int()` is deprecated as of rlang 1.1.0.")
  .Call(ffi_squash, x, "integer", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_dbl <- function(x) {
  deprecate_soft("`squash_dbl()` is deprecated as of rlang 1.1.0.")
  .Call(ffi_squash, x, "double", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_cpl <- function(x) {
  deprecate_soft("`squash_cpl()` is deprecated as of rlang 1.1.0.")
  .Call(ffi_squash, x, "complex", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_chr <- function(x) {
  deprecate_soft("`squash_chr()` is deprecated as of rlang 1.1.0.")
  .Call(ffi_squash, x, "character", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_raw <- function(x) {
  deprecate_soft("`squash_raw()` is deprecated as of rlang 1.1.0.")
  .Call(ffi_squash, x, "raw", is_spliced_bare, -1L)
}

#' @rdname flatten
#' @param predicate A function of one argument returning whether it
#'   should be spliced.
#' @export
flatten_if <- function(x, predicate = is_spliced) {
  deprecate_soft("`flatten_if()` is deprecated as of rlang 1.1.0.")
  .Call(ffi_squash, x, "list", predicate, 1L)
}
#' @rdname flatten
#' @export
squash_if <- function(x, predicate = is_spliced) {
  deprecate_soft("`squash_if()` is deprecated as of rlang 1.1.0.")
  .Call(ffi_squash, x, "list", predicate, -1L)
}
#' Splice lists
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `dots_splice()` is like [dots_list()] but automatically splices
#' list inputs.
#'
#' @inheritParams dots_list
#' @keywords internal
#' @export
dots_splice <- function(
  ...,
  .ignore_empty = c("trailing", "none", "all"),
  .preserve_empty = FALSE,
  .homonyms = c("keep", "first", "last", "error"),
  .check_assign = FALSE
) {
  deprecate_soft("`dots_splice()` is deprecated as of rlang 1.1.0.")
  dots <- .Call(
    ffi_dots_flat_list,
    frame_env = environment(),
    named = NULL,
    ignore_empty = .ignore_empty,
    preserve_empty = .preserve_empty,
    unquote_names = TRUE,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
  names(dots) <- names2(dots)
  dots
}

#  Deprecated in rlang 1.0.0

# Silently deprecated for now
# - `with_env()` is used in dplyr (unit test) and purrr.
# - `locally()` is used in carrier

#' Evaluate an expression within a given environment
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions evaluate `expr` within a given environment (`env`
#' for `with_env()`, or the child of the current environment for
#' `locally`). They rely on [eval_bare()] which features a lighter
#' evaluation mechanism than base R [base::eval()], and which also has
#' some subtle implications when evaluting stack sensitive functions
#' (see help for [eval_bare()]).
#'
#' `locally()` is equivalent to the base function
#' [base::local()] but it produces a much cleaner
#' evaluation stack, and has stack-consistent semantics. It is thus
#' more suited for experimenting with the R language.
#'
#' @inheritParams eval_bare
#' @param env An environment within which to evaluate `expr`. Can be
#'   an object with a [get_env()] method.
#' @keywords internal
#' @export
#' @examples
#' # with_env() is handy to create formulas with a given environment:
#' env <- child_env("rlang")
#' f <- with_env(env, ~new_formula())
#' identical(f_env(f), env)
#'
#' # Or functions with a given enclosure:
#' fn <- with_env(env, function() NULL)
#' identical(get_env(fn), env)
#'
#'
#' # Unlike eval() it doesn't create duplicates on the evaluation
#' # stack. You can thus use it e.g. to create non-local returns:
#' fn <- function() {
#'   g(current_env())
#'   "normal return"
#' }
#' g <- function(env) {
#'   with_env(env, return("early return"))
#' }
#' fn()
#'
#'
#' # Since env is passed to as_environment(), it can be any object with an
#' # as_environment() method. For strings, the pkg_env() is returned:
#' with_env("base", ~mtcars)
#'
#' # This can be handy to put dictionaries in scope:
#' with_env(mtcars, cyl)
with_env <- function(env, expr) {
  .External2(ffi_eval, substitute(expr), as_environment(env, caller_env()))
}
#' @rdname with_env
#' @export
locally <- function(expr) {
  .External2(ffi_eval, substitute(expr), child_env(caller_env()))
}


#  Soft-deprecated in rlang 0.4.0

##  Types

#' Base type of an object
#'
#' @description
#'
#' `r lifecycle::badge("soft-deprecated")`
#' `r lifecycle::badge("experimental")`
#'
#' This is equivalent to [base::typeof()] with a few differences that
#' make dispatching easier:
#' * The type of one-sided formulas is "quote".
#' * The type of character vectors of length 1 is "string".
#' * The type of special and builtin functions is "primitive".
#'
#' @param x An R object.
#' @export
#' @keywords internal
#' @examples
#' type_of(10L)
#'
#' # Quosures are treated as a new base type but not formulas:
#' type_of(quo(10L))
#' type_of(~10L)
#'
#' # Compare to base::typeof():
#' typeof(quo(10L))
#'
#' # Strings are treated as a new base type:
#' type_of(letters)
#' type_of(letters[[1]])
#'
#' # This is a bit inconsistent with the core language tenet that data
#' # types are vectors. However, treating strings as a different
#' # scalar type is quite helpful for switching on function inputs
#' # since so many arguments expect strings:
#' switch_type("foo", character = abort("vector!"), string = "result")
#'
#' # Special and builtin primitives are both treated as primitives.
#' # That's because it is often irrelevant which type of primitive an
#' # input is:
#' typeof(list)
#' typeof(`$`)
#' type_of(list)
#' type_of(`$`)
type_of <- function(x) {
  deprecate_warn(c(
    "`type_of()` is deprecated as of rlang 0.4.0.",
    "Please use `typeof()` or your own version instead."
  ))
  type_of_(x)
}

#' Dispatch on base types
#'
#' @description
#'
#' `r lifecycle::badge("soft-deprecated")`
#' `r lifecycle::badge("experimental")`
#'
#' `switch_type()` is equivalent to
#' \code{\link[base]{switch}(\link{type_of}(x, ...))}, while
#' `switch_class()` switchpatches based on `class(x)`. The `coerce_`
#' versions are intended for type conversion and provide a standard
#' error message when conversion fails.
#'
#'
#' @param .x An object from which to dispatch.
#' @param ... Named clauses. The names should be types as returned by
#'   [type_of()].
#' @param .to This is useful when you switchpatch within a coercing
#'   function. If supplied, this should be a string indicating the
#'   target type. A catch-all clause is then added to signal an error
#'   stating the conversion failure. This type is prettified unless
#'   `.to` inherits from the S3 class `"AsIs"` (see [base::I()]).
#' @export
#' @keywords internal
#' @examples
#' switch_type(3L,
#'   double = "foo",
#'   integer = "bar",
#'   "default"
#' )
#'
#' # Use the coerce_ version to get standardised error handling when no
#' # type matches:
#' to_chr <- function(x) {
#'   coerce_type(x, "a chr",
#'     integer = as.character(x),
#'     double = as.character(x)
#'   )
#' }
#' to_chr(3L)
#'
#' # Strings have their own type:
#' switch_type("str",
#'   character = "foo",
#'   string = "bar",
#'   "default"
#' )
#'
#' # Use a fallthrough clause if you need to dispatch on all character
#' # vectors, including strings:
#' switch_type("str",
#'   string = ,
#'   character = "foo",
#'   "default"
#' )
#'
#' # special and builtin functions are treated as primitive, since
#' # there is usually no reason to treat them differently:
#' switch_type(base::list,
#'   primitive = "foo",
#'   "default"
#' )
#' switch_type(base::`$`,
#'   primitive = "foo",
#'   "default"
#' )
#'
#' # closures are not primitives:
#' switch_type(rlang::switch_type,
#'   primitive = "foo",
#'   "default"
#' )
switch_type <- function(.x, ...) {
  deprecate_warn(c(
    "`switch_type()` is soft-deprecated as of rlang 0.4.0.",
    "Please use `switch(typeof())` or `switch(my_typeof())` instead."
  ))
  switch(type_of_(.x), ...)
}
#' @rdname switch_type
#' @export
coerce_type <- function(.x, .to, ...) {
  deprecate_warn("`coerce_type()` is soft-deprecated as of rlang 0.4.0.")
  switch(type_of_(.x), ..., abort_coercion(.x, .to))
}
#' @rdname switch_type
#' @export
switch_class <- function(.x, ...) {
  deprecate_warn("`switch_class()` is soft-deprecated as of rlang 0.4.0.")
  switch(class(.x), ...)
}
#' @rdname switch_type
#' @export
coerce_class <- function(.x, .to, ...) {
  deprecate_warn("`coerce_class()` is soft-deprecated as of rlang 0.4.0.")
  switch(class(.x), ..., abort_coercion(.x, .to))
}
#' Format a type for error messages
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `friendly_type()` is deprecated. Please use the
#' `standalone-obj-type.R` file instead. You can import it
#' in your package with `usethis::use_standalone("r-lib/rlang", "obj-type")`.
#'
#' @param type A type as returned by [typeof()].
#' @return A string of the prettified type, qualified with an
#'   indefinite article.
#' @export
#' @keywords internal
friendly_type <- function(type) {
  deprecate_warn("`friendly_type()` is deprecated as of rlang 0.4.11.")
  type
}
# rlang 0.4.0: Soft-deprecation
# rlang 1.1.0: Deprecation

##  Eval

#' Invoke a function with a list of arguments
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated in rlang 0.4.0 in favour of [exec()].
#'
#' @param .fn,args,...,.env,.bury `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
invoke <- function(
  .fn,
  .args = list(),
  ...,
  .env = caller_env(),
  .bury = c(".fn", "")
) {
  # rlang 0.4.0: Soft-deprecation
  # rlang 1.0.0: Deprecation
  deprecate_warn(c(
    "`invoke()` is deprecated as of rlang 0.4.0.",
    "Please use `exec()` or `inject()` instead."
  ))

  args <- c(.args, list(...))

  if (is_null(.bury) || !length(args)) {
    if (is_scalar_character(.fn)) {
      .fn <- env_get(.env, .fn, inherit = TRUE)
    }
    call <- call2(.fn, !!!args)
    return(.External2(ffi_eval, call, .env))
  }

  if (!is_character(.bury, 2L)) {
    abort("`.bury` must be a character vector of length 2")
  }
  arg_prefix <- .bury[[2]]
  fn_nm <- .bury[[1]]

  buried_nms <- paste0(arg_prefix, seq_along(args))
  buried_args <- set_names(args, buried_nms)
  .env <- env_bury(.env, !!!buried_args)
  args <- set_names(buried_nms, names(args))
  args <- syms(args)

  if (is_function(.fn)) {
    env_bind(.env, !!fn_nm := .fn)
    .fn <- fn_nm
  }

  call <- call2(.fn, !!!args)
  .External2(ffi_eval, call, .env)
}


##  Casting

#' Coerce an object to a base type
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
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
#' @section Lifecycle:
#'
#' These functions are deprecated in favour of `vctrs::vec_cast()`.
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
#' is only declarative, no encoding conversion is attempted.
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
#' as.list.foobar <- function(x) abort("don't call me")
#' as_list(x)
#' @name vector-coercion
NULL
signal_deprecated_cast <- function(fn, user_env = caller_env(2)) {
  deprecate_warn(
    user_env = user_env,
    c(
      sprintf("`%s()` is deprecated as of rlang 0.4.0", fn),
      "Please use `vctrs::vec_cast()` instead."
    )
  )
}
#' @rdname vector-coercion
#' @export
as_logical <- function(x) {
  signal_deprecated_cast("as_logical")
  legacy_as_logical(x)
}
#' @rdname vector-coercion
#' @export
as_integer <- function(x) {
  signal_deprecated_cast("as_integer")
  legacy_as_integer(x)
}
#' @rdname vector-coercion
#' @export
as_double <- function(x) {
  signal_deprecated_cast("as_double")
  legacy_as_double(x)
}
#' @rdname vector-coercion
#' @export
as_complex <- function(x) {
  signal_deprecated_cast("as_complex")
  legacy_as_complex(x)
}
#' @rdname vector-coercion
#' @export
as_character <- function(x, encoding = NULL) {
  signal_deprecated_cast("as_character")
  legacy_as_character(x, encoding = encoding)
}
#' @rdname vector-coercion
#' @export
as_list <- function(x) {
  signal_deprecated_cast("as_list")
  switch_type(x, environment = env_as_list(x), vec_as_list(x))
}
env_as_list <- function(x) {
  names_x <- names(x)
  x <- as_base_type(x, as.list)
  set_names(x, .Call(ffi_unescape_character, names_x))
}
vec_as_list <- function(x) {
  coerce_type_vec(
    x,
    vec_type_friendly(list()),
    logical = ,
    integer = ,
    double = ,
    string = ,
    character = ,
    complex = ,
    raw = as_base_type(x, as.list),
    list = {
      attributes(x) <- NULL
      x
    }
  )
}

legacy_as_logical <- function(x) {
  coerce_type_vec(
    x,
    vec_type_friendly(lgl()),
    logical = {
      attributes(x) <- NULL
      x
    },
    integer = as_base_type(x, as.logical),
    double = as_integerish_type(x, as.logical, lgl())
  )
}
legacy_as_integer <- function(x) {
  coerce_type_vec(
    x,
    vec_type_friendly(int()),
    logical = as_base_type(x, as.integer),
    integer = {
      attributes(x) <- NULL
      x
    },
    double = as_integerish_type(x, as.integer, int(), value = FALSE)
  )
}
legacy_as_double <- function(x) {
  coerce_type_vec(
    x,
    vec_type_friendly(dbl()),
    logical = ,
    integer = as_base_type(x, as.double),
    double = {
      attributes(x) <- NULL
      x
    }
  )
}
legacy_as_complex <- function(x) {
  coerce_type_vec(
    x,
    vec_type_friendly(cpl()),
    logical = ,
    integer = ,
    double = as_base_type(x, as.complex),
    complex = {
      attributes(x) <- NULL
      x
    }
  )
}
legacy_as_character <- function(x, encoding = NULL) {
  if (is_unspecified(x)) {
    return(rep_along(x, na_chr))
  }
  coerce_type_vec(
    x,
    vec_type_friendly(chr()),
    string = ,
    character = {
      attributes(x) <- NULL
      if (!is_null(encoding)) {
        Encoding(x) <- encoding
      }
      x
    }
  )
}

is_unspecified <- function(x) {
  is_logical(x) && all(map_lgl(x, identical, NA))
}

as_base_type <- function(x, as_type) {
  # Zap attributes temporarily instead of unclassing. We want to avoid
  # method dispatch, but we also want to avoid an extra copy of atomic
  # vectors: the first when unclassing, the second when coercing. This
  # is also useful for uncopyable types like environments.
  attrs <- .Call(ffi_attrib, x)
  .Call(ffi_poke_attrib, x, NULL)

  # This function assumes that the target type is different than the
  # input type, otherwise no duplication is done and the output will
  # be modified by side effect when we restore the input attributes.
  on.exit(.Call(ffi_poke_attrib, x, attrs))

  as_type(x)
}
as_integerish_type <- function(x, as_type, to, value = FALSE) {
  if (is_integerish(x)) {
    as_base_type(x, as_type)
  } else {
    abort(paste0(
      "Can't convert a fractional double vector to ",
      obj_type_friendly(to, value = value),
      ""
    ))
  }
}

coerce_type_vec <- function(.x, .to, ...) {
  # Cannot reuse coerce_type() because switch() has a bug with
  # fallthrough and multiple levels of dots forwarding.
  out <- switch(type_of_(.x), ..., abort_coercion(.x, .to, call = NULL))

  if (!is_null(names(.x))) {
    # Avoid a copy of `out` when we restore the names, since it could be
    # a heavy atomic vector. We own `out`, so it is ok to change its
    # attributes inplace.
    .Call(ffi_poke_attrib, out, pairlist(names = names(.x)))
  }

  out
}
vec_coerce <- function(x, type) {
  .Call(ffi_vec_coerce, x, type)
}


#  Stack and frames  -------------------------------------------------

# 2022-01: https://github.com/tidyverse/purrr/issues/851

#' Call stack information
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated as of rlang 0.3.0.
#' @param n The number of frames to go back in the stack.
#' @name stack-deprecated
#' @keywords internal
NULL

#' @rdname stack-deprecated
#' @export
ctxt_frame <- function(n = 1) {
  deprecate_warn("`ctxt_frame()` is deprecated as of rlang 0.3.0.")
  stopifnot(n > 0)
  pos <- sys.nframe() - n

  if (pos < 0L) {
    stop("not that many frames on the stack", call. = FALSE)
  } else if (pos == 0L) {
    global_frame()
  } else {
    new_frame(list(
      pos = pos,
      caller_pos = sys.parent(n + 1),
      expr = sys.call(-n),
      env = sys.frame(-n),
      fn = sys.function(-n),
      fn_name = call_name(sys.call(-n))
    ))
  }
}

# 2022-01: Used in `ctxt_frame()`

#' @rdname stack-deprecated
#' @export
global_frame <- function() {
  deprecate_warn("`global_frame()` is deprecated as of rlang 0.3.0.")
  new_frame(list(
    pos = 0L,
    caller_pos = NA_integer_,
    expr = NULL,
    env = globalenv(),
    fn = NULL,
    fn_name = NULL
  ))
}
new_frame <- function(x) {
  structure(x, class = "frame")
}


#  Tidy eval  --------------------------------------------------------

#' Squash a quosure
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated, please use [quo_squash()] instead.
#'
#' @inheritParams quo_squash
#' @keywords internal
#' @export
quo_expr <- function(quo, warn = FALSE) {
  # 2022-01: Still used by many packages on CRAN
  deprecate_warn(paste_line(
    "`quo_expr()` is deprecated as of rlang 0.2.0.",
    "Please use `quo_squash()` instead."
  ))
  quo_squash(quo, warn = warn)
}

#' Process unquote operators in a captured expression
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `expr_interp()` is deprecated, please use [inject()] instead.
#'
#' @param x,env `r lifecycle::badge("deprecated")`
#' @keywords internal
#' @export
expr_interp <- function(x, env = NULL) {
  if (is_formula(x)) {
    f_rhs(x) <- .Call(ffi_interp, f_rhs(x), env %||% f_env(x))
  } else if (is_closure(x)) {
    body(x) <- .Call(ffi_interp, body(x), env %||% fn_env(x))
  } else {
    x <- .Call(ffi_interp, x, env %||% parent.frame())
  }
  x
}

#' Deprecated `UQ()` and `UQS()` operators
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' These operators are deprecated in favour of
#' [`!!`][injection-operator] and [`!!!`][splice-operator].
#'
#' @keywords internal
#' @export
UQ <- function(x) {
  abort("`UQ()` can only be used within a defused argument")
}
#' @rdname UQ
#' @export
UQS <- function(x) {
  abort("`UQS()` can only be used within a defused argument")
}


#  Expressions  ------------------------------------------------------

#' Create a call
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' These functions are deprecated, please use [call2()] and
#' [new_call()] instead.
#'
#' @inheritParams call2
#' @keywords internal
#' @export
lang <- function(.fn, ..., .ns = NULL) {
  # 2022-01: Still used in attempt
  # https://github.com/ColinFay/attempt/issues/16
  deprecate_warn(paste_line(
    "`lang()` is deprecated as of rlang 0.2.0.",
    "Please use `call2()` instead."
  ))
  call2(.fn, ..., .ns = .ns)
}
#' Is object a call?
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' These functions are deprecated, please use [is_call()] and its `n`
#' argument instead.
#' @inheritParams is_call
#' @keywords internal
#' @export
is_lang <- function(x, name = NULL, n = NULL, ns = NULL) {
  # 2022-01: Still used in foolbox
  # https://github.com/mailund/foolbox/issues/50
  deprecate_warn(paste_line(
    "`is_lang()` is deprecated as of rlang 0.2.0.",
    "Please use `is_call()` instead."
  ))
  is_call(x, name, n, ns)
}

#' Standardise a call
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated in rlang 0.4.11 in favour of [call_match()].
#' `call_standardise()` was designed for call wrappers that include an
#' environment like formulas or quosures. The function definition was
#' plucked from that environment. However in practice it is rare to
#' use it with wrapped calls, and then it's easy to forget to supply
#' the environment. For these reasons, we have designed [call_match()]
#' as a simpler wrapper around [match.call()].
#'
#' This is essentially equivalent to [base::match.call()], but with
#' experimental handling of primitive functions.
#'
#' @inheritParams call_fn
#' @inheritParams call_match
#'
#' @return A quosure if `call` is a quosure, a raw call otherwise.
#' @keywords internal
#' @export
call_standardise <- function(call, env = caller_env()) {
  deprecate_soft("`call_standardise()` is deprecated as of rlang 0.4.11")

  expr <- get_expr(call)
  if (!is_call(expr)) {
    abort_call_input_type("call")
  }

  # The call name might be a literal, not necessarily a symbol
  env <- get_env(call, env)
  fn <- eval_bare(node_car(expr), env)

  if (is_primitive(fn)) {
    call
  } else {
    matched <- match.call(fn, expr)
    set_expr(call, matched)
  }
}
#' Extract function from a call
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated in rlang 0.4.11.
#'
#' @param call,env `r lifecycle::badge("deprecated")`
#' @keywords internal
#' @export
call_fn <- function(call, env = caller_env()) {
  deprecate_soft("`call_fn()` is deprecated as of rlang 0.4.11")
  expr <- get_expr(call)
  env <- get_env(call, env)

  if (!is_call(expr)) {
    abort_call_input_type("call")
  }

  switch(
    call_type(expr),
    recursive = abort("`call` does not call a named or inlined function"),
    inlined = node_car(expr),
    named = ,
    namespaced = ,
    eval_bare(node_car(expr), env)
  )
}
# rlang 0.4.11: silent deprecation
# rlang 1.1.0: soft-deprecation

#  Environments  -----------------------------------------------------

# 2022-01: https://github.com/r-lib/conflicted/issues/65

#' Deprecated `scoped` functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated as of rlang 0.3.0. Please use
#' [is_attached()] instead.
#'
#' @param nm The name of an environment attached to the search
#'   path. Call [base::search()] to see what is currently on the path.
#' @keywords internal
#' @export
scoped_env <- function(nm) {
  deprecate_warn(paste_line(
    "`scoped_env()` is deprecated as of rlang 0.3.0.",
    "Please use `search_env()` instead."
  ))
  local_options(lifecycle_disable_warnings = TRUE)

  if (identical(nm, "NULL")) {
    return(empty_env())
  }
  if (!is_scoped(nm)) {
    stop(paste0(nm, " is not in scope"), call. = FALSE)
  }
  as.environment(nm)
}

# 2022-01: https://github.com/tidyverse/purrr/issues/851

#' @rdname scoped_env
#' @export
is_scoped <- function(nm) {
  deprecate_warn(paste_line(
    "`is_scoped()` is deprecated as of rlang 0.3.0.",
    "Please use `is_attached()` instead."
  ))
  local_options(lifecycle_disable_warnings = TRUE)

  if (!is_scalar_character(nm)) {
    stop("`nm` must be a string", call. = FALSE)
  }
  nm %in% c(search(), "NULL")
}


#  Attributes  -------------------------------------------------------

#' Add attributes to an object
#'
#' `r lifecycle::badge("deprecated")`
#' @param .x,... `r lifecycle::badge("deprecated")`
#'
#' @keywords internal
#' @export
set_attrs <- function(.x, ...) {
  # 2018-10: Soft-deprecated
  # 2019-06: Deprecated
  # 2022-01: Used in `survivalAnalysis`
  deprecate_warn("`set_attrs()` is deprecated as of rlang 0.3.0")

  if (!is_copyable(.x)) {
    abort("`.x` is uncopyable.")
  }
  set_attrs_impl(.x, ...)
}
set_attrs_impl <- function(.x, ...) {
  attrs <- dots_list(...)

  # If passed a single unnamed NULL, zap attributes
  if (identical(attrs, set_attrs_null)) {
    attributes(.x) <- NULL
  } else {
    attributes(.x) <- c(attributes(.x), attrs)
  }

  .x
}
set_attrs_null <- list(NULL)
names(set_attrs_null) <- ""


#  Conditions --------------------------------------------------------

# 1.0.0: Silently deprecated. Used in recipes (in a deprecated function).
# 1.1.0: soft-deprecated

#' Establish handlers on the stack
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' As of rlang 1.0.0, `with_handlers()` is deprecated. Use the base
#' functions or the experimental [try_fetch()] function instead.
#'
#' @param .expr,...,handler `r lifecycle::badge("deprecated")`
#' @keywords internal
#' @export
with_handlers <- function(.expr, ...) {
  deprecate_soft(c(
    "`with_handlers()` is deprecated as of rlang 1.0.0.",
    "i" = "Please use `tryCatch()`, `withCallingHandlers()`, or `try_fetch()`."
  ))

  handlers <- list2(...)

  is_calling <- map_lgl(handlers, inherits, "rlang_box_calling_handler")
  handlers <- map_if(handlers, is_calling, unbox)
  handlers <- map(handlers, as_function)

  calling <- handlers[is_calling]
  exiting <- handlers[!is_calling]

  expr <- quote(.expr)
  if (length(calling)) {
    expr <- expr(withCallingHandlers(!!expr, !!!calling))
  }
  if (length(exiting)) {
    expr <- expr(tryCatch(!!expr, !!!exiting))
  }

  .External2(ffi_eval, expr, environment())
}
#' @rdname with_handlers
#' @export
calling <- function(handler) {
  deprecate_soft("`calling()` is deprecated as of rlang 1.0.0.")
  handler <- as_function(handler)
  new_box(handler, "rlang_box_calling_handler")
}
#' @rdname with_handlers
#' @export
exiting <- function(handler) {
  deprecate_soft(c(
    "`exiting()` is deprecated as of rlang 0.4.0.",
    "Handlers are now treated as exiting by default."
  ))
  handler <- as_function(handler)
  structure(
    handler,
    class = c("rlang_handler_exiting", "rlang_handler", "function")
  )
}


#  Scoped_

# rlang 0.4.2: Silent deprecation.
# rlang 1.0.0: Soft deprecation.

#' Deprecated `scoped_` functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated as of rlang 0.4.2. Use [local_interactive()],
#' [local_options()], or [local_bindings()] instead.
#'
#' @inheritParams local_interactive
#' @inheritParams local_options
#' @inheritParams local_bindings
#'
#' @keywords internal
#' @export
scoped_interactive <- function(value = TRUE, frame = caller_env()) {
  deprecate_soft(c(
    "`scoped_interactive()` is deprecated as of rlang 0.4.2.",
    "Please use `local_interactive()` instead."
  ))
  local_interactive(value = value, frame = frame)
}
#' @rdname scoped_interactive
#' @export
scoped_options <- function(..., .frame = caller_env()) {
  deprecate_soft(c(
    "`scoped_options()` is deprecated as of rlang 0.4.2.",
    "Please use `local_options()` instead."
  ))
  local_options(..., .frame = .frame)
}
#' @rdname scoped_interactive
#' @export
scoped_bindings <- function(..., .env = .frame, .frame = caller_env()) {
  deprecate_soft(c(
    "`scoped_bindings()` is deprecated as of rlang 0.4.2.",
    "Please use `local_bindings()` instead."
  ))
  local_bindings(..., .env = .env, .frame = .frame)
}
