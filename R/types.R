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
#' @param encoding Expected encoding of a string or character
#'   vector. One of `UTF-8`, `latin1`, or `unknown`.
#' @seealso [bare-type-predicates] [scalar-type-predicates]
#' @name type-predicates
NULL

#' @export
#' @rdname type-predicates
is_list <- function(x, n = NULL) {
  .Call(rlang_is_list, x, n)
}

parsable_atomic_types <- c("logical", "integer", "double", "complex", "character")
atomic_types <- c(parsable_atomic_types, "raw")
#' @export
#' @rdname type-predicates
is_atomic <- function(x, n = NULL) {
  .Call(rlang_is_atomic, x, n)
}
#' @export
#' @rdname type-predicates
is_vector <- function(x, n = NULL) {
  .Call(rlang_is_vector, x, n)
}

# Mostly for unit testing
is_finite <- function(x) {
  .Call(rlang_is_finite, x)
}

#' @export
#' @rdname type-predicates
is_integer <- function(x, n = NULL) {
  .Call(rlang_is_integer, x, n)
}
#' @export
#' @rdname type-predicates
is_double <- function(x, n = NULL, finite = NULL) {
  .Call(rlang_is_double, x, n, finite)
}
#' @export
#' @rdname type-predicates
is_character <- function(x, n = NULL, encoding = NULL) {
  if (!is_null(encoding)) {
    warn_deprecated("The `encoding` argument is deprecated as of rlang 0.3.0.")
  }
  if (!.Call(rlang_is_character, x, n)) {
    return(FALSE)
  }
  if (!is_null(encoding)) {
    stopifnot(typeof(encoding) == "character")
    if (!all(chr_encoding(x) %in% encoding)) {
      return(FALSE)
    }
  }
  TRUE
}
#' @export
#' @rdname type-predicates
is_logical <- function(x, n = NULL) {
  .Call(rlang_is_logical, x, n)
}
#' @export
#' @rdname type-predicates
is_raw <- function(x, n = NULL) {
  .Call(rlang_is_raw, x, n)
}
#' @export
#' @rdname type-predicates
is_bytes <- is_raw

#' @export
#' @rdname type-predicates
is_null <- function(x) {
  .Call(rlang_is_null, x)
}

#' Scalar type predicates
#'
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' @inheritParams type-predicates
#' @param x object to be tested.
#' @seealso [type-predicates], [bare-type-predicates]
#' @name scalar-type-predicates
NULL

#' @export
#' @rdname scalar-type-predicates
is_scalar_list <- function(x) {
  .Call(rlang_is_list, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_atomic <- function(x) {
  .Call(rlang_is_atomic, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_vector <- function(x) {
  .Call(rlang_is_vector, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_integer <- function(x) {
  .Call(rlang_is_integer, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_double <- function(x) {
  .Call(rlang_is_double, x, 1L, NULL)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_character <- function(x, encoding = NULL) {
  if (!is_null(encoding)) {
    warn_deprecated("The `encoding` argument is deprecated as of rlang 0.3.0.")
  }
  is_character(x, encoding = encoding, n = 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_logical <- function(x) {
  .Call(rlang_is_logical, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_raw <- function(x) {
  .Call(rlang_is_raw, x, 1L)
}
#' @export
#' @rdname scalar-type-predicates
is_string <- function(x, encoding = NULL) {
  if (!is_null(encoding)) {
    warn_deprecated("The `encoding` argument is deprecated as of rlang 0.3.0.")
  }
  is_character(x, encoding = encoding, n = 1L) && !is.na(x)
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_bytes <- is_scalar_raw

is_bool <- function(x) {
  is_logical(x, n = 1) && !is.na(x)
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
is_bare_character <- function(x, n = NULL, encoding = NULL) {
  if (!is_null(encoding)) {
    warn_deprecated("The `encoding` argument is deprecated as of rlang 0.3.0.")
  }
  !is.object(x) && is_character(x, n, encoding = encoding)
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
  .Call(rlang_is_integerish, x, n, finite)
}
#' @rdname is_integerish
#' @export
is_bare_integerish <- function(x, n = NULL, finite = NULL) {
  !is.object(x) && is_integerish(x, n, finite)
}
#' @rdname is_integerish
#' @export
is_scalar_integerish <- function(x, finite = NULL) {
  .Call(rlang_is_integerish, x, 1L, finite)
}

#' Base type of an object
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' This is equivalent to [base::typeof()] with a few differences that
#' make dispatching easier:
#' * The type of one-sided formulas is "quote".
#' * The type of character vectors of length 1 is "string".
#' * The type of special and builtin functions is "primitive".
#'
#'
#' @section Life cycle:
#'
#' `type_of()` is an experimental function. Expect API changes.
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
  type <- typeof(x)
  if (is_formulaish(x)) {
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

#' Dispatch on base types
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' `switch_type()` is equivalent to
#' \code{\link[base]{switch}(\link{type_of}(x, ...))}, while
#' `switch_class()` switchpatches based on `class(x)`. The `coerce_`
#' versions are intended for type conversion and provide a standard
#' error message when conversion fails.
#'
#'
#' @section Life cycle:
#'
#' * Like [type_of()], `switch_type()` and `coerce_type()` are
#'   experimental functions.
#'
#' * `switch_class()` and `coerce_class()` are experimental functions.
#'
#' @param .x An object from which to dispatch.
#' @param ... Named clauses. The names should be types as returned by
#'   [type_of()].
#' @param .to This is useful when you switchpatch within a coercing
#'   function. If supplied, this should be a string indicating the
#'   target type. A catch-all clause is then added to signal an error
#'   stating the conversion failure. This type is prettified unless
#'   `.to` inherits from the S3 class `"AsIs"` (see [base::I()]).
#' @seealso [switch_lang()]
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
  switch(type_of(.x), ...)
}
#' @rdname switch_type
#' @export
coerce_type <- function(.x, .to, ...) {
  switch(type_of(.x), ..., abort_coercion(.x, .to))
}
#' @rdname switch_type
#' @export
switch_class <- function(.x, ...) {
  switch(class(.x), ...)
}
#' @rdname switch_type
#' @export
coerce_class <- function(.x, .to, ...) {
  switch(class(.x), ..., abort_coercion(.x, .to))
}
abort_coercion <- function(x, to_type) {
  x_type <- friendly_type_of(x)
  if (!inherits(to_type, "AsIs")) {
    to_type <- as_friendly_type(to_type)
  }
  abort(paste0("Can't convert ", x_type, " to ", to_type))
}

#' Format a type for error messages
#'
#' @section Life cycle:
#'
#' * Like [type_of()], `friendly_type()` is experimental.
#'
#' @param type A type as returned by [type_of()] or [lang_type_of()].
#' @return A string of the prettified type, qualified with an
#'   indefinite article.
#' @export
#' @keywords internal
#' @examples
#' friendly_type("logical")
#' friendly_type("integer")
#' friendly_type("string")
#' @export
friendly_type <- function(type) {
  friendly <- as_friendly_type(type)
  if (!is_null(friendly)) {
    return(friendly)
  }

  friendly <- friendly_lang_type_of(type)
  if (!is_null(friendly)) {
    return(friendly)
  }

  friendly <- friendly_expr_type_of(type)
  if (!is_null(friendly)) {
    return(friendly)
  }

  type
}

friendly_type_of <- function(x, length = FALSE) {
  if (is.object(x)) {
    return(sprintf("a `%s` object", paste_classes(x)))
  }

  friendly <- as_friendly_type(typeof(x))

  if (length && is_vector(x)) {
    friendly <- paste0(friendly, sprintf(" of length %s", length(x)))
  }

  friendly
}
as_friendly_type <- function(type) {
  switch(type,
    logical = "a logical vector",
    integer = "an integer vector",
    numeric = ,
    double = "a double vector",
    complex = "a complex vector",
    character = "a character vector",
    raw = "a raw vector",
    string = "a string",
    list = "a list",

    NULL = "NULL",
    environment = "an environment",
    externalptr = "a pointer",
    weakref = "a weak reference",
    S4 = "an S4 object",

    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    expression = "an expression vector",
    quosure = "a quosure",
    formula = "a formula",

    char = "an internal string",
    promise = "an internal promise",
    ... = "an internal dots object",
    any = "an internal `any` object",
    bytecode = "an internal bytecode object",

    primitive = ,
    builtin = ,
    special = "a primitive function",
    closure = "a function",

    type
  )
}
paste_classes <- function(x) {
  paste(class(x), collapse = "/")
}

friendly_lang_type_of <- function(type) {
  switch(type,
    named = "a named call",
    namespaced = "a namespaced call",
    recursive = "a recursive call",
    inlined = "an inlined call"
  )
}

friendly_expr_type_of <- function(type) {
  switch(type,
    NULL = "NULL",
    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    literal = "a syntactic literal",
    missing = "the missing argument"
  )
}

#' Dispatch on call type
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' `switch_lang()` dispatches clauses based on the subtype of call, as
#' determined by `lang_type_of()`. The subtypes are based on the type
#' of call head (see details).
#'
#' @details
#'
#' Calls (objects of type `language`) do not necessarily call a named
#' function. They can also call an anonymous function or the result of
#' some other expression. The language subtypes are organised around
#' the kind of object being called:
#'
#' * For regular calls to named function, `switch_lang()` returns
#'   "named".
#'
#' * Sometimes the function being called is the result of another
#'   function call, e.g. `foo()()`, or the result of another
#'   subsetting call, e.g. `foo$bar()` or `foo@bar()`. In this case,
#'   the call head is not a symbol, it is another call (e.g. to the
#'   infix functions `$` or `@`). The call subtype is said to be
#'   "recursive".
#'
#' * A special subset of recursive calls are namespaced calls like
#'   `foo::bar()`. `switch_lang()` returns "namespaced" for these
#'   calls. It is generally a good idea if your function treats
#'   `bar()` and `foo::bar()` similarly.
#'
#' * Finally, it is possible to have a literal (see [is_expression()] for a
#'   definition of literals) as call head. In most cases, this will be
#'   a function inlined in the call (this is sometimes an expedient
#'   way of dealing with scoping issues). For calls with a literal
#'   node head, `switch_lang()` returns "inlined". Note that if a call
#'   head contains a literal that is not function, something went
#'   wrong and using that object will probably make R crash.
#'   `switch_lang()` issues an error in this case.
#'
#' The reason we use the term _node head_ is because calls are
#' structured as tree objects. This makes sense because the best
#' representation for language code is a tree whose hierarchy is
#' determined by the order of operations. See [node] for more on this.
#'
#'
#' @section Life cycle:
#'
#' All these functions are in the questioning stage and likely to be
#' removed from the package.
#'
#' @inheritParams switch_type
#' @param .x,x A language object (a call). If a formula quote, the RHS
#'   is extracted first.
#' @param ... Named clauses. The names should be types as returned by
#'   `lang_type_of()`.
#'
#' @keywords internal
#' @export
#' @examples
#' # Named calls:
#' lang_type_of(~foo())
#'
#' # Recursive calls:
#' lang_type_of(~foo$bar())
#' lang_type_of(~foo()())
#'
#' # Namespaced calls:
#' lang_type_of(~base::list())
#'
#' # For an inlined call, let's inline a function in the head node:
#' call <- quote(foo(letters))
#' call[[1]] <- base::toupper
#'
#' call
#' lang_type_of(call)
switch_lang <- function(.x, ...) {
  switch(lang_type_of(.x), ...)
}
#' @rdname switch_lang
#' @export
coerce_lang <- function(.x, .to, ...) {
  msg <- paste0("Can't convert ", type_of(.x), " to ", .to, "")
  switch(lang_type_of(.x), ..., abort(msg))
}
#' @rdname switch_lang
#' @export
lang_type_of <- function(x) {
  x <- get_expr(x)
  stopifnot(typeof(x) == "language")

  type <- typeof(node_car(x))
  if (type == "symbol") {
    "named"
  } else if (is_namespaced_symbol(node_car(x))) {
    "namespaced"
  } else if (type == "language") {
    "recursive"
  } else if (type %in% c("closure", "builtin", "special")) {
    "inlined"
  } else {
    abort("corrupt language object")
  }
}

# Exported `switch_lang()` function is likely to be deprecated in the
# future. Use switch_call() internally:
switch_call <- switch_lang

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
  switch_type(x,
    NULL = ,
    char = ,
    symbol = ,
    primitive = ,
    environment = ,
    pointer =
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
  .Call(rlang_is_reference, x, y)
}


# Use different generic name to avoid import warnings when loading
# packages that import all of rlang after it has been load_all'd
rlang_type_sum <- function(x) {
  if (is_installed("pillar")) {
    pillar::type_sum(x)
  } else {
    UseMethod("type_sum")
  }
}

type_sum.ordered <- function(x) "ord"
type_sum.factor <- function(x) "fct"
type_sum.POSIXct <- function(x) "dttm"
type_sum.difftime <- function(x) "time"
type_sum.Date <- function(x) "date"
type_sum.data.frame <- function(x) class(x)[[1]]

type_sum.default <- function(x) {
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
