#' Type predicates
#'
#' These type predicates aim to make type testing in R more
#' consistent. They are wrappers around \code{\link{typeof}}, so
#' operate at a level beneath S3/S4 etc.
#'
#' Compare to base R functions:
#' \itemize{

#'   \item The predicates for vectors include the \code{n} argument
#'     for pattern-matching on the vector length.
#'   \item Unlike \code{is.atomic()}, \code{is_atomic()} does not
#'     return \code{TRUE} for \code{NULL}.
#'   \item Unlike \code{is.vector()}, \code{is_vector()} test if an
#'     object is an atomic vector or a list. \code{is.vector} checks
#'     for the presence of attributes (other than name).
#'   \item \code{is_function()} returns \code{TRUE} only for regular
#'     functions, not special or primitive functions.
#' }
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @param encoding Expected encoding of a string or character
#'   vector. One of \code{UTF-8}, \code{latin1}, or \code{unknown}.
#' @seealso \link{bare-type-predicates} \link{scalar-type-predicates}
#' @name type-predicates
NULL

#' @export
#' @rdname type-predicates
is_list <- function(x, n = NULL) {
  if (typeof(x) != "list") return(FALSE)
  if (!is_null(n) && length(x) != n) return(FALSE)
  TRUE
}

parsable_atomic_types <- c("logical", "integer", "double", "complex", "character")
atomic_types <- c(parsable_atomic_types, "raw")
#' @export
#' @rdname type-predicates
is_atomic <- function(x, n = NULL) {
  if (!typeof(x) %in% atomic_types) return(FALSE)
  if (!is_null(n) && length(x) != n) return(FALSE)
  TRUE
}
#' @export
#' @rdname type-predicates
is_vector <- function(x, n = NULL) {
  is_atomic(x, n) || is_list(x, n)
}

#' @export
#' @rdname type-predicates
is_integer <- function(x, n = NULL) {
  if (typeof(x) != "integer")
  if (!is_null(n) && length(x) != n) return(FALSE)
  TRUE
}
#' @export
#' @rdname type-predicates
is_double <- function(x, n = NULL) {
  if (typeof(x) != "double") return(FALSE)
  if (!is_null(n) && length(x) != n) return(FALSE)
  TRUE
}
#' @export
#' @rdname type-predicates
is_character <- function(x, n = NULL, encoding = NULL) {
  if (typeof(x) != "character") return(FALSE)
  if (!is_null(n) && length(x) != n) return(FALSE)
  stopifnot(typeof(encoding) %in% c("character", "NULL"))
  if (!is_null(encoding) && !all(chr_encoding(x) %in% encoding)) return(FALSE)
  TRUE
}
#' @export
#' @rdname type-predicates
is_logical <- function(x, n = NULL) {
  if (typeof(x) != "logical") return(FALSE)
  if (!is_null(n) && length(x) != n) return(FALSE)
  TRUE
}
#' @export
#' @rdname type-predicates
is_raw <- function(x, n = NULL) {
  if (typeof(x) != "raw") return(FALSE)
  if (!is_null(n) && length(x) != n) return(FALSE)
  TRUE
}
#' @export
#' @rdname type-predicates
is_bytes <- is_raw

#' @export
#' @rdname type-predicates
is_null <- function(x) {
  typeof(x) == "NULL"
}

#' Scalar type predicates
#'
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' @inheritParams type-predicates
#' @param x object to be tested.
#' @seealso \link{type-predicates} \link{bare-type-predicates}
#' @name scalar-type-predicates
NULL

#' @export
#' @rdname scalar-type-predicates
is_scalar_list <- function(x) {
  is_list(x) && length(x) == 1
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_atomic <- function(x) {
  is_atomic(x) && length(x) == 1
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_vector <- function(x) {
  is_vector(x) && length(x) == 1
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_integer <- function(x) {
  is_integer(x) && length(x) == 1
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_double <- function(x) {
  is_double(x) && length(x) == 1
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_character <- function(x, encoding = NULL) {
  is_character(x, encoding = encoding) && length(x) == 1
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_logical <- function(x) {
  is_logical(x) && length(x) == 1
}
#' @export
#' @rdname scalar-type-predicates
is_scalar_raw <- function(x) {
  is_raw(x) && length(x) == 1
}
#' @export
#' @rdname scalar-type-predicates
is_string <- is_scalar_character
#' @export
#' @rdname scalar-type-predicates
is_scalar_bytes <- is_scalar_raw

#' Bare type predicates
#'
#' These predicates check for a given type but only return \code{TRUE}
#' for bare R objects. Bare objects have no class attributes. For
#' example, a data frame is a list, but not a bare list.
#'
#' \itemize{
#'   \item The predicates for vectors include the \code{n} argument
#'     for pattern-matching on the vector length.
#'   \item Like \code{\link{is_atomic}()} and unlike base R
#'     \code{is.atomic()}, \code{is_bare_atomic()} does not return
#'     \code{TRUE} for \code{NULL}.
#'   \item Unlike base R \code{is.numeric()}, \code{is_bare_double()}
#'     only returns \code{TRUE} for floating point numbers.
#' }
#' @inheritParams type-predicates
#' @seealso \link{type-predicates} \link{scalar-type-predicates}
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

#' Is object a formula?
#'
#' @inheritParams is_empty
#' @export
#' @examples
#' x <- disp ~ am
#' is_formula(x)
#'
#' is_formula(~ 10)
#' is_formula(10)
is_formula <- function(x) {
  if(typeof(x) != "language") {
    return(FALSE)
  }

  head <- x[[1]]
  if (!is_symbol(head)) {
    return(FALSE)
  }

  as.character(head) %in% c("~", ":=")
}

#' Is object an environment?
#'
#' \code{is_bare_env()} tests whether \code{x} is an environment
#' without a s3 or s4 class.
#'
#' @inheritParams is_empty
#' @export
is_env <- function(x) {
  typeof(x) == "environment"
}
#' @rdname is_env
#' @export
is_bare_env <- function(x) {
  !is.object(x) && typeof(x) == "environment"
}

#' Is object identical to TRUE or FALSE?
#'
#' These functions bypass R's automatic conversion rules and check
#' that \code{x} is literally \code{TRUE} or \code{FALSE}.
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
#' These predicates check whether R considers a number vector to be
#' integer-like, according to its own tolerance check (which is in
#' fact delegated to the C library). This function is not adapted to
#' data analysis, see the help for \code{\link[base]{is.integer}()}
#' for examples of how to check for whole numbers.
#'
#' @seealso \code{\link{is_bare_numeric}()} for testing whether an
#'   object is a base numeric type (a bare double or integer vector).
#' @inheritParams type-predicates
#' @export
#' @examples
#' is_integerish(10L)
#' is_integerish(10.0)
#' is_integerish(10.000001)
#' is_integerish(TRUE)
is_integerish <- function(x, n = NULL) {
  if (typeof(x) == "integer") return(TRUE)
  if (typeof(x) != "double") return(FALSE)
  if (!is_null(n) && length(x) != n) return(FALSE)
  all(x == as.integer(x))
}
#' @rdname is_integerish
#' @export
is_bare_integerish <- function(x, n = NULL) {
  !is.object(x) && is_integerish(x, n)
}
#' @rdname is_integerish
#' @export
is_scalar_integerish <- function(x) {
  !is.object(x) && is_integerish(x, 1L)
}

#' Base type of an object.
#'
#' This is equivalent to [base::typeof()] with a few differences that
#' make dispatching easier:
#' * The type of one-sided formulas is "quote".
#' * The type of character vectors of length 1 is "string".
#' * The type of special and builtin functions is "primitive".
#'
#' @param x An R object.
#' @export
#' @examples
#' type_of(10L)
#' type_of(~10L)
#' typeof(~10L)
#'
#' type_of(letters)
#' type_of(letters[[1]])
#'
#' typeof(list)
#' typeof(`$`)
#' type_of(list)
#' type_of(`$`)
#'
#' type_of(quote(base::list()))
#' @md
type_of <- function(x) {
  type <- typeof(x)
  if (is_fquote(x)) {
    "quote"
  } else if (type == "character") {
    if (length(x) == 1) "string" else "character"
  } else if (type %in% c("builtin", "special")) {
    "primitive"
  } else {
    type
  }
}

#' Dispatch on base types.
#'
#' This is equivalent to \code{\link[base]{switch}(\link{type_of}(x,
#' ...))}. The `coerce_` versions are intended for type conversion and
#' provide a standard error message when conversion fails.
#'
#' @param .x An object from which to dispatch.
#' @param ... Named clauses. The names should be types as returned by
#'   [type_of()].
#' @param .to This is useful when you switchpatch within a coercing
#'   function. If supplied, this should be a string indicating the
#'   target type. A catch-all clause is then added to signal an error
#'   stating the conversion failure.
#' @seealso [switch_lang()]
#' @export
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
#'   coerce_type(x, "chr",
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
#' @md
switch_type <- function(.x, ...) {
    switch(type_of(.x), ...)
}
#' @rdname switch_type
#' @export
coerce_type <- function(.x, .to, ...) {
  msg <- paste0("Cannot convert objects of type `", type_of(.x), "` to `", .to, "`")
  switch(type_of(.x), ..., abort_coercion(.x, .to))
}
switch_class <- function(.x, ..., .to) {
  if (missing(.to)) {
    switch(class(.x), ...)
  } else {
    switch(class(.x), ..., abort_coercion(.x, .to))
  }
}
abort_coercion <- function(x, to) {
  abort(paste0(
    "Cannot convert objects of type `", type_of(x), "` to `", to, "`"
  ))
}

#' Dispatch on call type.
#'
#' `switch_lang()` dispatches clauses based on the subtype of call, as
#' determined by `lang_type_of()`. The subtypes are based on the type
#' of call head (see details).
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
#' * Finally, it is possible to have a literal (see [is_expr()] for a
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
#' representation for language code is a parse tree, with the tree
#' hierarchy determined by the order of operations. See [pairlist] for
#' more on this.
#'
#' @inheritParams switch_type
#' @param .x,x A language object (a call). If a formula quote, the RHS
#'   is extracted first.
#' @param ... Named clauses. The names should be types as returned by
#'   `lang_type_of()`.
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
#' @md
switch_lang <- function(.x, ...) {
  switch(lang_type_of(.x), ...)
}
#' @rdname switch_lang
#' @export
coerce_lang <- function(.x, .to, ...) {
  msg <- paste0("Cannot convert objects of type `", type_of(.x), "` to `", .to, "`")
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
