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
  signal_soft_deprecated(c(
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
  signal_soft_deprecated(c(
    "`switch_type()` is soft-deprecated as of rlang 0.4.0.",
    "Please use `switch(typeof())` or `switch(my_typeof())` instead."
  ))
  switch(type_of_(.x), ...)
}
#' @rdname switch_type
#' @export
coerce_type <- function(.x, .to, ...) {
  signal_soft_deprecated("`coerce_type()` is soft-deprecated as of rlang 0.4.0.")
  switch(type_of_(.x), ..., abort_coercion(.x, .to))
}
#' @rdname switch_type
#' @export
switch_class <- function(.x, ...) {
  signal_soft_deprecated("`switch_class()` is soft-deprecated as of rlang 0.4.0.")
  switch(class(.x), ...)
}
#' @rdname switch_type
#' @export
coerce_class <- function(.x, .to, ...) {
  signal_soft_deprecated("`coerce_class()` is soft-deprecated as of rlang 0.4.0.")
  switch(class(.x), ..., abort_coercion(.x, .to))
}

#' Format a type for error messages
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `friendly_type()` is deprecated. Please use the
#' `compat-friendly-type.R` file instead.
#'
#' @param type A type as returned by [typeof()].
#' @return A string of the prettified type, qualified with an
#'   indefinite article.
#' @export
#' @keywords internal
friendly_type <- function(type) {
  signal_soft_deprecated("`friendly_type()` is deprecated as of rlang 0.4.11.")
  type
}


##  Casting

#' Coerce an object to a base type
#'
#' @description
#'
#' `r lifecycle::badge("soft-deprecated")`
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
#' as.list.foobar <- function(x) abort("dont call me")
#' as_list(x)
#' @name vector-coercion
NULL

signal_deprecated_cast <- function(fn, env = caller_env(2)) {
  signal_soft_deprecated(env = env, c(
    sprintf("`%s()` is deprecated as of rlang 0.4.0", fn),
    "Please use `vctrs::vec_cast()` instead."
  ))
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
  switch_type(x,
    environment = env_as_list(x),
    vec_as_list(x)
  )
}
env_as_list <- function(x) {
  names_x <- names(x)
  x <- as_base_type(x, as.list)
  set_names(x, .Call(ffi_unescape_character, names_x))
}
vec_as_list <- function(x) {
  coerce_type_vec(x, friendly_type_of(list(), value = FALSE),
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

legacy_as_logical <- function(x) {
  coerce_type_vec(x, friendly_type_of(lgl(), value = FALSE),
    logical = { attributes(x) <- NULL; x },
    integer = as_base_type(x, as.logical),
    double = as_integerish_type(x, as.logical, lgl())
  )
}
legacy_as_integer <- function(x) {
  coerce_type_vec(x, friendly_type_of(int(), value = FALSE),
    logical = as_base_type(x, as.integer),
    integer = { attributes(x) <- NULL; x },
    double = as_integerish_type(x, as.integer, int(), value = FALSE)
  )
}
legacy_as_double <- function(x) {
  coerce_type_vec(x, friendly_type_of(dbl(), value = FALSE),
    logical = ,
    integer = as_base_type(x, as.double),
    double = { attributes(x) <- NULL; x }
  )
}
legacy_as_complex <- function(x) {
  coerce_type_vec(x, friendly_type_of(cpl(), value = FALSE),
    logical = ,
    integer = ,
    double = as_base_type(x, as.complex),
    complex = { attributes(x) <- NULL; x }
  )
}
legacy_as_character <- function(x, encoding = NULL) {
  if (is_unspecified(x)) {
    return(rep_along(x, na_chr))
  }
  coerce_type_vec(
    x,
    friendly_type_of(chr(), value = FALSE),
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
      friendly_type_of(to, value = value),
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

#' Get caller frame
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param n Number of frames to go back.
#' @keywords internal
#' @export
caller_frame <- function(n = 1) {
  warn_deprecated("`caller_frame()` is deprecated as of rlang 0.3.0.")
  call_frame(n + 2)
}

#' Call stack information
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' The `eval_` and `call_` families of functions provide a replacement
#' for the base R functions prefixed with `sys.` (which are all about
#' the context stack), as well as for [parent.frame()] (which is the
#' only base R function for querying the call stack). The context
#' stack includes all R-level evaluation contexts. It is linear in
#' terms of execution history but due to lazy evaluation it is
#' potentially nonlinear in terms of call history. The call stack
#' history, on the other hand, is homogenous.
#'
#' @details
#'
#' `ctxt_frame()` and `call_frame()` return a `frame` object
#' containing the following fields: `expr` and `env` (call expression
#' and evaluation environment), `pos` and `caller_pos` (position of
#' current frame in the context stack and position of the caller), and
#' `fun` (function of the current frame). `ctxt_stack()` and
#' `call_stack()` return a list of all context or call frames on the
#' stack. Finally, `ctxt_depth()` and `call_depth()` report the
#' current context position or the number of calling frames on the
#' stack.
#'
#' The base R functions take two sorts of arguments to indicate which
#' frame to query: `which` and `n`. The `n` argument is
#' straightforward: it's the number of frames to go down the stack,
#' with `n = 1` referring to the current context. The `which` argument
#' is more complicated and changes meaning for values lower than 1.
#' For the sake of consistency, the rlang functions all take the
#' same kind of argument `n`. This argument has a single meaning (the
#' number of frames to go down the stack) and cannot be lower than 1.
#'
#' Note finally that `parent.frame(1)` corresponds to
#' `call_frame(2)$env`, as `n = 1` always refers to the current
#' frame. This makes the `_frame()` and `_stack()` functions
#' consistent: `ctxt_frame(2)` is the same as `ctxt_stack()[[2]]`.
#' Also, `ctxt_depth()` returns one more frame than
#' [base::sys.nframe()] because it counts the global frame. That is
#' consistent with the `_stack()` functions which return the global
#' frame as well. This way, `call_stack(call_depth())` is the same as
#' `global_frame()`.
#'
#'
#' @section Life cycle:
#'
#' These functions are soft-deprecated and replaced by [trace_back()].
#'
#' @param n The number of frames to go back in the stack.
#' @param clean Whether to post-process the call stack to clean
#'   non-standard frames. If `TRUE`, suboptimal call-stack entries by
#'   [base::eval()] will be cleaned up: the duplicate frame created by
#'   `eval()` is eliminated.
#' @param trim The number of layers of intervening frames to trim off
#'   the stack. See [stack_trim()] and examples.
#' @name stack
#' @keywords internal
#' @examples
#' # Expressions within arguments count as contexts
#' identity(identity(ctxt_depth())) # returns 2
#'
#' # But they are not part of the call stack because arguments are
#' # evaluated within the calling function (or the global environment
#' # if called at top level)
#' identity(identity(call_depth())) # returns 0
#'
#' # The context stacks includes all intervening execution frames. The
#' # call stack doesn't:
#' f <- function(x) identity(x)
#' f(f(ctxt_stack()))
#' f(f(call_stack()))
#'
#' g <- function(cmd) cmd()
#' f(g(ctxt_stack))
#' f(g(call_stack))
#'
#' # The rlang _stack() functions return a list of frame
#' # objects. Use purrr::transpose() or index a field with
#' # purrr::map()'s to extract a particular field from a stack:
#'
#' # stack <- f(f(call_stack()))
#' # purrr::map(stack, "env")
#' # purrr::transpose(stack)$expr
#'
#' # current_frame() is an alias for ctxt_frame(1)
#' fn <- function() list(current = current_frame(), first = ctxt_frame(1))
#' fn()
#'
#' # While current_frame() is the top of the stack, global_frame() is
#' # the bottom:
#' fn <- function() {
#'   n <- ctxt_depth()
#'   ctxt_frame(n)
#' }
#' identical(fn(), global_frame())
#'
#'
#' # ctxt_stack() returns a stack with all intervening frames. You can
#' # trim layers of intervening frames with the trim argument:
#' identity(identity(ctxt_stack()))
#' identity(identity(ctxt_stack(trim = 1)))
#'
#' # ctxt_stack() is called within fn() with intervening frames:
#' fn <- function(trim) identity(identity(ctxt_stack(trim = trim)))
#' fn(0)
#'
#' # We can trim the first layer of those:
#' fn(1)
#'
#' # The outside intervening frames (at the fn() call site) are still
#' # returned, but can be trimmed as well:
#' identity(identity(fn(1)))
#' identity(identity(fn(2)))
#'
#' g <- function(trim) identity(identity(fn(trim)))
#' g(2)
#' g(3)
NULL

new_frame <- function(x) {
  structure(x, class = "frame")
}
#' @export
print.frame <- function(x, ...) {
  cat("<frame ", x$pos, ">", sep = "")
  if (!x$pos) {
    cat(" [global]\n")
  } else {
    cat(" (", x$caller_pos, ")\n", sep = "")
  }

  expr <- deparse(x$expr)
  if (length(expr) > 1) {
    expr <- paste(expr[[1]], "<...>")
  }
  cat("expr: ", expr, "\n", sep = "")
  cat("env:  [", env_format(x$env), "]\n", sep = "")
}
#' Is object a frame?
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param x Object to test
#' @keywords internal
#' @export
is_frame <- function(x) {
  inherits(x, "frame")
}

#' @rdname stack
#' @export
global_frame <- function() {
  warn_deprecated("`global_frame()` is deprecated as of rlang 0.3.0.")
  new_frame(list(
    pos = 0L,
    caller_pos = NA_integer_,
    expr = NULL,
    env = globalenv(),
    fn = NULL,
    fn_name = NULL
  ))
}
#' @rdname stack
#' @export
current_frame <- function() {
  warn_deprecated("`current_frame()` is deprecated as of rlang 0.3.0.")
  ctxt_frame(2)
}

#' @rdname stack
#' @export
ctxt_frame <- function(n = 1) {
  warn_deprecated("`ctxt_frame()` is deprecated as of rlang 0.3.0.")
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

# Positions of frames in the call stack up to `n`
trail_make <- function(callers, n = NULL, clean = TRUE) {
  n_ctxt <- length(callers)
  if (is.null(n)) {
    n_max <- n_ctxt
  } else {
    if (n > n_ctxt) {
      stop("not that many frames on the evaluation stack", call. = FALSE)
    }
    n_max <- n + 1
  }

  state <- trail_next(callers, 1, clean)
  if (!length(state$i) || state$i == 0) {
    return(0L)
  }
  j <- 1

  # Preallocate a sufficiently large vector
  out <- integer(n_max)
  out[j] <- state$i

  while (state$i != 0 && j < n_max) {
    j <- j + 1
    n_ctxt <- length(state$callers)
    next_pos <- n_ctxt - state$i + 1
    state <- trail_next(state$callers, next_pos, clean)
    out[j] <- state$i
  }

  # Return relevant subset
  if (!is.null(n) && n > j) {
    stop("not that many frames on the call stack", call. = FALSE)
  }
  out[seq_len(j)]
}

trail_next <- function(callers, i, clean) {
  if (i == 0L) {
    return(list(callers = callers, i = 0L))
  }

  i <- callers[i]

  if (clean) {
    # base::Recall() creates a custom context with the wrong sys.parent()
    if (identical(sys.function(i - 1L), base::Recall)) {
      i_pos <- trail_index(callers, i)
      callers[i_pos] <- i - 1L
    }

    # The R-level eval() creates two contexts. We skip the second one
    if (length(i) && is_prim_eval(sys.function(i))) {
      n_ctxt <- length(callers)
      special_eval_pos <- trail_index(callers, i)
      callers <- callers[-special_eval_pos]
      i <- i - 1L
    }

  }

  list(callers = callers, i = i)
}

trail_index <- function(callers, i) {
  n_ctxt <- length(callers)
  n_ctxt - i + 1L
}

#' @rdname stack
#' @export
call_frame <- function(n = 1, clean = TRUE) {
  stopifnot(n > 0)

  eval_callers <- ctxt_stack_callers()
  trail <- trail_make(eval_callers, n, clean = clean)
  pos <- trail[n]

  if (identical(pos, 0L)) {
    return(global_frame())
  }

  frame <- new_frame(list(
    pos = pos,
    caller_pos = trail[n + 1],
    expr = sys.call(pos),
    env = sys.frame(pos),
    fn = sys.function(pos),
    fn_name = call_name(sys.call(pos))
  ))

  if (clean) {
    frame <- frame_clean_eval(frame)
  }
  frame
}


# The _depth() functions count the global frame as well

#' @rdname stack
#' @export
ctxt_depth <- function() {
  warn_deprecated("`ctxt_depth()` is deprecated as of rlang 0.3.0.")
  sys.nframe()
}
#' @rdname stack
#' @export
call_depth <- function() {
  warn_deprecated("`call_depth()` is deprecated as of rlang 0.3.0.")
  eval_callers <- ctxt_stack_callers()
  trail <- trail_make(eval_callers)
  length(trail)
}


# Summaries ----------------------------------------------------------

#' @rdname stack
#' @export
ctxt_stack <- function(n = NULL, trim = 0) {
  warn_deprecated("`ctxt_stack()` is deprecated as of rlang 0.3.0.")

  stack_data <- list(
    pos = ctxt_stack_trail(),
    caller_pos = ctxt_stack_callers(),
    expr = ctxt_stack_exprs(),
    env = ctxt_stack_envs(),
    fn = ctxt_stack_fns()
  )

  # Remove ctxt_stack() from stack
  stack_data <- map(stack_data, drop_first)

  stack_data <- stack_subset(stack_data, n)
  stack_data$fn_name <- map(stack_data$expr, call_name)

  stack <- transpose(stack_data)
  stack <- map(stack, new_frame)

  if (is.null(n) || (length(n) && n > length(stack))) {
    stack <- c(stack, list(global_frame()))
  }
  if (trim > 0) {
    stack <- stack_trim(stack, n = trim + 1)
  }

  structure(stack, class = c("ctxt_stack", "stack"))
}

ctxt_stack_trail <- function() {
  pos <- sys.nframe() - 1
  seq(pos, 1)
}
ctxt_stack_exprs <- function() {
  exprs <- sys.calls()
  rev(drop_last(exprs))
}
ctxt_stack_envs <- function(n = 1) {
  envs <- sys.frames()
  rev(drop_last(envs))
}
ctxt_stack_callers <- function() {
  callers <- sys.parents()
  rev(drop_last(callers))
}
ctxt_stack_fns <- function() {
  pos <- sys.nframe() - 1
  map(seq(pos, 1), sys.function)
}

stack_subset <- function(stack_data, n) {
  if (length(n)) {
    stopifnot(n > 0)
    n_stack <- length(stack_data[[1]])
    if (n == n_stack + 1) {
      # We'll add the global frame later
      n <- n <- n - 1
    } else if (n > n_stack + 1) {
      stop("not that many frames on the stack", call. = FALSE)
    }
    stack_data <- map(stack_data, `[`, seq_len(n))
  }
  stack_data
}

#' @rdname stack
#' @export
call_stack <- function(n = NULL, clean = TRUE) {
  warn_deprecated("`call_stack()` is deprecated as of rlang 0.3.0.")

  eval_callers <- ctxt_stack_callers()
  trail <- trail_make(eval_callers, n, clean = clean)

  stack_data <- list(
    pos = drop_last(trail),
    caller_pos = drop_first(trail),
    expr = map(trail, sys.call),
    env = map(trail, sys.frame),
    fn = map(trail, sys.function)
  )
  stack_data$fn_name <- map(stack_data$expr, call_name)

  stack <- transpose(stack_data)
  stack <- map(stack, new_frame)
  if (clean) {
    stack <- map(stack, frame_clean_eval)
  }

  if (trail[length(trail)] == 0L) {
    stack <- c(stack, list(global_frame()))
  }

  structure(stack, class = c("call_stack", "stack"))
}

frame_clean_eval <- function(frame) {
  if (identical(frame$fn, base::eval)) {
    # Use the environment from the context created in do_eval()
    # (the context with the fake primitive call)
    stopifnot(is_prim_eval(sys.function(frame$pos + 1)))
    frame$env <- sys.frame(frame$pos + 1)
  }

  frame
}

#' Is object a stack?
#'
#' `r lifecycle::badge("soft-deprecated")`
#'
#' @param x An object to test
#' @keywords internal
#' @export
is_stack <- function(x) {
  warn_deprecated("`is_stack()` is deprecated as of rlang 0.3.0.")
  inherits(x, "stack")
}

#' @rdname is_stack
#' @export
is_eval_stack <- function(x) {
  warn_deprecated("`is_eval_stack()` is deprecated as of rlang 0.3.0.")
  inherits(x, "ctxt_stack")
}

#' @rdname is_stack
#' @export
is_call_stack <- function(x) {
  warn_deprecated("`is_call_stack()` is deprecated as of rlang 0.3.0.")
  inherits(x, "call_stack")
}

#' @export
`[.stack` <- function(x, i) {
  structure(NextMethod(), class = class(x))
}

# Handles global_frame() whose `caller_pos` is NA
sys_frame <- function(n) {
  if (is.na(n)) {
    NULL
  } else {
    sys.frame(n)
  }
}

#' Find the position or distance of a frame on the evaluation stack
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' The frame position on the stack can be computed by counting frames
#' from the global frame (the bottom of the stack, the default) or
#' from the current frame (the top of the stack).
#'
#' @details
#'
#' While this function returns the position of the frame on the
#' evaluation stack, it can safely be called with intervening frames
#' as those will be discarded.
#'
#'
#' @section Life cycle:
#'
#' These functions are deprecated and replaced by [trace_back()].
#'
#' @param frame The environment of a frame. Can be any object with a
#'   [get_env()] method. Note that for frame objects, the position from
#'   the global frame is simply `frame$pos`. Alternatively, `frame`
#'   can be an integer that represents the position on the stack (and
#'   is thus returned as is if `from` is "global".
#' @param from Whether to compute distance from the global frame (the
#'   bottom of the evaluation stack), or from the current frame (the
#'   top of the evaluation stack).
#'
#' @keywords internal
#' @export
#' @examples
#' fn <- function() g(environment())
#' g <- function(env) frame_position(env)
#'
#' # frame_position() returns the position of the frame on the evaluation stack:
#' fn()
#' identity(identity(fn()))
#'
#' # Note that it trims off intervening calls before counting so you
#' # can safely nest it within other calls:
#' g <- function(env) identity(identity(frame_position(env)))
#' fn()
#'
#' # You can also ask for the position from the current frame rather
#' # than the global frame:
#' fn <- function() g(environment())
#' g <- function(env) h(env)
#' h <- function(env) frame_position(env, from = "current")
#' fn()
frame_position <- function(frame, from = c("global", "current")) {
  warn_deprecated("`frame_position()` is deprecated as of rlang 0.3.0.")

  stack <- stack_trim(ctxt_stack(), n = 2)

  if (arg_match(from) == "global") {
    frame_position_global(frame, stack)
  } else {
    caller_pos <- call_frame(2)$pos
    frame_position_current(frame, stack, caller_pos)
  }
}

frame_position_global <- function(frame, stack = NULL) {
  if (is_frame(frame)) {
    return(frame$pos)
  } else if (is_integerish(frame)) {
    return(frame)
  }

  frame <- get_env(frame)
  stack <- stack %||% stack_trim(ctxt_stack(), n = 2)
  envs <- map(stack, "[[", "env")

  i <- 1
  for (env in envs) {
    if (identical(env, frame)) {
      return(length(envs) - i)
    }
    i <- i + 1
  }

  abort("`frame` not found on evaluation stack")
}

frame_position_current <- function(frame, stack = NULL,
                                   caller_pos = NULL) {
  if (is_integerish(frame)) {
    pos <- frame
  } else {
    stack <- stack %||% stack_trim(ctxt_stack(), n = 2)
    pos <- frame_position_global(frame, stack)
  }
  caller_pos <- caller_pos %||% call_frame(2)$pos
  caller_pos - pos + 1
}


#' Trim top call layers from the evaluation stack
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' [ctxt_stack()] can be tricky to use in real code because all
#' intervening frames are returned with the stack, including those at
#' `ctxt_stack()` own call site. `stack_trim()` makes it easy to
#' remove layers of intervening calls.
#'
#'
#' @section Life cycle:
#'
#' These functions are deprecated and replaced by [trace_back()].
#'
#' @param stack An evaluation stack.
#' @param n The number of call frames (not eval frames) to trim off
#'   the top of the stack. In other words, the number of layers of
#'   intervening frames to trim.
#' @export
#' @keywords internal
#' @examples
#' # Intervening frames appear on the evaluation stack:
#' identity(identity(ctxt_stack()))
#'
#' # stack_trim() will trim the first n layers of calls:
#' stack_trim(identity(identity(ctxt_stack())))
#'
#' # Note that it also takes care of calls intervening at its own call
#' # site:
#' identity(identity(
#'   stack_trim(identity(identity(ctxt_stack())))
#' ))
#'
#' # It is especially useful when used within a function that needs to
#' # inspect the evaluation stack but should nonetheless be callable
#' # within nested calls without side effects:
#' stack_util <- function() {
#'   # n = 2 means that two layers of intervening calls should be
#'   # removed: The layer at ctxt_stack()'s call site (including the
#'   # stack_trim() call), and the layer at stack_util()'s call.
#'   stack <- stack_trim(ctxt_stack(), n = 2)
#'   stack
#' }
#' user_fn <- function() {
#'   # A user calls your stack utility with intervening frames:
#'   identity(identity(stack_util()))
#' }
#' # These intervening frames won't appear in the evaluation stack
#' identity(user_fn())
stack_trim <- function(stack, n = 1) {
  warn_deprecated("`stack_trim()` is deprecated as of rlang 0.3.0.")

  if (n < 1) {
    return(stack)
  }

  # Add 1 to discard stack_trim()'s own intervening frames
  caller_pos <- call_frame(n + 1, clean = FALSE)$pos

  n_frames <- length(stack)
  n_skip <- n_frames - caller_pos
  stack[seq(n_skip, n_frames)]
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
  warn_deprecated(paste_line(
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
#' @param x A function, raw expression, or formula to interpolate.
#' @param env The environment in which unquoted expressions should be
#'   evaluated. By default, the formula or closure environment if a
#'   formula or a function, or the current environment otherwise.
#' @examples
#' # All tidy NSE functions like quo() unquote on capture:
#' quo(list(!!(1 + 2)))
#'
#' # expr_interp() is meant to provide the same functionality when you
#' # have a formula or expression that might contain unquoting
#' # operators:
#' f <- ~list(!!(1 + 2))
#' expr_interp(f)
#'
#' # Note that only the outer formula is unquoted (which is a reason
#' # to use expr_interp() as early as possible in all user-facing
#' # functions):
#' f <- ~list(~!!(1 + 2), !!(1 + 2))
#' expr_interp(f)
#'
#'
#' # Another purpose for expr_interp() is to interpolate a closure's
#' # body. This is useful to inline a function within another. The
#' # important limitation is that all formal arguments of the inlined
#' # function should be defined in the receiving function:
#' other_fn <- function(x) toupper(x)
#'
#' fn <- expr_interp(function(x) {
#'   x <- paste0(x, "_suffix")
#'   !!! body(other_fn)
#' })
#' fn
#' fn("foo")
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
  warn_deprecated(paste_line(
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
  warn_deprecated(paste_line(
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
  expr <- get_expr(call)
  if (!is_call(expr)) {
    abort_call_input_type("call")
  }

  if (is_frame(call)) {
    fn <- call$fn
  } else {
    # The call name might be a literal, not necessarily a symbol
    env <- get_env(call, env)
    fn <- eval_bare(node_car(expr), env)
  }

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
#'
#' Deprecated in rlang 0.4.11.
#'
#' If `call` is a quosure or formula, the function will be retrieved
#' from the associated environment. Otherwise, it is looked up in the
#' calling frame.
#'
#' @param call A defused function call.
#' @param env The environment where to find the definition of the
#'   function quoted in `call` in case `call` is not wrapped in a
#'   quosure.
#' @seealso [call_name()]
#' @examples
#' # Extract from a quoted call:
#' call_fn(quote(matrix()))
#' @keywords internal
#' @export
call_fn <- function(call, env = caller_env()) {
  expr <- get_expr(call)
  env <- get_env(call, env)

  if (!is_call(expr)) {
    abort_call_input_type("call")
  }

  switch(call_type(expr),
    recursive = abort("`call` does not call a named or inlined function"),
    inlined = node_car(expr),
    named = ,
    namespaced = ,
    eval_bare(node_car(expr), env)
  )
}


#  Nodes  ------------------------------------------------------------

#' Mutate node components
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated and renamed with `node_poke_`
#' prefix in rlang 0.2.0. This change follows a new naming convention
#' where mutation is referred to as "poking".
#'
#' @inheritParams new_node
#'
#' @keywords internal
#' @export
mut_node_car <- function(x, newcar) {
  warn_deprecated("`mut_node_car()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(ffi_node_poke_car, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cdr <- function(x, newcdr) {
  warn_deprecated("`mut_node_cdr()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(ffi_node_poke_cdr, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_caar <- function(x, newcar) {
  warn_deprecated("`mut_node_caar()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(ffi_node_poke_caar, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cadr <- function(x, newcar) {
  warn_deprecated("`mut_node_cadr()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(ffi_node_poke_cadr, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cdar <- function(x, newcdr) {
  warn_deprecated("`mut_node_cdar()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(ffi_node_poke_cdar, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_cddr <- function(x, newcdr) {
  warn_deprecated("`mut_node_cddr()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(ffi_node_poke_cddr, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_tag <- function(x, newtag) {
  warn_deprecated("`mut_node_tag()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(ffi_node_poke_tag, x, newtag))
}

#' @rdname vector-old-ctors
#' @export
node <- function(car, cdr = NULL) {
  warn_deprecated(paste_line(
    "`node()` is deprecated as of rlang 0.2.0.",
    "Please use `new_node()` instead."
  ))
  new_node(car, cdr)
}


#  Environments  -----------------------------------------------------

#' Bind a promise or active binding
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' As of rlang 0.3.0, `env_bind_exprs()` and `env_bind_fns()` have
#' been renamed to [env_bind_lazy()] and [env_bind_active()] for
#' consistency.
#'
#' @inheritParams env_bind
#'
#' @keywords internal
#' @export
env_bind_exprs <- function(.env, ..., .eval_env = caller_env()) {
  warn_deprecated(paste_line(
    "`env_bind_exprs()` is deprecated as of rlang 0.3.0.",
    "Please use `env_bind_lazy()` instead."
  ))
  env_bind_lazy(.env = .env, ..., .eval_env = .eval_env)
}
#' @rdname env_bind_exprs
#' @export
env_bind_fns <- function(.env, ...) {
  warn_deprecated(paste_line(
    "`env_bind_fns()` is deprecated as of rlang 0.3.0.",
    "Please use `env_bind_active()` instead."
  ))
  env_bind_active(.env = .env, ...)
}

#' Retired `scoped` functions
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated as of rlang 0.3.0. They are replaced
#' by [is_attached()], ...
#'
#' @param nm The name of an environment attached to the search
#'   path. Call [base::search()] to see what is currently on the path.
#'
#' @keywords internal
#' @export
scoped_env <- function(nm) {
  warn_deprecated(paste_line(
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
#' @rdname scoped_env
#' @export
is_scoped <- function(nm) {
  warn_deprecated(paste_line(
    "`is_scoped()` is deprecated as of rlang 0.3.0.",
    "Please use `is_attached()` instead."
  ))
  local_options(lifecycle_disable_warnings = TRUE)

  if (!is_scalar_character(nm)) {
    stop("`nm` must be a string", call. = FALSE)
  }
  nm %in% scoped_names()
}
#' @rdname scoped_env
#' @export
scoped_envs <- function() {
  warn_deprecated(paste_line(
    "`scoped_envs()` is deprecated as of rlang 0.3.0.",
    "Please use `search_envs()` instead."
  ))
  local_options(lifecycle_disable_warnings = TRUE)

  envs <- c(list(.GlobalEnv), env_parents(.GlobalEnv))
  set_names(envs, scoped_names())
}
#' @rdname scoped_env
#' @export
scoped_names <- function() {
  warn_deprecated(paste_line(
    "`scoped_names()` is deprecated as of rlang 0.3.0.",
    "Please use `base::search()` instead."
  ))
  c(search(), "NULL")
}


#  Vectors  ----------------------------------------------------------

#' Retired vector construction by length
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated and renamed with `new_` prefix in
#' rlang 0.2.0. This is for consistency with other non-variadic object
#' constructors.
#'
#' @param .x A vector.
#' @name vector-old-ctors
#' @keywords internal
NULL

#' @rdname vector-old-ctors
#' @export
lgl_along <- function(.x) {
  stop_defunct("`lgl_along()` is deprecated as of rlang 0.2.0.")
}
#' @rdname vector-old-ctors
#' @export
int_along <- function(.x) {
  stop_defunct("`int_along()` is deprecated as of rlang 0.2.0.")
}


#  Attributes  -------------------------------------------------------

#' Add attributes to an object
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#' `r lifecycle::badge("deprecated")`
#'
#' `set_attrs()` adds, changes, or zaps attributes of objects. Pass a
#' single unnamed `NULL` argument to zap all attributes. For
#' [uncopyable][is_copyable] types, use `mut_attrs()`.
#'
#' @details
#'
#' Unlike [structure()], these setters have no special handling of
#' internal attributes names like `.Dim`, `.Dimnames` or `.Names`.
#'
#'
#' @section Life cycle:
#'
#' These functions are deprecated since rlang 0.3.0.
#'
#' @param .x An object to decorate with attributes.
#' @param ... <[dynamic][dyn-dots]> A list of named attributes. Pass
#'   a single unnamed `NULL` argument to zap all attributes from `.x`.
#' @return `set_attrs()` returns a modified [shallow copy][duplicate]
#'   of `.x`. `mut_attrs()` invisibly returns the original `.x`
#'   modified in place.
#'
#' @keywords internal
#' @export
#' @examples
#' set_attrs(letters, names = 1:26, class = "my_chr")
#'
#' # Splice a list of attributes:
#' attrs <- list(attr = "attr", names = 1:26, class = "my_chr")
#' obj <- set_attrs(letters, splice(attrs))
#' obj
#'
#' # Zap attributes by passing a single unnamed NULL argument:
#' set_attrs(obj, NULL)
#' set_attrs(obj, !!! list(NULL))
#'
#' # Note that set_attrs() never modifies objects in place:
#' obj
#'
#' # For uncopyable types, mut_attrs() lets you modify in place:
#' env <- env()
#' mut_attrs(env, foo = "bar")
#' env
set_attrs <- function(.x, ...) {
  warn_deprecated("`set_attrs()` is deprecated as of rlang 0.3.0")

  if (!is_copyable(.x)) {
    abort("`.x` is uncopyable: use `mut_attrs()` to change attributes in place")
  }
  set_attrs_impl(.x, ...)
}
#' @rdname set_attrs
#' @export
mut_attrs <- function(.x, ...) {
  warn_deprecated("`set_attrs()` is deprecated as of rlang 0.3.0")

  if (is_copyable(.x)) {
    abort("`.x` is copyable: use `set_attrs()` to change attributes without side effect")
  }
  invisible(set_attrs_impl(.x, ...))
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

#' Is a vector uniquely named?
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Like [is_named()] but also checks that names are unique.
#' @param x A vector.
#' @keywords internal
#' @export
is_dictionaryish <- function(x) {
  if (!length(x)) {
    return(!is.null(x))
  }

  is_named(x) && !any(duplicated(names(x)))
}


#  Conditions --------------------------------------------------------

#' Exiting handler
#'
#' @description
#'
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `exiting()` is no longer necessary as handlers are exiting by default.
#'
#' @keywords internal
#' @export
exiting <- function(handler) {
  signal_soft_deprecated(c(
    "`exiting()` is soft-deprecated as of rlang 0.4.0.",
    "Handlers are now treated as exiting by default."
  ))
  handler <- as_function(handler)
  structure(handler, class = c("rlang_handler_exiting", "rlang_handler", "function"))
}


#  Scoped_

#' Questioning `scoped_` functions
#'
#' @description
#'
#' `r lifecycle::badge("questioning")`
#'
#' These functions have been renamed to use the conventional `local_`
#' prefix. They will be deprecated in the next minor version of rlang.
#'
#' @inheritParams local_interactive
#' @inheritParams local_options
#' @inheritParams local_bindings
#'
#' @keywords internal
#'
#' @export
scoped_interactive <- function(value = TRUE, frame = caller_env()) {
  local_interactive(value = value, frame = frame)
}
#' @rdname scoped_interactive
#' @export
scoped_options <- function(..., .frame = caller_env()) {
  local_options(..., .frame = .frame)
}
#' @rdname scoped_interactive
#' @export
scoped_bindings <- function(..., .env = .frame, .frame = caller_env()) {
  local_bindings(..., .env = .env, .frame = .frame)
}


#  Superseded

#' Mask bindings by defining symbols deeper in a scope
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is superseded. Please use [env()] (and possibly
#' [set_env()] if you're masking the bindings for another object like
#' a closure or a formula) instead.
#'
#' `env_bury()` is like [env_bind()] but it creates the bindings in a
#' new child environment. This makes sure the new bindings have
#' precedence over old ones, without altering existing environments.
#' Unlike `env_bind()`, this function does not have side effects and
#' returns a new environment (or object wrapping that environment).
#'
#' @inheritParams env_bind
#' @return A copy of `.env` enclosing the new environment containing
#'   bindings to `...` arguments.
#' @seealso [env_bind()], [env_unbind()]
#'
#' @keywords internal
#' @export
#' @examples
#' orig_env <- env(a = 10)
#' fn <- set_env(function() a, orig_env)
#'
#' # fn() currently sees `a` as the value `10`:
#' fn()
#'
#' # env_bury() will bury the current scope of fn() behind a new
#' # environment:
#' fn <- env_bury(fn, a = 1000)
#' fn()
#'
#' # Even though the symbol `a` is still defined deeper in the scope:
#' orig_env$a
env_bury <- function(.env, ...) {
  env_ <- get_env(.env)
  env_ <- child_env(env_, ...)
  set_env(.env, env_)
}
