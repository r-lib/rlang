
#  Soft-deprecated in rlang 0.4.0

##  Types

#' Base type of an object
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
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


##  Casting

#' Coerce an object to a base type
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
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
  coerce_type_vec(x, friendly_type("logical"),
    logical = { attributes(x) <- NULL; x },
    integer = as_base_type(x, as.logical),
    double = as_integerish_type(x, as.logical, "logical")
  )
}
#' @rdname vector-coercion
#' @export
as_integer <- function(x) {
  signal_deprecated_cast("as_integer")
  coerce_type_vec(x, friendly_type("integer"),
    logical = as_base_type(x, as.integer),
    integer = { attributes(x) <- NULL; x },
    double = as_integerish_type(x, as.integer, "integer")
  )
}
#' @rdname vector-coercion
#' @export
as_double <- function(x) {
  signal_deprecated_cast("as_double")
  coerce_type_vec(x, friendly_type("double"),
    logical = ,
    integer = as_base_type(x, as.double),
    double = { attributes(x) <- NULL; x }
  )
}
#' @rdname vector-coercion
#' @export
as_complex <- function(x) {
  signal_deprecated_cast("as_complex")
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
  signal_deprecated_cast("as_character")
  if (is_unspecified(x)) {
    return(rep_along(x, na_chr))
  }
  coerce_type_vec(x, friendly_type("character"),
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

is_unspecified <- function(x) {
  is_logical(x) && all(map_lgl(x, identical, NA))
}

as_base_type <- function(x, as_type) {
  # Zap attributes temporarily instead of unclassing. We want to avoid
  # method dispatch, but we also want to avoid an extra copy of atomic
  # vectors: the first when unclassing, the second when coercing. This
  # is also useful for uncopyable types like environments.
  attrs <- .Call(rlang_attrib, x)
  .Call(rlang_poke_attrib, x, NULL)

  # This function assumes that the target type is different than the
  # input type, otherwise no duplication is done and the output will
  # be modified by side effect when we restore the input attributes.
  on.exit(.Call(rlang_poke_attrib, x, attrs))

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
  out <- switch(type_of_(.x), ..., abort_coercion(.x, .to))

  if (!is_null(names(.x))) {
    # Avoid a copy of `out` when we restore the names, since it could be
    # a heavy atomic vector. We own `out`, so it is ok to change its
    # attributes inplace.
    .Call(rlang_poke_attrib, out, pairlist(names = names(.x)))
  }

  out
}
vec_coerce <- function(x, type) {
  .Call(rlang_vec_coerce, x, type)
}


#  Stack and frames  -------------------------------------------------

#' Get caller frame
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
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
  envs <- pluck(stack, "env")

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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
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

#' Parse text into a quosure
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' These functions were deprecated and renamed to [parse_quo()]
#' and [parse_quos()] in rlang 0.2.0. This is for consistency with the
#' convention that suffixes indicating return types are not
#' abbreviated.
#'
#' @inheritParams parse_expr
#' @keywords internal
#' @export
parse_quosure <- function(x, env = caller_env()) {
  warn_deprecated(paste_line(
    "`parse_quosure()` is deprecated as of rlang 0.2.0.",
    "Please use `parse_quo()` instead."
  ))
  parse_quo(x, env = env)
}
#' @rdname parse_quosure
#' @export
parse_quosures <- function(x, env = caller_env()) {
  warn_deprecated(paste_line(
    "`parse_quosures()` is deprecated as of rlang 0.2.0.",
    "Please use `parse_quos()` instead."
  ))
  parse_quos(x, env = env)
}

#' Squash a quosure
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' This function is deprecated, please use [quo_squash()] instead.
#'
#' @inheritParams quo_squash
#' @keywords internal
#' @export
quo_expr <- function(quo, warn = FALSE) {
  warn_deprecated(paste_line(
    "`quo_expr()` is deprecated as of rlang 0.2.0.",
    "Please use `quo_squash()` instead."
  ))
  quo_squash(quo, warn = warn)
}

#' Create an overscope
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' These functions have been deprecated in rlang 0.2.0. Please use
#' [as_data_mask()] and [new_data_mask()] instead. We no longer
#' require the mask to be cleaned up so `overscope_clean()` does not
#' have a replacement.
#'
#' @inheritParams as_data_mask
#' @param quo A [quosure][nse-defuse].
#'
#' @keywords internal
#' @export
as_overscope <- function(quo, data = NULL) {
  warn_deprecated(paste_line(
    "`as_overscope()` is deprecated as of rlang 0.2.0.",
    "Please use `as_data_mask()` instead."
  ))
  as_data_mask(data)
}
#' @rdname as_overscope
#' @param enclosure The `parent` argument of [new_data_mask()].
#' @export
new_overscope <- function(bottom, top = NULL, enclosure = NULL) {
  warn_deprecated(paste_line(
    "`new_overscope()` is deprecated as of rlang 0.2.0.",
    "Please use `new_data_mask()` instead."
  ))
  new_data_mask(bottom, top)
}
#' @rdname as_overscope
#' @param overscope A data mask.
#' @export
overscope_clean <- function(overscope) {
  warn_deprecated("`overscope_clean()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_data_mask_clean, overscope))
}

#' Evaluate next quosure in a data mask
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' `overscope_eval_next()` is deprecated as of rlang 0.2.0. Please use
#' `eval_tidy()` to which you can now supply an overscope.
#'
#' @param quo A quosure.
#' @param overscope A valid overscope containing bindings for `~`,
#'   `.top_env` and `_F` and whose parents contain overscoped bindings
#'   for tidy evaluation.
#' @param env The lexical enclosure in case `quo` is not a validly
#'   scoped quosure. This is the [base environment][base_env] by
#'   default.
#'
#' @keywords internal
#' @export
overscope_eval_next <- function(overscope, quo, env = base_env()) {
  warn_deprecated(paste_line(
    "`overscope_eval_next()` is deprecated as of rlang 0.2.0.",
    "Please use `eval_tidy()` with a data mask instead."
  ))
  .Call(rlang_eval_tidy, quo, overscope, env)
}


#  Expressions  ------------------------------------------------------

#' Create a call
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' These functions are deprecated, please use [call2()] and
#' [new_call()] instead.
#'
#' @inheritParams call2
#' @keywords internal
#' @export
lang <- function(.fn, ..., .ns = NULL) {
  warn_deprecated(paste_line(
    "`lang()` is deprecated as of rlang 0.2.0.",
    "Please use `call2()` instead."
  ))
  call2(.fn, ..., .ns = .ns)
}
#' @rdname lang
#' @inheritParams new_call
#' @export
new_language <- function(head, tail = NULL) {
  warn_deprecated(paste_line(
    "`new_language()` is deprecated as of rlang 0.2.0.",
    "Please use `new_call()` instead."
  ))
  new_call(head, tail)
}

#' Is object a call?
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' These functions are deprecated, please use [is_call()] and its `n`
#' argument instead.
#'
#' @inheritParams is_call
#' @keywords internal
#' @export
is_lang <- function(x, name = NULL, n = NULL, ns = NULL) {
  warn_deprecated(paste_line(
    "`is_lang()` is deprecated as of rlang 0.2.0.",
    "Please use `is_call()` instead."
  ))
  is_call(x, name, n, ns)
}
#' @rdname is_lang
#' @export
is_unary_lang <- function(x, name = NULL, ns = NULL) {
  warn_deprecated(paste_line(
    "`is_unary_lang()` is deprecated as of rlang 0.2.0.",
    "Please use `is_call()` instead."
  ))
  is_call(x, name, n = 1L, ns = ns)
}
#' @rdname is_lang
#' @export
is_binary_lang <- function(x, name = NULL, ns = NULL) {
  warn_deprecated(paste_line(
    "`is_binary_lang()` is deprecated as of rlang 0.2.0.",
    "Please use `is_call()` instead."
  ))
  is_call(x, name, n = 2L, ns = ns)
}
#' @rdname is_lang
#' @param quo A quosure to test.
#' @export
quo_is_lang <- function(quo) {
  warn_deprecated(paste_line(
    "`quo_is_lang()` is deprecated as of rlang 0.2.0.",
    "Please use `quo_is_call()` instead."
  ))
  .Call(rlang_quo_is_call, quo)
}

#' Manipulate or access a call
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' These functions are deprecated, please use [call_modify()],
#' [call_standardise()], or [call_fn()] instead.
#'
#' @inheritParams call_modify
#' @param lang,.lang The `call` or `.call` argument of the renamed
#'   functions.
#' @keywords internal
#' @export
lang_modify <- function(.lang, ..., .standardise = FALSE) {
  warn_deprecated(paste_line(
    "`lang_modify()` is deprecated as of rlang 0.2.0.",
    "Please use `call_modify()` instead."
  ))
  if (.standardise) {
    .lang <- call_standardise(.lang, caller_env())
  }
  call_modify(.lang, ...)
}
#' @rdname lang_modify
#' @export
lang_standardise <- function(lang) {
  warn_deprecated(paste_line(
    "`lang_standardise()` is deprecated as of rlang 0.2.0.",
    "Please use `call_standardise()` instead."
  ))
  call_standardise(lang, env = caller_env())
}
#' @rdname lang_modify
#' @export
lang_fn <- function(lang) {
  warn_deprecated(paste_line(
    "`lang_fn()` is deprecated as of rlang 0.2.0.",
    "Please use `call_fn()` instead."
  ))
  call_fn(lang, caller_env())
}
#' @rdname lang_modify
#' @export
lang_name <- function(lang) {
  warn_deprecated(paste_line(
    "`lang_name()` is deprecated as of rlang 0.2.0.",
    "Please use `call_name()` instead."
  ))
  call_name(lang)
}
#' @rdname lang_modify
#' @export
lang_args <- function(lang) {
  warn_deprecated(paste_line(
    "`lang_args()` is deprecated as of rlang 0.2.0.",
    "Please use `call_args()` instead."
  ))
  call_args(lang)
}
#' @rdname lang_modify
#' @export
lang_args_names <- function(lang) {
  warn_deprecated(paste_line(
    "`lang_args_names()` is deprecated as of rlang 0.2.0.",
    "Please use `call_args_names()` instead."
  ))
  call_args_names(lang)
}


#' Return the head or tail of a call
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' As of rlang 0.2.0 these functions are retired (deprecated for now)
#' because they are low level accessors that are rarely needed for end
#' users.
#'
#' @param lang A call.
#' @keywords internal
#' @export
lang_head <- function(lang) {
  warn_deprecated("`lang_head()` is deprecated as of rlang 0.2.0.")
  call <- get_expr(lang)
  stopifnot(is_call(call))
  node_car(call)
}
#' @rdname lang_head
#' @export
lang_tail <- function(lang) {
  warn_deprecated("`lang_tail()` is deprecated as of rlang 0.2.0.")
  call <- get_expr(lang)
  stopifnot(is_call(call))
  node_cdr(call)
}

#' Is an object an expression?
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' This function was deprecated and renamed to [is_expression()] in
#' rlang 0.2.0. This is for consistency with other type predicates
#' which are not abbreviated.
#'
#' @inheritParams is_expression
#' @keywords internal
#' @export
is_expr <- function(x) {
  warn_deprecated(paste_line(
    "`is_expr()` is deprecated as of rlang 0.2.0.",
    "Please use `is_expression()` instead."
  ))
  is_expression(x)
}


#  Nodes  ------------------------------------------------------------

#' Mutate node components
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
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
  invisible(.Call(rlang_node_poke_car, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cdr <- function(x, newcdr) {
  warn_deprecated("`mut_node_cdr()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cdr, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_caar <- function(x, newcar) {
  warn_deprecated("`mut_node_caar()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_caar, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cadr <- function(x, newcar) {
  warn_deprecated("`mut_node_cadr()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cadr, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cdar <- function(x, newcdr) {
  warn_deprecated("`mut_node_cdar()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cdar, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_cddr <- function(x, newcdr) {
  warn_deprecated("`mut_node_cddr()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cddr, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_tag <- function(x, newtag) {
  warn_deprecated("`mut_node_tag()` is deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_tag, x, newtag))
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

#' Coerce to an environment
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' This function is deprecated as it was renamed to [as_environment()]
#' in rlang 0.2.0.
#'
#' @keywords internal
#' @export
as_env <- function(x, parent = NULL) {
  warn_deprecated("`as_env()` is deprecated as of rlang 0.2.0.")
  as_environment(x, parent)
}

#' Is an object an environment?
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' These functions were deprecated and renamed to [is_environment()]
#' and [is_bare_environment()] in rlang 0.2.0. This is for consistency
#' with other type predicates which are not abbreviated.
#'
#' @inheritParams is_environment
#' @keywords internal
#' @export
is_env <- function(x) {
  warn_deprecated(paste_line(
    "`is_env()` is deprecated as of rlang 0.2.0.",
    "Please use `is_environment()` instead."
  ))
  is_environment(x)
}
#' @rdname is_env
#' @export
is_bare_env <- function(x) {
  warn_deprecated(paste_line(
    "`is_bare_env()` is deprecated as of rlang 0.2.0.",
    "Please use `is_bare_environment()` instead."
  ))
  is_bare_environment(x)
}

#' Bind a promise or active binding
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' These functions were deprecated and renamed with `new_` prefix in
#' rlang 0.2.0. This is for consistency with other non-variadic object
#' constructors.
#'
#' @param .x A vector.
#' @inheritParams new-vector
#' @inheritParams new-vector-along-retired
#' @name vector-old-ctors
#' @keywords internal
NULL

#' @rdname vector-old-ctors
#' @export
lgl_len <- function(.n) {
  warn_deprecated(paste_line(
    "`lgl_len()` is deprecated as of rlang 0.2.0.",
    "Please use `new_logical()` instead."
  ))
  new_logical(.n)
}
#' @rdname vector-old-ctors
#' @export
int_len <- function(.n) {
  warn_deprecated(paste_line(
    "`int_len()` is deprecated as of rlang 0.2.0.",
    "Please use `new_integer()` instead."
  ))
  new_integer(.n)
}
#' @rdname vector-old-ctors
#' @export
dbl_len <- function(.n) {
  warn_deprecated(paste_line(
    "`dbl_len()` is deprecated as of rlang 0.2.0.",
    "Please use `new_double()` instead."
  ))
  new_double(.n)
}
#' @rdname vector-old-ctors
#' @export
chr_len <- function(.n) {
  warn_deprecated(paste_line(
    "`chr_len()` is deprecated as of rlang 0.2.0.",
    "Please use `new_character()` instead."
  ))
  new_character(.n)
}
#' @rdname vector-old-ctors
#' @export
cpl_len <- function(.n) {
  warn_deprecated(paste_line(
    "`cpl_len()` is deprecated as of rlang 0.2.0.",
    "Please use `new_complex()` instead."
  ))
  new_complex(.n)
}
#' @rdname vector-old-ctors
#' @export
raw_len <- function(.n) {
  warn_deprecated(paste_line(
    "`raw_len()` is deprecated as of rlang 0.2.0.",
    "Please use `new_raw()` instead."
  ))
  new_raw(.n)
}
#' @rdname vector-old-ctors
#' @export
bytes_len <- function(.n) {
  warn_deprecated(paste_line(
    "`bytes_len()` is deprecated as of rlang 0.2.0.",
    "Please use `new_raw()` instead."
  ))
  new_raw(.n)
}
#' @rdname vector-old-ctors
#' @export
list_len <- function(.n) {
  warn_deprecated(paste_line(
    "`list_len()` is deprecated as of rlang 0.2.0.",
    "Please use `new_list()` instead."
  ))
  new_list(.n)
}

#' @rdname vector-old-ctors
#' @export
lgl_along <- function(.x) {
  warn_deprecated("`lgl_along()` is deprecated as of rlang 0.2.0.")
  local_options(lifecycle_disable_warnings = TRUE)
  new_logical_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
int_along <- function(.x) {
  warn_deprecated("`int_along()` is deprecated as of rlang 0.2.0.")
  local_options(lifecycle_disable_warnings = TRUE)
  new_integer_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
dbl_along <- function(.x) {
  warn_deprecated("`dbl_along()` is deprecated as of rlang 0.2.0.")
  local_options(lifecycle_disable_warnings = TRUE)
  new_double_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
chr_along <- function(.x) {
  warn_deprecated("`chr_along()` is deprecated as of rlang 0.2.0.")
  local_options(lifecycle_disable_warnings = TRUE)
  new_character_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
cpl_along <- function(.x) {
  warn_deprecated("`cpl_along()` is deprecated as of rlang 0.2.0.")
  local_options(lifecycle_disable_warnings = TRUE)
  new_complex_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
raw_along <- function(.x) {
  warn_deprecated("`raw_along()` is deprecated as of rlang 0.2.0.")
  local_options(lifecycle_disable_warnings = TRUE)
  new_raw_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
bytes_along <- function(.x) {
  warn_deprecated("`bytes_along()` is deprecated as of rlang 0.2.0.")
  local_options(lifecycle_disable_warnings = TRUE)
  new_raw_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
list_along <- function(.x) {
  warn_deprecated("`list_along()` is deprecated as of rlang 0.2.0.")
  local_options(lifecycle_disable_warnings = TRUE)
  new_list_along(.x, NULL)
}

#' Create vectors matching the length of a given vector
#'
#' These functions are deprecated as of rlang 0.3.0 because they
#' are longer to type than the equivalent [rep_along()] or
#' [rep_named()] calls without added clarity.
#'
#' @param x A vector.
#' @param names Names for the new vector.
#' @name new-vector-along-retired
#' @keywords internal

#' @export
#' @rdname new-vector-along-retired
new_logical_along <- function(x, names = base::names(x)) {
  warn_deprecated_along("logical", "NA")
  set_names_impl(rep_len(na_lgl, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_integer_along <- function(x, names = base::names(x)) {
  warn_deprecated_along("integer", "na_int")
  set_names_impl(rep_len(na_int, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_double_along <- function(x, names = base::names(x)) {
  warn_deprecated_along("double", "na_dbl")
  set_names_impl(rep_len(na_dbl, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_character_along <- function(x, names = base::names(x)) {
  warn_deprecated_along("character", "na_chr")
  set_names_impl(rep_len(na_chr, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_complex_along <- function(x, names = base::names(x)) {
  warn_deprecated_along("complex", "na_cpl")
  set_names_impl(rep_len(na_cpl, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_raw_along <- function(x, names = base::names(x)) {
  warn_deprecated_along("raw", "new_raw(1)")
  set_names_impl(vector("raw", length(x)), x, names)
}
#' @export
#' @rdname new-vector-along-retired
new_list_along <- function(x, names = base::names(x)) {
  warn_deprecated_along("list", "list(NULL)")
  set_names_impl(vector("list", length(x)), x, names)
}
warn_deprecated_along <- function(type, na) {
  warn_deprecated(paste_line(
    sprintf("`new_%s_along()` is deprecated as of rlang 0.3.0.", type),
    sprintf("Please use `rep_along(x, %s)` or `rep_named(nms, %s)` instead.", na, na)
  ))
}
# FIXME: This can be simplified once the `_along` ctors are defunct
set_names_impl <- function(x, mold, nm, ...) {
  .Call(rlang_set_names, x, mold, nm, environment())
}

#' Prepend a vector
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' Vector functions are now out of scope for rlang. They might be
#' revived in the vctrs or funs packages.
#'
#'
#' @keywords internal
#'
#' @param x the vector to be modified.
#' @param values to be included in the modified vector.
#' @param before a subscript, before which the values are to be appended.
#' @export
prepend <- function(x, values, before = 1) {
  warn_deprecated_vector("prepend")

  n <- length(x)
  stopifnot(before > 0 && before <= n)

  if (before == 1) {
    c(values, x)
  } else {
    c(x[1:(before - 1)], values, x[before:n])
  }
}

#' @rdname prepend
#' @param .x A vector to modify.
#' @param ... <[dynamic][dyn-dots]> List of elements to merge into
#'   `.x`. Named elements already existing in `.x` are used as
#'   replacements. Elements that have new or no names are inserted at
#'   the end.
#' @export
modify <- function(.x, ...) {
  warn_deprecated_vector("modify")

  out <- as.list(.x)
  args <- list2(...)

  args_nms <- names(args)
  exists <- have_name(args) & args_nms %in% names(out)

  for (nm in args_nms[exists]) {
    out[[nm]] <- args[[nm]]
  }

  c(out, args[!exists])
}

warn_deprecated_vector <- function(fn) {
  warn_deprecated(paste_line(
    sprintf("`%s()` is deprecated as of rlang 0.4.0.", fn),
    "",
    "Vector tools are now out of scope for rlang to make it a more",
    "focused package."
  ))
}



#  Attributes  -------------------------------------------------------

#' Add attributes to an object
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
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


#  Conditions --------------------------------------------------------

#' Exiting handler
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' These functions have been renamed to use the conventional `local_`
#' prefix. They will be deprecated in the next minor version of rlang.
#'
#' @inheritParams local_interactive
#' @inheritParams local_options
#' @inheritParams local_bindings
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("superseded")}
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
