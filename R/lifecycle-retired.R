
signal_soft_deprecated <- function(msg) {
  if (is_true(peek_option("lifecycle_force_verbose_retirement"))) {
    warn_deprecated_once(msg)
  }
  invisible(NULL)
}


# Soft-deprecated ----------------------------------------------------

#' Parse text into a quosure
#'
#' These functions were soft-deprecated and renamed to [parse_quo()]
#' and [parse_quos()] in rlang 0.2.0. This is for consistency with the
#' convention that suffixes indicating return types are not
#' abbreviated.
#'
#' @inheritParams parse_expr
#' @keywords internal
#' @export
parse_quosure <- function(x, env = caller_env()) {
  signal_soft_deprecated(paste_line(
    "`parse_quosure()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `parse_quo()` instead"
  ))
  parse_quo(x, env = env)
}
#' @rdname parse_expr
#' @export
parse_quosures <- function(x, env = caller_env()) {
  signal_soft_deprecated(paste_line(
    "`parse_quosures()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `parse_quos()` instead"
  ))
  parse_quos(x, env = env)
}

#' Squash a quosure
#'
#' This function is soft-deprecated, please use [quo_squash()] instead.
#'
#' @inheritParams quo_squash
#' @keywords internal
#' @export
quo_expr <- function(quo, warn = FALSE) {
  signal_soft_deprecated(paste_line(
    "`quo_expr()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `quo_squash()` instead"
  ))
  quo_squash(quo, warn = warn)
}

#' Create a call
#'
#' These functions are soft-deprecated, please use [call2()] and
#' [new_call()] instead.
#'
#' @inheritParams call2
#' @keywords internal
#' @export
lang <- function(.fn, ..., .ns = NULL) {
  signal_soft_deprecated(paste_line(
    "`lang()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call2()` instead"
  ))
  call2(.fn, ..., .ns = .ns)
}
#' @rdname lang
#' @inheritParams new_call
#' @export
new_language <- function(head, tail = NULL) {
  signal_soft_deprecated(paste_line(
    "`new_language()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_call()` instead"
  ))
  new_call(head, tail)
}

#' Is object a call?
#'
#' These functions are soft-deprecated, please use [is_call()] and its
#' `n` argument instead.
#'
#' @inheritParams is_call
#' @keywords internal
#' @export
is_lang <- function(x, name = NULL, n = NULL, ns = NULL) {
  signal_soft_deprecated(paste_line(
    "`is_lang()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_call()` instead"
  ))
  is_call(x, name, n, ns)
}
#' @rdname is_lang
#' @export
is_unary_lang <- function(x, name = NULL, ns = NULL) {
  signal_soft_deprecated(paste_line(
    "`is_unary_lang()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_call()` instead"
  ))
  is_call(x, name, n = 1L, ns = ns)
}
#' @rdname is_lang
#' @export
is_binary_lang <- function(x, name = NULL, ns = NULL) {
  signal_soft_deprecated(paste_line(
    "`is_binary_lang()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_call()` instead"
  ))
  is_call(x, name, n = 2L, ns = ns)
}
#' @rdname is_lang
#' @param quo A quosure to test.
#' @export
quo_is_lang <- function(quo) {
  signal_soft_deprecated(paste_line(
    "`quo_is_lang()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `quo_is_call()` instead"
  ))
  .Call(rlang_quo_is_call, quo)
}

#' Manipulate or access a call
#'
#' These functions are soft-deprecated, please use [call_modify()],
#' [call_standardise()], or [call_fn()] instead.
#'
#' @inheritParams call_modify
#' @param lang,.lang The `call` or `.call` argument of the renamed
#'   functions.
#' @keywords internal
#' @export
lang_modify <- function(.lang, ..., .standardise = FALSE) {
  signal_soft_deprecated(paste_line(
    "`lang_modify()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_modify()` instead"
  ))
  call_modify(.lang, ..., .standardise = .standardise, .env = caller_env())
}
#' @rdname lang_modify
#' @export
lang_standardise <- function(lang) {
  signal_soft_deprecated(paste_line(
    "`lang_standardise()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_standardise()` instead"
  ))
  call_standardise(lang, env = caller_env())
}
#' @rdname lang_modify
#' @export
lang_fn <- function(lang) {
  signal_soft_deprecated(paste_line(
    "`lang_fn()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_fn()` instead"
  ))
  call_fn(lang, caller_env())
}
#' @rdname lang_modify
#' @export
lang_name <- function(lang) {
  signal_soft_deprecated(paste_line(
    "`lang_name()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_name()` instead"
  ))
  call_name(lang)
}
#' @rdname lang_modify
#' @export
lang_args <- function(lang) {
  signal_soft_deprecated(paste_line(
    "`lang_args()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_args()` instead"
  ))
  call_args(lang)
}
#' @rdname lang_modify
#' @export
lang_args_names <- function(lang) {
  signal_soft_deprecated(paste_line(
    "`lang_args_names()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `call_args_names()` instead"
  ))
  call_args_names(lang)
}


#' Return the head or tail of a call
#'
#' As of rlang 0.2.0 these functions are retired (soft-deprecated for
#' now) because they are low level accessors that are rarely needed
#' for end users.
#'
#' @param lang A call.
#' @export
lang_head <- function(lang) {
  signal_soft_deprecated("`lang_head()` is soft-deprecated as of rlang 0.2.0.")
  call <- get_expr(lang)
  stopifnot(is_call(call))
  node_car(call)
}
#' @rdname lang_head
#' @export
lang_tail <- function(lang) {
  signal_soft_deprecated("`lang_tail()` is soft-deprecated as of rlang 0.2.0.")
  call <- get_expr(lang)
  stopifnot(is_call(call))
  node_cdr(call)
}

#' Create an overscope
#'
#' These functions have been soft-deprecated in rlang 0.2.0. Please
#' use [as_data_mask()] and [new_data_mask()] instead. We no longer
#' require the mask to be cleaned up so `overscope_clean()` does not
#' have a replacement.
#'
#' @inheritParams as_data_mask
#' @param quo A [quosure][quotation].
#'
#' @keywords internal
#' @export
as_overscope <- function(quo, data = NULL) {
  signal_soft_deprecated(paste_line(
    "`as_overscope()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `as_data_mask()` instead"
  ))
  as_data_mask(data, quo_get_env(quo))
}
#' @rdname as_overscope
#' @param enclosure The `parent` argument of [new_data_mask()].
#' @export
new_overscope <- function(bottom, top = NULL, enclosure = NULL) {
  signal_soft_deprecated(paste_line(
    "`new_overscope()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_data_mask()` instead"
  ))
  new_data_mask(bottom, top, enclosure)
}
#' @rdname as_overscope
#' @param overscope A data mask.
#' @export
overscope_clean <- function(overscope) {
  signal_soft_deprecated("`overscope_clean()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_data_mask_clean, overscope))
}

#' Tidy evaluation in a custom environment
#'
#' This function is defunct as of rlang 0.3.0.
#'
#' @inheritParams eval_tidy
#' @inheritParams as_data_mask
#'
#' @keywords internal
#' @export
eval_tidy_ <- function(expr, bottom, top = NULL, env = caller_env()) {
  abort_defunct("`eval_tidy_()` is defunct as of rlang 0.3.0. Use `eval_tidy()` instead.")
}
#' Evaluate next quosure in a data mask
#'
#' `overscope_eval_next()` is soft-deprecated as of rlang
#' 0.2.0. Please use `eval_tidy()` to which you can now supply an
#' overscope.
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
  signal_soft_deprecated(paste_line(
    "`overscope_eval_next()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `eval_tidy()` with a data mask instead"
  ))
  .Call(rlang_eval_tidy, quo, overscope, environment())
}


#' Create a dictionary
#'
#' The dictionary class was soft-deprecated in rlang 0.2.0. It was
#' trying to be too general and did not prove useful. Please use
#' [as_data_pronoun()] or your own pronoun class instead.
#'
#' @param x An object for which you want to find associated data.
#' @param lookup_msg An error message when your data source is
#'   accessed inappropriately (by position rather than name).
#' @param read_only Whether users can replace elements of the
#'   dictionary.
#'
#' @name dictionary
#' @keywords internal
#' @export
as_dictionary <- function(x, lookup_msg = NULL, read_only = FALSE) {
  signal_soft_deprecated(paste_line(
    "`as_dictionary()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `as_data_pronoun()` instead"
  ))
  UseMethod("as_dictionary")
}
#' @export
as_dictionary.default <- function(x, lookup_msg = NULL, read_only = FALSE) {
  x <- discard_unnamed(x)
  check_dictionaryish(x)
  new_dictionary(as.list(x), lookup_msg, read_only)
}
#' @export
as_dictionary.dictionary <- function(x, lookup_msg = NULL, read_only = FALSE) {
  dict <- unclass_data_pronoun(x)
  dict$lookup_msg <- lookup_msg %||% x$lookup_msg
  dict$read_only <- read_only
  set_attrs(dict, class = class(x))
}
#' @export
as_dictionary.NULL <- function(x, lookup_msg = NULL, read_only = FALSE) {
  new_dictionary(list(), lookup_msg, read_only)
}
#' @export
as_dictionary.environment <- function(x, lookup_msg = NULL, read_only = FALSE) {
  lookup_msg <- lookup_msg %||% "Object `%s` not found in environment"
  new_dictionary(x, lookup_msg, read_only)
}
#' @export
as_dictionary.data.frame <- function(x, lookup_msg = NULL, read_only = FALSE) {
  check_dictionaryish(x)
  lookup_msg <- lookup_msg %||% "Column `%s` not found in data"
  new_dictionary(x, lookup_msg, read_only)
}

check_dictionaryish <- function(x) {
  if (!length(x)) {
    return(NULL)
  }
  if (!is_named(x)) {
    abort("Data must be uniquely named but some variables are unnamed")
  }
  nms <- names(x)
  dups <- duplicated(nms)
  if (any(dups)) {
    dups <- unique(nms[dups])
    dups <- chr_enumerate(chr_quoted(dups), final = "and")
    abort(paste0(
      "Data must be uniquely named but the following variables have duplicates: ", dups
    ))
  }
}
new_dictionary <- function(x, lookup_msg, read_only) {
  .Call(rlang_new_data_pronoun, x, lookup_msg, read_only)
}

#' @rdname dictionary
#' @export
is_dictionary <- function(x) {
  signal_soft_deprecated("`is_dictionary()` is soft-deprecated as of rlang 0.2.0.")
  inherits(x, "rlang_data_pronoun")
}

#' Coerce to an environment
#'
#' This function is soft-deprecated as it was renamed to
#' [as_environment()] in rlang 0.2.0.
#'
#' @keywords internal
#' @export
as_env <- function(x, parent = NULL) {
  signal_soft_deprecated("`is_dictionary()` is soft-deprecated as of rlang 0.2.0.")
  as_environment(x, parent)
}

#' Mutate node components
#'
#' These functions were soft-deprecated and renamed with `node_poke_`
#' prefix in rlang 0.2.0. This change follows a new naming convention
#' where mutation is referred to as "poking".
#'
#' @inheritParams new_node
#'
#' @keywords internal
#' @export
mut_node_car <- function(x, newcar) {
  signal_soft_deprecated("`mut_node_car()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_car, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cdr <- function(x, newcdr) {
  signal_soft_deprecated("`mut_node_cdr()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cdr, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_caar <- function(x, newcar) {
  signal_soft_deprecated("`mut_node_caar()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_caar, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cadr <- function(x, newcar) {
  signal_soft_deprecated("`mut_node_cadr()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cadr, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cdar <- function(x, newcdr) {
  signal_soft_deprecated("`mut_node_cdar()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cdar, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_cddr <- function(x, newcdr) {
  signal_soft_deprecated("`mut_node_cddr()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_cddr, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_tag <- function(x, newtag) {
  signal_soft_deprecated("`mut_node_tag()` is soft-deprecated as of rlang 0.2.0.")
  invisible(.Call(rlang_node_poke_tag, x, newtag))
}

#' Is an object an expression?
#'
#' This function was soft-deprecated and renamed to [is_expression()]
#' in rlang 0.2.0. This is for consistency with other type predicates
#' which are not abbreviated.
#'
#' @inheritParams is_expression
#' @keywords internal
#' @export
is_expr <- function(x) {
  signal_soft_deprecated(paste_line(
    "`is_expr()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_expression()` instead"
  ))
  is_expression(x)
}

#' Is an object an environment?
#'
#' These functions were soft-deprecated and renamed to
#' [is_environment()] and [is_bare_environment()] in rlang 0.2.0. This
#' is for consistency with other type predicates which are not
#' abbreviated.
#'
#' @inheritParams is_environment
#' @keywords internal
#' @export
is_env <- function(x) {
  signal_soft_deprecated(paste_line(
    "`is_env()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_environment()` instead"
  ))
  is_environment(x)
}
#' @rdname is_env
#' @export
is_bare_env <- function(x) {
  signal_soft_deprecated(paste_line(
    "`is_bare_env()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `is_bare_environment()` instead"
  ))
  is_bare_environment(x)
}


# Deprecated ---------------------------------------------------------

#' Test for or coerce to quosure-like objects
#'
#' These functions are deprecated as of rlang 0.2.0 because they make
#' the assumption that quosures are a subtype of formula, which we are
#' now considering to be an implementation detail.
#'
#' @inheritParams is_formula
#' @inheritParams as_quosure
#'
#' @keywords internal
#' @export
is_quosureish <- function(x, scoped = NULL) {
  warn("`is_quosureish()` is deprecated as of rlang 0.2.0")
  is_formula(x, scoped = scoped, lhs = FALSE)
}
#' @rdname is_quosureish
#' @export
as_quosureish <- function(x, env = caller_env()) {
  warn("`as_quosureish()` is deprecated as of rlang 0.2.0")
  if (is_quosureish(x)) {
    if (!is_environment(get_env(x))) {
      set_env(x, env)
    }
    x
  } else if (is_frame(x)) {
    new_quosure(x$expr, sys_frame(x$caller_pos))
  } else {
    new_quosure(get_expr(x), get_env(x, env))
  }
}

#' Retired vector construction by length
#'
#' These functions were soft-deprecated and renamed with `new_` prefix
#' in rlang 0.2.0. This is for consistency with other non-variadic
#' object constructors.
#'
#' @inheritParams new-vector
#' @inheritParams new-vector-along
#' @name vector-old-ctors
#' @keywords internal
NULL

#' @rdname vector-old-ctors
#' @export
lgl_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`lgl_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_logical()` instead"
  ))
  new_logical(.n)
}
#' @rdname vector-old-ctors
#' @export
int_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`int_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_integer()` instead"
  ))
  new_integer(.n)
}
#' @rdname vector-old-ctors
#' @export
dbl_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`dbl_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_double()` instead"
  ))
  new_double(.n)
}
#' @rdname vector-old-ctors
#' @export
chr_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`chr_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_character()` instead"
  ))
  new_character(.n)
}
#' @rdname vector-old-ctors
#' @export
cpl_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`cpl_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_complex()` instead"
  ))
  new_complex(.n)
}
#' @rdname vector-old-ctors
#' @export
raw_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`raw_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_raw()` instead"
  ))
  new_raw(.n)
}
#' @rdname vector-old-ctors
#' @export
bytes_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`bytes_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_raw()` instead"
  ))
  new_raw(.n)
}
#' @rdname vector-old-ctors
#' @export
list_len <- function(.n) {
  signal_soft_deprecated(paste_line(
    "`list_len()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_list()` instead"
  ))
  new_list(.n)
}

#' @rdname vector-old-ctors
#' @export
lgl_along <- function(.x) {
  signal_soft_deprecated(paste_line(
    "`lgl_along()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_logical_along()` instead"
  ))
  new_logical_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
int_along <- function(.x) {
  signal_soft_deprecated(paste_line(
    "`int_along()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_integer_along()` instead"
  ))
  new_integer_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
dbl_along <- function(.x) {
  signal_soft_deprecated(paste_line(
    "`dbl_along()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_double_along()` instead"
  ))
  new_double_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
chr_along <- function(.x) {
  signal_soft_deprecated(paste_line(
    "`chr_along()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_character_along()` instead"
  ))
  new_character_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
cpl_along <- function(.x) {
  signal_soft_deprecated(paste_line(
    "`cpl_along()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_complex_along()` instead"
  ))
  new_complex_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
raw_along <- function(.x) {
  signal_soft_deprecated(paste_line(
    "`raw_along()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_raw_along()` instead"
  ))
  new_raw_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
bytes_along <- function(.x) {
  signal_soft_deprecated(paste_line(
    "`bytes_along()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_raw_along()` instead"
  ))
  new_raw_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
list_along <- function(.x) {
  signal_soft_deprecated(paste_line(
    "`list_along()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_list_along()` instead"
  ))
  new_list_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
node <- function(car, cdr = NULL) {
  signal_soft_deprecated(paste_line(
    "`node()` is soft-deprecated as of rlang 0.2.0.",
    "Please use `new_node()` instead"
  ))
  new_node(car, cdr)
}

#' Add attributes to an object
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
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
#' These functions are soft-deprecated since rlang 0.3.0.
#'
#' @param .x An object to decorate with attributes.
#' @param ... A list of named attributes. These have [explicit
#'   splicing semantics][tidy-dots]. Pass a single unnamed `NULL` argument to
#'   zap all attributes from `.x`.
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
  signal_soft_deprecated("`set_attrs()` is soft-deprecated as of rlang 0.3.0")

  if (!is_copyable(.x)) {
    abort("`.x` is uncopyable: use `mut_attrs()` to change attributes in place")
  }
  set_attrs_impl(.x, ...)
}
#' @rdname set_attrs
#' @export
mut_attrs <- function(.x, ...) {
  signal_soft_deprecated("`set_attrs()` is soft-deprecated as of rlang 0.3.0")

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
