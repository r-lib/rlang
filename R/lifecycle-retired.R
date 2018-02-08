
signal_soft_deprecation <- function(msg) {
  if (is_true(peek_option("lifecycle_force_verbose_retirement"))) {
    warn(msg)
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
  signal_soft_deprecation(
    "`parse_quosure()` is soft-deprecated as of rlang 0.2.0. Please use `parse_quo()` instead."
  )
  parse_quo(x, env = env)
}
#' @rdname parse_expr
#' @export
parse_quosures <- function(x, env = caller_env()) {
  signal_soft_deprecation(
    "`parse_quosures()` is soft-deprecated as of rlang 0.2.0. Please use `parse_quos()` instead."
  )
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
  quo_squash(quo, warn = warn)
}

#' Create a call
#'
#' This function is soft-deprecated, please use [call2()] instead.
#'
#' @inheritParams call2
#' @keywords internal
#' @export
lang <- function(.fn, ..., .ns = NULL) {
  call2(.fn, ..., .ns = .ns)
}
#' @rdname lang
#' @inheritParams new_call
#' @export
new_language <- function(head, tail = NULL) {
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
  is_call(x, name, n, ns)
}
#' @rdname is_lang
#' @export
is_unary_lang <- function(x, name = NULL, ns = NULL) {
  is_call(x, name, n = 1L, ns = ns)
}
#' @rdname is_lang
#' @export
is_binary_lang <- function(x, name = NULL, ns = NULL) {
  is_call(x, name, n = 2L, ns = ns)
}
#' @rdname is_lang
#' @param quo A quosure to test.
#' @export
quo_is_lang <- function(quo) {
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
  call_modify(.lang, ..., .standardise = .standardise, .env = caller_env())
}
#' @rdname lang_modify
#' @export
lang_standardise <- function(lang) {
  call_standardise(lang, env = caller_env())
}
#' @rdname lang_modify
#' @export
lang_fn <- function(lang) {
  call_fn(lang, caller_env())
}
#' @rdname lang_modify
#' @export
lang_name <- function(lang) {
  call_name(lang)
}
#' @rdname lang_modify
#' @export
lang_args <- function(lang) {
  call_args(lang)
}
#' @rdname lang_modify
#' @export
lang_args_names <- function(lang) {
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
  call <- get_expr(lang)
  stopifnot(is_call(call))
  node_car(call)
}
#' @rdname lang_head
#' @export
lang_tail <- function(lang) {
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
  as_data_mask(data, quo_get_env(quo))
}
#' @rdname as_overscope
#' @param enclosure The `parent` argument of [new_data_mask()].
#' @export
new_overscope <- function(bottom, top = NULL, enclosure = base_env()) {
  new_data_mask(bottom, top, enclosure)
}
#' @rdname as_overscope
#' @param overscope A data mask.
#' @export
overscope_clean <- function(overscope) {
  invisible(.Call(rlang_data_mask_clean, overscope))
}

#' Tidy evaluation in a custom environment
#'
#' This function is soft-deprecated as of rlang 0.2.0.
#'
#' @inheritParams eval_tidy
#' @inheritParams as_data_mask
#'
#' @keywords internal
#' @export
eval_tidy_ <- function(expr, bottom, top = NULL, env = caller_env()) {
  data_mask <- new_overscope(bottom, top %||% bottom)
  on.exit(overscope_clean(data_mask))
  .Call(rlang_eval_tidy, expr, data_mask, environment())
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
  invisible(.Call(rlang_node_poke_car, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cdr <- function(x, newcdr) {
  invisible(.Call(rlang_node_poke_cdr, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_caar <- function(x, newcar) {
  invisible(.Call(rlang_node_poke_caar, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cadr <- function(x, newcar) {
  invisible(.Call(rlang_node_poke_cadr, x, newcar))
}
#' @rdname mut_node_car
#' @export
mut_node_cdar <- function(x, newcdr) {
  invisible(.Call(rlang_node_poke_cdar, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_cddr <- function(x, newcdr) {
  invisible(.Call(rlang_node_poke_cddr, x, newcdr))
}
#' @rdname mut_node_car
#' @export
mut_node_tag <- function(x, newtag) {
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
  is_expression(x)
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

#' Deprecated condition constructors
#'
#' These functions were deprecated in rlang 0.2.0 to follow the
#' convention that return types are indicated as suffixes. Please use
#' [cnd()], [error_cnd()], [warning_cnd()] and [message_cnd()]
#' instead.
#'
#' @inheritParams cnd
#' @name deprecated-cnd
#' @keywords internal
#' @export
new_cnd <- function(.type = NULL, ..., .msg = NULL) {
  # Deprecated in 0.1.2
  warning("`new_cnd()` has been renamed to `cnd()` for consistency",
    call. = FALSE)
  cnd(.type = .type, ..., .msg = .msg)
}
#' @rdname deprecated-cnd
#' @export
cnd_error <- function(.type = NULL, ..., .msg = NULL) {
  # Deprecated in 0.1.2
  warning("`cnd_error()` has been renamed to `error_cnd()` for consistency",
    call. = FALSE)
  error_cnd(.type = .type, ..., .msg = .msg)
}
#' @rdname deprecated-cnd
#' @export
cnd_warning <- function(.type = NULL, ..., .msg = NULL) {
  # Deprecated in 0.1.2
  warning("`cnd_warning()` has been renamed to `warning_cnd()` for consistency",
    call. = FALSE)
  warning_cnd(.type = .type, ..., .msg = .msg)
}
#' @rdname deprecated-cnd
#' @export
cnd_message <- function(.type = NULL, ..., .msg = NULL) {
  # Deprecated in 0.1.2
  warning("`cnd_message()` has been renamed to `message_cnd()` for consistency",
    call. = FALSE)
  message_cnd(.type = .type, ..., .msg = .msg)
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
  new_logical(.n)
}
#' @rdname vector-old-ctors
#' @export
int_len <- function(.n) {
  new_integer(.n)
}
#' @rdname vector-old-ctors
#' @export
dbl_len <- function(.n) {
  new_double(.n)
}
#' @rdname vector-old-ctors
#' @export
chr_len <- function(.n) {
  new_character(.n)
}
#' @rdname vector-old-ctors
#' @export
cpl_len <- function(.n) {
  new_complex(.n)
}
#' @rdname vector-old-ctors
#' @export
raw_len <- function(.n) {
  new_raw(.n)
}
#' @rdname vector-old-ctors
#' @export
bytes_len <- function(.n) {
  new_raw(.n)
}
#' @rdname vector-old-ctors
#' @export
list_len <- function(.n) {
  new_list(.n)
}

#' @rdname vector-old-ctors
#' @export
lgl_along <- function(.x) {
  new_logical_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
int_along <- function(.x) {
  new_integer_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
dbl_along <- function(.x) {
  new_double_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
chr_along <- function(.x) {
  new_character_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
cpl_along <- function(.x) {
  new_complex_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
raw_along <- function(.x) {
  new_raw_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
bytes_along <- function(.x) {
  new_raw_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
list_along <- function(.x) {
  new_list_along(.x, NULL)
}
#' @rdname vector-old-ctors
#' @export
node <- function(car, cdr = NULL) {
  new_node(car, cdr)
}
