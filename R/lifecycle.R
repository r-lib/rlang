#' Life cycle of the rlang package
#'
#' @description
#'
#' The rlang package is currently maturing. Unless otherwise stated,
#' this applies to all its exported functions. Maturing functions are
#' susceptible to API changes. Only use these in packages if you're
#' prepared to make changes as the package evolves. See sections below
#' for a list of functions marked as stable.
#'
#' The documentation pages of retired functions contain life cycle
#' sections that explain the reasons for their retirements.
#'
#'
#' @section Stable functions:
#'
#' TODO: Review all functions in rlang and mark stable ones as such.
#'
#' * [eval_tidy()]
#' * [!!], [!!!]
#' * [enquo()], [quo()], [quos()]
#' * [enexpr()], [expr()], [exprs()]
#' * [sym()], [syms()]
#' * [new_quosure()], [is_quosure()]
#'
#' * [quo_get_expr()], [quo_set_expr()]
#' * [quo_get_env()], [quo_set_env()]
#'
#' * [set_names()], [names2()]
#' * [as_function()]
#'
#'
#' @section Experimental functions:
#'
#' * [type_of()], [switch_type()], [coerce_type()]
#' * [switch_class()], [coerce_class()]
#' * [lang_type_of()], [switch_lang()], [coerce_lang()]
#' * [set_attrs()], [mut_attrs()]
#'
#'
#' @section Soft-deprecated functions and arguments:
#'
#' **rlang 0.2.0:**
#'
#' * [UQ()]
#' * [UQS()]
#' * [parse_quosure()]
#' * [parse_quosures()]
#' * [quo_expr()] => [quo_squash()]
#'
#' * [lang()] => [call2()]
#' * [new_language()] => [new_call()]
#' * [is_lang()] => [is_call()]
#' * [is_unary_lang()] => [is_unary_call()]
#' * [is_binary_lang()] => [is_binary_call()]
#'
#' * [lang_modify()] => [call_modify()]
#' * [lang_standardise()] => [call_standardise()]
#' * [lang_fn()] => [call_fn()]
#' * [lang_name()] => [call_name()]
#' * [lang_head()] => [call_head()]
#' * [lang_tail()] => [call_tail()]
#' * [lang_args()] => [call_args()]
#' * [lang_args_names()] => [call_args_names()]
#'
#'
#' @section Deprecated functions and arguments:
#'
#' **rlang 0.2.0:**
#'
#' * [UQE()]
#' * [is_quosureish()], [as_quosureish()]
#'
#'
#' @section Defunct functions and arguments:
#'
#' **rlang 0.2.0:**
#'
#' * [:=][quasiquotation]
#'
#' @name lifecycle
NULL
