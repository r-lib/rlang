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
#'
#' * [set_names()]
#' * [as_function()]
#'
#'
#' @section Experimental functions:
#'
#' * [type_of()], [switch_type()], [coerce_type()]
#' * [switch_class()], [coerce_class()]
#' * [lang_type_of()], [switch_lang()], [coerce_lang()]
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
