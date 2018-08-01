#' Life cycle of the rlang package
#'
#' @description
#'
#' \badgematuring
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
#' \badgestable
#'
#' * [eval_tidy()]
#' * [!!], [!!!]
#' * [enquo()], [quo()], [quos()]
#' * [enexpr()], [expr()], [exprs()]
#' * [sym()], [syms()]
#' * [new_quosure()], [is_quosure()]
#' * [missing_arg()], [is_missing()]
#'
#' * [quo_get_expr()], [quo_set_expr()]
#' * [quo_get_env()], [quo_set_env()]
#'
#' * [eval_bare()]
#'
#' * [set_names()], [names2()]
#' * [as_function()]
#'
#'
#' @section Experimental functions:
#'
#' \badgeexperimental
#'
#' These functions are not yet part of the rlang API. Expect breaking
#' changes.
#'
#' * [type_of()], [switch_type()], [coerce_type()]
#' * [switch_class()], [coerce_class()]
#' * [lang_type_of()], [switch_lang()], [coerce_lang()]
#' * [set_attrs()], [mut_attrs()]
#' * [with_env()], [locally()]
#' * [env_poke()]
#'
#' * [env_bind_fns()], [env_bind_exprs()]
#' * [pkg_env()], [pkg_env_name()]
#' * [scoped_env()], [scoped_names()], [scoped_envs()], [is_scoped()]
#' * [ns_env()], [ns_imports_env()], [ns_env_name()]
#'
#' * [is_pairlist()], [as_pairlist()], [is_node()], [is_node_list()]
#' * [is_definition()], [new_definition()], [is_formulaish()],
#'   [dots_definitions()]
#'
#' * [scoped_options()], [with_options()], [push_options()],
#'   [peek_options()], [peek_option()]
#'
#' * [as_bytes()], [chr_unserialise_unicode()], [set_chr_encoding()],
#'   [chr_encoding()], [set_str_encoding()], [str_encoding()]
#'
#' * [mut_utf8_locale()], [mut_latin1_locale()], [mut_mbcs_locale()]
#'
#' * [prepend()], [modify()]
#'
#'
#' @section Questioning functions:
#'
#' \badgequestioning
#'
#' Since rlang 0.2.0.9000:
#'
#' * [child_env()]
#'
#'
#' Since rlang 0.2.0:
#'
#' * [UQ()], [UQS()]
#'
#' * [dots_splice()], [splice()]
#' * [invoke()]
#'
#' * [is_frame()], [global_frame()], [current_frame()],
#'   [ctxt_frame()], [call_frame()], [frame_position()]
#'
#' * [ctxt_depth()], [call_depth()], [ctxt_stack()], [call_stack()],
#'   [stack_trim()]
#'
#'
#' @section Soft-deprecated functions and arguments:
#'
#' \badgesoftdeprecated
#'
#' **Retired in rlang 0.2.0.9000**
#'
#' * [get_env()]: The `env` argument no longer has a default and must be supplied
#' * [cnd_signal()]: The `.mufflable` argument no longer has any effect
#'
#'
#' **Renamed in rlang 0.2.0.9000**
#'
#' * [cnd_signal()]: `.cnd` => `cnd`
#'
#'
#' **Retired in rlang 0.2.0:**
#'
#' * [eval_tidy_()]
#' * [overscope_clean()]
#' * [overscope_eval_next()] => [eval_tidy()]
#'
#' * [lang_head()], [lang_tail()]
#'
#'
#' **Renamed in rlang 0.2.0:**
#'
#' * [quo_expr()] => [quo_squash()]
#' * [parse_quosure()] => [parse_quo()]
#' * [parse_quosures()] => [parse_quos()]
#' * [as_overscope()] => [as_data_mask()]
#' * [new_overscope()] => [new_data_mask()]
#' * [as_dictionary()] => [as_data_pronoun()]
#'
#' * [lang()] => [call2()]
#' * [new_language()] => [new_call()]
#' * [is_lang()] => [is_call()]
#' * [is_unary_lang()] => Use the `n` argument of [is_call()]
#' * [is_binary_lang()] => Use the `n` argument of [is_call()]
#' * [quo_is_lang()] => [quo_is_call()]
#' * [is_expr()] => [is_expression()]
#'
#' * [lang_modify()] => [call_modify()]
#' * [lang_standardise()] => [call_standardise()]
#' * [lang_fn()] => [call_fn()]
#' * [lang_name()] => [call_name()]
#' * [lang_args()] => [call_args()]
#' * [lang_args_names()] => [call_args_names()]
#'
#'
#' @section Deprecated functions and arguments:
#'
#' \badgedeprecated
#'
#' **Renamed in rlang 0.2.0.9000**
#'
#' * [env_tail()]: `sentinel` => `last`
#' * [abort()], [warn()], [inform()]: `msg`, `type` and `call` =>
#'   `.msg`, `.type` and `.call`
#'
#'
#' **Retired in rlang 0.2.0:**
#'
#' * [UQE()]
#' * [is_quosureish()], [as_quosureish()]
#'
#'
#' @section Defunct functions and arguments:
#'
#' \badgedefunct
#'
#' **Retired in rlang 0.3.0:**
#'
#' * [cnd()], [error_cnd()], [warning_cnd()] and [message_cnd()]:
#'   `.msg` => `message`.
#' * [cnd_signal()]: `.msg` and `.call`.
#' * `cnd_inform()`, `cnd_warn()` and `cnd_abort()`
#'
#'
#' **Renamed in rlang 0.2.0**
#'
#' * `new_cnd()` => [cnd()]
#' * `cnd_message()` => [message_cnd()]
#' * `cnd_warning()` => [warning_cnd()]
#' * `cnd_error()` => [error_cnd()]
#'
#'
#' **Renamed in rlang 0.3.0:**
#'
#' * `inplace()` => [calling()]. The `muffle` argument of `inplace()`
#'   has not been implemented in `calling()` and is now defunct.
#' * `rst_muffle()` => [cnd_muffle()]
#'
#'
#' @name lifecycle
NULL
