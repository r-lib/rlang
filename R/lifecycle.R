#' Life cycle of the rlang package
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
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
#' * [as_function()], [new_function()]
#'
#'
#' @section Experimental functions:
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' These functions are not yet part of the rlang API. Expect breaking
#' changes.
#'
#' * [with_env()], [locally()], [env_poke()]
#' * [pkg_env()], [pkg_env_name()], [ns_env()], [ns_imports_env()], [ns_env_name()]
#'
#' * [is_pairlist()], [as_pairlist()], [is_node()], [is_node_list()]
#'
#' * [is_definition()], [new_definition()], [is_formulaish()],
#'   [dots_definitions()]
#'
#' * [local_options()], [with_options()], [push_options()],
#'   [peek_options()], [peek_option()]
#'
#' * [as_bytes()], [chr_unserialise_unicode()]
#'
#' * [caller_fn()], [current_fn()]
#'
#'
#' @section Questioning stage:
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#'
#' **In the questioning stage as of rlang 0.4.0**
#'
#' These functions are likely to be moved to the vctrs package:
#'
#' * [lgl()], [int()], etc.
#' * [new_logical()], [new_integer()], etc.
#' * [na_lgl], [na_int], [is_lgl_na()], [is_int_na()], etc.
#'
#'
#' **In the questioning stage as of rlang 0.3.0**
#'
#' * [child_env()]
#' * [flatten()], [squash()], and their atomic vector variants
#' * [modify()] and [prepend()]
#' * [with_restarts()], [rst_list()], [rst_exists()], [rst_jump()],
#'   [rst_maybe_jump()], [rst_abort()]. It is not clear yet whether we
#'   want to recommend restarts as a style of programming in R.
#' * [return_from()] and [return_to()].
#' * [expr_label()], [expr_name()], and [expr_text()].
#'
#'
#' **In the questioning stage as of rlang 0.2.0**
#'
#' * [UQ()], [UQS()]
#' * [dots_splice()], [splice()]
#'
#'
#' @section Soft-deprecated functions and arguments:
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#'
#' **Soft-deprecated in rlang 0.4.0**
#'
#' * [exiting()]: Handlers are now treated as exiting by default.
#' * [invoke()]: Use the simpler [exec()] instead.
#' * [as_logical()], [as_integer()], etc. => `vctrs::vec_cast()`.
#' * [type_of()], [switch_type()], [coerce_type()], [switch_class()],
#'   [coerce_class()]
#'
#'
#' @section Deprecated functions and arguments:
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' **Bumped to deprecated in rlang 0.4.0**
#'
#' * [modify()] and [prepend()].
#' * `new_logical_along()`, `new_integer_along()`,
#'   `new_double_along()`, `new_complex_along()`,
#'   `new_character_along()`, `new_raw_along()`, `new_list_along()`.
#'
#' * [lang_modify()] => [call_modify()]
#' * [lang_standardise()] => [call_standardise()]
#' * [lang_fn()] => [call_fn()]
#' * [lang_name()] => [call_name()]
#' * [lang_args()] => [call_args()]
#' * [lang_args_names()] => [call_args_names()]
#' * [lang_head()], [lang_tail()]
#' * [lang()] => [call2()]
#' * [new_language()] => [new_call()]
#' * [is_lang()] => [is_call()]
#' * [is_unary_lang()] => Use the `n` argument of [is_call()]
#' * [is_binary_lang()] => Use the `n` argument of [is_call()]
#' * [quo_is_lang()] => [quo_is_call()]
#'
#' * [call_modify()]: `.standardise` and `.env` arguments.
#'
#' * [is_expr()] => [is_expression()]
#' * `quo_expr()` => [quo_squash()]
#' * [parse_quosure()] => [parse_quo()]
#' * [parse_quosures()] => [parse_quos()]
#' * Assigning non-quosure objects to quosure lists.
#' * `as.character()` on quosures.
#'
#' * [cnd_signal()]: `.cnd` => `cnd`
#' * [cnd_signal()]: The `.mufflable` argument no longer has any effect
#'
#' * `scoped_names()` => [base::search()]
#' * `is_scoped()` => [is_attached()]
#' * `scoped_env()` => [search_env()]
#' * `scoped_envs()` => [search_envs()]
#'
#' * `env_bind_exprs()` => [env_bind_lazy()]
#' * `env_bind_fns()` => [env_bind_active()]
#' * Passing a function or formula to `env_depth()`,
#'   `env_poke_parent()`, `env_parent<-`, `env_tail()`, `set_env()`,
#'   `env_clone()`, `env_inherits()`, `env_bind()`,
#'   `local_bindings()`, `with_bindings()`, `env_poke()`,
#'   `env_has()`, `env_get()`, `env_names()`, `env_bind_exprs()` and
#'   `env_bind_fns()`. This internal genericity was causing confusion
#'   (see issue #427). You should now extract the environment
#'   separately before calling these functions.
#' * [get_env()]: The `env` argument no longer has a default and must be supplied
#'
#' * [is_frame()], [global_frame()], [current_frame()],
#'   [ctxt_frame()], [call_frame()], [frame_position()],
#'   [caller_frame()]
#'
#' * [ctxt_depth()], [call_depth()], [ctxt_stack()], [call_stack()],
#'   [stack_trim()]
#'
#' * [set_attrs()], [mut_attrs()]
#'
#' * The `width` and `printer` arguments of [exprs_auto_name()] and
#'   [quos_auto_name()] no longer have any effect. For the same
#'   reason, passing a width as `.named` argument of dots collectors
#'   like `quos()` is deprecated.
#'
#' * `as_overscope()` => [as_data_mask()]
#' * `new_overscope()` => [new_data_mask()]
#' * `overscope_eval_next()` => [eval_tidy()]
#' * `overscope_clean()`
#'
#'
#' @section Defunct functions and arguments:
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("defunct")}
#'
#' **Defunct as of rlang 0.4.0**
#'
#' * `length()` and `names()` on tidy eval `.data` pronouns.
#' * Supplying a named `!!!` call.
#'
#' @keywords internal
#' @name lifecycle
NULL
