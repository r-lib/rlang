
lifecycle <- function(stage) {
  url <- paste0("https://www.tidyverse.org/lifecycle/#", stage)
  img <- lifecycle_img(stage, url)

  sprintf(
    "\\ifelse{html}{%s}{\\strong{%s}}",
    img,
    upcase1(stage)
  )
}

lifecycle_img <- function(stage, url) {
  file <- sprintf("lifecycle-%s.svg", stage)
  stage_alt <- upcase1(stage)

  switch(stage,

    experimental = ,
    maturing = ,
    stable = ,
    questioning = ,
    archived =
      sprintf(
        "\\out{<a href='%s'><img src='%s' alt='%s lifecycle'></a>}",
        url,
        file.path("figures", file),
        stage_alt
      )
   ,

    `soft-deprecated` = ,
    deprecated = ,
    defunct =
      sprintf(
        "\\figure{%s}{options: alt='%s lifecycle'}",
        file,
        stage_alt
      )

  )
}
upcase1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


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
#' * [caller_fn()], [current_fn()]
#'
#'
#' @section Questioning stage:
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' **In the questioning stage as of rlang 0.3.0**
#'
#' * [child_env()]
#' * [type_of()], [switch_type()], [coerce_type()]
#' * [switch_class()], [coerce_class()]
#' * [lang_type_of()], [switch_lang()], [coerce_lang()]
#' * [flatten()], [squash()], and their atomic vector variants
#' * [modify()] and [prepend()]
#' * [as_logical()], [as_character()], etc.
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
#' **Soft-deprecated as of rlang 0.3.0**
#'
#' * [get_env()]: The `env` argument no longer has a default and must be supplied
#' * [cnd_signal()]: The `.mufflable` argument no longer has any effect
#' * [invoke()]: Use the simpler [exec()] instead.
#' * [set_attrs()], [mut_attrs()]
#'
#' * [cnd_signal()]: `.cnd` => `cnd`
#'
#' * [is_frame()], [global_frame()], [current_frame()],
#'   [ctxt_frame()], [call_frame()], [frame_position()],
#'   [caller_frame()]
#'
#' * [ctxt_depth()], [call_depth()], [ctxt_stack()], [call_stack()],
#'   [stack_trim()]
#'
#' * Passing a function or formula to `env_depth()`,
#'   `env_poke_parent()`, `env_parent<-`, `env_tail()`, `set_env()`,
#'   `env_clone()`, `env_inherits()`, `env_bind()`,
#'   `scoped_bindings()`, `with_bindings()`, `env_poke()`,
#'   `env_has()`, `env_get()`, `env_names()`, `env_bind_exprs()` and
#'   `env_bind_fns()`. This internal genericity was causing confusion
#'   (see issue #427). You should now extract the environment
#'   separately before calling these functions.
#'
#' * `env_bind_exprs()` => [env_bind_lazy()]
#' * `env_bind_fns()` => [env_bind_active()]
#'
#' * `scoped_names()` => [base::search()]
#' * `is_scoped()` => [is_attached()]
#' * `scoped_env()` => [search_env()]
#' * `scoped_envs()` => [search_envs()]
#'
#' * The `width` and `printer` arguments of [exprs_auto_name()] and
#'   [quos_auto_name()] no longer have any effect. For the same
#'   reason, passing a width as `.named` argument of dots collectors
#'   like `quos()` is soft-deprecated.
#'
#' * [call_modify()]: `.standardise` and `.env` arguments.
#'
#' * `new_logical_along()`, `new_integer_along()`,
#'   `new_double_along()`, `new_complex_along()`,
#'   `new_character_along()`, `new_raw_along()`, `new_list_along()`.
#'
#' * `as.character()` on quosures.
#'
#' * Assigning non-quosure objects to quosure lists.
#'
#' * Supplying a named `!!!` call.
#'
#'
#' **Soft-deprecated as of rlang 0.2.0:**
#'
#' * [overscope_clean()]
#' * [overscope_eval_next()] => [eval_tidy()]
#'
#' * [lang_head()], [lang_tail()]
#'
#' * [quo_expr()] => [quo_squash()]
#' * [parse_quosure()] => [parse_quo()]
#' * [parse_quosures()] => [parse_quos()]
#' * [as_overscope()] => [as_data_mask()]
#' * [new_overscope()] => [new_data_mask()]
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
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("deprecated")}
#'
#' **Deprecated as of rlang 0.3.0**
#'
#' * [as_data_mask()]: `parent` argument
#' * [new_data_mask()]: `parent` argument
#'
#' * [env_tail()]: `sentinel` => `last`
#'
#' * [abort()], [warn()], [inform()]: `msg`, `type` => `.msg`, `.type`
#' * [abort()], [warn()], [inform()], [cnd()], [error_cnd()],
#'   [warning_cnd()], [message_cnd()]: `call` argument.
#'
#' * `length()` and `names()` on tidy eval `.data` pronouns.
#'
#' * [is_character()], [is_string()], and variants: The `encoding`
#'   argument.
#'
#'
#' @section Defunct functions and arguments:
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("defunct")}
#'
#' **Defunct as of rlang 0.3.0:**
#'
#' * [UQE()]
#' * [eval_tidy_()]
#' * [is_quosureish()], [as_quosureish()]
#' * `as_dictionary()` => [as_data_pronoun()]
#'
#' * [cnd_signal()]: `.msg` and `.call`.
#'
#' * [cnd()], [error_cnd()], [warning_cnd()] and [message_cnd()]:
#'   `.msg` => `message`.
#'
#'
#' @section Archived:
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("archived")}
#'
#' These functions were entirely removed from the package. You will
#' find them in the commit history and previous releases.
#'
#' **Archived in rlang 0.3.0:**
#'
#' * `cnd_inform()`, `cnd_warn()` and `cnd_abort()`
#'
#' * `new_cnd()` => [cnd()]
#' * `cnd_message()` => [message_cnd()]
#' * `cnd_warning()` => [warning_cnd()]
#' * `cnd_error()` => [error_cnd()]
#' * `rst_muffle()` => [cnd_muffle()]
#' * `inplace()` => [calling()]. The `muffle` argument of `inplace()`
#'   has not been implemented in `calling()` and is now defunct.
#'
#' @name lifecycle
NULL
