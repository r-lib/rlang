#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Compile with `-fvisibility=hidden -DHAVE_VISIBILITY_ATTRIBUTE` if you link to this library
#include <R_ext/Visibility.h>
#define export attribute_visible extern

#include <rlang.h>

// Callable from other packages
extern sexp* r_squash_if(sexp*, enum r_type, bool (*is_spliceable)(sexp*), int);
extern bool rlang_is_clevel_spliceable(sexp*);
extern bool rlang_is_quosure(sexp*);

// Callable from this package
extern sexp* r_f_lhs(sexp*);
extern sexp* r_f_rhs(sexp*);
extern sexp* r_new_condition(sexp*, sexp*, sexp*);
extern sexp* r_env_clone(sexp*, sexp*);
extern sexp* rlang_env_unbind(sexp*, sexp*, sexp*);
extern sexp* rlang_env_poke_parent(sexp*, sexp*);
extern sexp* rlang_env_frame(sexp* env);
extern sexp* rlang_env_hash_table(sexp* env);
extern sexp* rlang_poke_type(sexp*, sexp*);
extern sexp* rlang_replace_na(sexp*, sexp*);
extern sexp* rlang_node_car(sexp*);
extern sexp* rlang_node_cdr(sexp*);
extern sexp* rlang_node_caar(sexp*);
extern sexp* rlang_node_cadr(sexp*);
extern sexp* rlang_node_cdar(sexp*);
extern sexp* rlang_node_cddr(sexp*);
extern sexp* rlang_missing_arg();
extern sexp* rlang_node_poke_car(sexp*, sexp*);
extern sexp* rlang_node_poke_cdr(sexp*, sexp*);
extern sexp* rlang_node_poke_caar(sexp*, sexp*);
extern sexp* rlang_node_poke_cadr(sexp*, sexp*);
extern sexp* rlang_node_poke_cdar(sexp*, sexp*);
extern sexp* rlang_node_poke_cddr(sexp*, sexp*);
extern sexp* rlang_duplicate(sexp*, sexp*);
extern sexp* r_node_tree_clone(sexp*);
extern sexp* rlang_node_tag(sexp*);
extern sexp* rlang_node_poke_tag(sexp*, sexp*);
extern sexp* rlang_interp(sexp*, sexp*);
extern sexp* rlang_is_function(sexp*);
extern sexp* rlang_is_closure(sexp*);
extern sexp* rlang_is_primitive(sexp*);
extern sexp* rlang_is_primitive_eager(sexp*);
extern sexp* rlang_is_primitive_lazy(sexp*);
extern sexp* rlang_is_formula(sexp*, sexp*, sexp*);
extern sexp* rlang_is_formulaish(sexp*, sexp*, sexp*);
extern sexp* rlang_is_reference(sexp*, sexp*);
extern sexp* rlang_sexp_address(sexp*);
extern sexp* rlang_length(sexp*);
extern sexp* rlang_true_length(sexp* x);
extern sexp* rlang_squash(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_symbol(sexp*);
extern sexp* rlang_sym_as_character(sexp*);
extern sexp* rlang_tilde_eval(sexp*, sexp*, sexp*);
extern sexp* rlang_unescape_character(sexp*);
extern sexp* rlang_capturearginfo(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_capturedots(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_new_call_node(sexp*, sexp*);
extern sexp* rlang_cnd_signal(sexp*);
extern sexp* rlang_r_string(sexp*);
extern sexp* rlang_exprs_interp(sexp*, sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_quos_interp(sexp*, sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_dots_list(sexp*, sexp*, sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_dots_flat_list(sexp*, sexp*, sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_dots_pairlist(sexp*, sexp*, sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* r_new_formula(sexp*, sexp*, sexp*);
extern sexp* rlang_new_quosure(sexp*, sexp*);
extern sexp* rlang_enexpr(sexp*, sexp*);
extern sexp* rlang_ensym(sexp*, sexp*);
extern sexp* rlang_enquo(sexp*, sexp*);
extern sexp* rlang_get_expression(sexp*, sexp*);
extern sexp* rlang_vec_alloc(sexp*, sexp*);
extern sexp* rlang_vec_coerce(sexp*, sexp*);
extern sexp* rlang_mark_object(sexp*);
extern sexp* rlang_promise_expr(sexp*, sexp*);
extern sexp* rlang_promise_env(sexp*, sexp*);
extern sexp* rlang_promise_value(sexp*, sexp*);
extern sexp* rlang_unmark_object(sexp*);
extern sexp* rlang_quo_is_missing(sexp*);
extern sexp* rlang_quo_is_symbol(sexp*);
extern sexp* rlang_quo_is_call(sexp*);
extern sexp* rlang_quo_is_symbolic(sexp*);
extern sexp* rlang_quo_is_null(sexp*);
extern sexp* rlang_vec_poke_n(sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_vec_poke_range(sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_quo_get_expr(sexp*);
extern sexp* rlang_quo_set_expr(sexp*, sexp*);
extern sexp* rlang_quo_get_env(sexp*);
extern sexp* rlang_quo_set_env(sexp*, sexp*);
extern sexp* rlang_which_operator(sexp*);
extern sexp* rlang_new_data_mask(sexp*, sexp*);
extern sexp* rlang_new_data_mask_compat(sexp*, sexp*, sexp*);
extern sexp* rlang_as_data_mask(sexp*);
extern sexp* rlang_as_data_mask_compat(sexp*, sexp*);
extern sexp* rlang_data_mask_clean(sexp*);
extern sexp* rlang_as_data_pronoun(sexp*);
extern sexp* rlang_env_get(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_env_get_list(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_env_unlock(sexp*);
extern sexp* rlang_interrupt();
extern sexp* rlang_is_list(sexp*, sexp*);
extern sexp* rlang_is_atomic(sexp*, sexp*);
extern sexp* rlang_is_vector(sexp*, sexp*);
extern sexp* rlang_is_finite(sexp*);
extern sexp* rlang_is_logical(sexp*, sexp*);
extern sexp* rlang_is_integer(sexp*, sexp*);
extern sexp* rlang_is_double(sexp*, sexp*, sexp*);
extern sexp* rlang_is_integerish(sexp*, sexp*, sexp*);
extern sexp* rlang_is_character(sexp*, sexp*);
extern sexp* rlang_is_raw(sexp*, sexp*);
extern sexp* rlang_is_data_mask(sexp*);
extern sexp* rlang_data_pronoun_get(sexp*, sexp*);
extern sexp* rlang_cnd_type(sexp*);
extern sexp* rlang_env_inherits(sexp*, sexp*);
extern sexp* rlang_eval_top(sexp*, sexp*);
extern sexp* rlang_attrib(sexp*);
extern sexp* rlang_named(sexp*, sexp*);
extern sexp* r_pairlist_rev(sexp*);
extern sexp* rlang_new_splice_box(sexp*);
extern sexp* rlang_is_splice_box(sexp*);
extern sexp* rlang_unbox(sexp*);
extern sexp* rlang_new_function(sexp*, sexp*, sexp*);
extern sexp* rlang_is_string(sexp*, sexp*);
extern sexp* rlang_new_weakref(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_wref_key(sexp*);
extern sexp* rlang_wref_value(sexp*);
extern sexp* rlang_is_weakref(sexp*);
extern sexp* rlang_find_var(sexp*, sexp*);
extern sexp* rlang_env_bind_list(sexp*, sexp*, sexp*);
extern sexp* rlang_glue_is_there();
extern sexp* rlang_linked_version();
extern sexp* rlang_names2(sexp*, sexp*);
extern sexp* rlang_set_names(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_chr_get(sexp* x, sexp* i);
extern sexp* rlang_env_has(sexp*, sexp*, sexp*);
extern sexp* rlang_env_poke(sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_env_bind(sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_raw_deparse_str(sexp*, sexp*, sexp*);
extern sexp* rlang_env_browse(sexp*, sexp*);
extern sexp* rlang_env_is_browsed(sexp*);
extern sexp* rlang_ns_registry_env();
extern sexp* rlang_hash(sexp*);

// Library initialisation defined below
sexp* rlang_library_load(sexp*);
sexp* rlang_library_unload();

// For unit tests
extern sexp* chr_prepend(sexp*, sexp*);
extern sexp* chr_append(sexp*, sexp*);
extern sexp* rlang_test_r_warn(sexp*);
extern sexp* rlang_on_exit(sexp*, sexp*);
extern sexp* rlang_test_is_special_op_sym(sexp*);
extern sexp* rlang_test_base_ns_get(sexp*);
extern sexp* rlang_test_parse(sexp*);
extern sexp* rlang_test_parse_eval(sexp*, sexp*);
extern sexp* r_current_frame();
extern sexp* rlang_test_node_list_clone_until(sexp*, sexp*);
extern sexp* rlang_test_sys_frame(sexp*);
extern sexp* rlang_test_sys_call(sexp*);
extern sexp* rlang_test_nms_are_duplicated(sexp*, sexp*);
extern sexp* rlang_test_Rf_warningcall(sexp*, sexp*);
extern sexp* rlang_test_Rf_errorcall(sexp*, sexp*);
extern sexp* rlang_test_lgl_sum(sexp*, sexp*);
extern sexp* rlang_test_lgl_which(sexp*, sexp*);
extern sexp* rlang_new_dict(sexp*, sexp*);
extern sexp* rlang_dict_put(sexp*, sexp*, sexp*);
extern sexp* rlang_dict_has(sexp*, sexp*);
extern sexp* rlang_dict_get(sexp*, sexp*);
extern sexp* rlang_dict_resize(sexp*, sexp*);

static const r_callable r_callables[] = {
  {"r_init_library",                    (r_void_fn) &r_init_library, 0},
  {"rlang_library_load",                (r_void_fn) &rlang_library_load, 1},
  {"rlang_library_unload",              (r_void_fn) &rlang_library_unload, 0},
  {"r_f_lhs",                           (r_void_fn) &r_f_lhs, 1},
  {"r_f_rhs",                           (r_void_fn) &r_f_rhs, 1},
  {"rlang_new_condition",               (r_void_fn) &r_new_condition, 3},
  {"rlang_replace_na",                  (r_void_fn) &rlang_replace_na, 2},
  {"rlang_capturearginfo",              (r_void_fn) &rlang_capturearginfo, 4},
  {"rlang_capturedots",                 (r_void_fn) &rlang_capturedots, 4},
  {"rlang_duplicate",                   (r_void_fn) &rlang_duplicate, 2},
  {"rlang_node_tree_clone",             (r_void_fn) &r_node_tree_clone, 1},
  {"rlang_interp",                      (r_void_fn) &rlang_interp, 2},
  {"rlang_is_function",                 (r_void_fn) &rlang_is_function, 1},
  {"rlang_is_closure",                  (r_void_fn) &rlang_is_closure, 1},
  {"rlang_is_primitive",                (r_void_fn) &rlang_is_primitive, 1},
  {"rlang_is_primitive_eager",          (r_void_fn) &rlang_is_primitive_eager, 1},
  {"rlang_is_primitive_lazy",           (r_void_fn) &rlang_is_primitive_lazy, 1},
  {"rlang_is_formula",                  (r_void_fn) &rlang_is_formula, 3},
  {"rlang_is_formulaish",               (r_void_fn) &rlang_is_formulaish, 3},
  {"rlang_is_reference",                (r_void_fn) &rlang_is_reference, 2},
  {"rlang_length",                      (r_void_fn) &rlang_length, 1},
  {"rlang_true_length",                 (r_void_fn) &rlang_true_length, 1},
  {"rlang_attrib",                      (r_void_fn) &r_attrib, 1},
  {"rlang_poke_attrib",                 (r_void_fn) &r_poke_attrib, 2},
  {"rlang_missing_arg",                 (r_void_fn) &rlang_missing_arg, 0},
  {"rlang_node_car",                    (r_void_fn) &rlang_node_car, 1},
  {"rlang_node_cdr",                    (r_void_fn) &rlang_node_cdr, 1},
  {"rlang_node_caar",                   (r_void_fn) &rlang_node_caar, 1},
  {"rlang_node_cadr",                   (r_void_fn) &rlang_node_cadr, 1},
  {"rlang_node_cdar",                   (r_void_fn) &rlang_node_cdar, 1},
  {"rlang_node_cddr",                   (r_void_fn) &rlang_node_cddr, 1},
  {"rlang_node_poke_car",               (r_void_fn) &rlang_node_poke_car, 2},
  {"rlang_node_poke_cdr",               (r_void_fn) &rlang_node_poke_cdr, 2},
  {"rlang_node_poke_caar",              (r_void_fn) &rlang_node_poke_caar, 2},
  {"rlang_node_poke_cadr",              (r_void_fn) &rlang_node_poke_cadr, 2},
  {"rlang_node_poke_cdar",              (r_void_fn) &rlang_node_poke_cdar, 2},
  {"rlang_node_poke_cddr",              (r_void_fn) &rlang_node_poke_cddr, 2},
  {"rlang_new_node",                    (r_void_fn) &r_new_node, 2},
  {"rlang_nms_are_duplicated",          (r_void_fn) &rlang_test_nms_are_duplicated, 2},
  {"rlang_env_clone",                   (r_void_fn) &r_env_clone, 2},
  {"rlang_env_unbind",                  (r_void_fn) &rlang_env_unbind, 3},
  {"rlang_env_poke_parent",             (r_void_fn) &rlang_env_poke_parent, 2},
  {"rlang_env_frame",                   (r_void_fn) &rlang_env_frame, 1},
  {"rlang_env_hash_table",              (r_void_fn) &rlang_env_hash_table, 1},
  {"rlang_poke_type",                   (r_void_fn) &rlang_poke_type, 2},
  {"rlang_mark_object",                 (r_void_fn) &rlang_mark_object, 1},
  {"rlang_promise_expr",                (r_void_fn) &rlang_promise_expr, 2},
  {"rlang_promise_env",                 (r_void_fn) &rlang_promise_env, 2},
  {"rlang_promise_value",               (r_void_fn) &rlang_promise_value, 2},
  {"rlang_unmark_object",               (r_void_fn) &rlang_unmark_object, 1},
  {"rlang_node_tag",                    (r_void_fn) &rlang_node_tag, 1},
  {"rlang_node_poke_tag",               (r_void_fn) &rlang_node_poke_tag, 2},
  {"rlang_squash",                      (r_void_fn) &rlang_squash, 4},
  {"rlang_sexp_address",                (r_void_fn) &rlang_sexp_address, 1},
  {"rlang_symbol",                      (r_void_fn) &rlang_symbol, 1},
  {"rlang_sym_as_character",            (r_void_fn) &rlang_sym_as_character, 1},
  // No longer necessary but keep this around for a while in case
  // quosures ended up saved as RDS.
  {"rlang_tilde_eval",                  (r_void_fn) &rlang_tilde_eval, 3},
  {"rlang_unescape_character",          (r_void_fn) &rlang_unescape_character, 1},
  {"rlang_new_call",                    (r_void_fn) &rlang_new_call_node, 2},
  {"rlang_cnd_signal",                  (r_void_fn) &rlang_cnd_signal, 1},
  {"rlang_test_chr_prepend",            (r_void_fn) &chr_prepend, 2},
  {"rlang_test_chr_append",             (r_void_fn) &chr_append, 2},
  {"rlang_test_r_warn",                 (r_void_fn) &rlang_test_r_warn, 1},
  {"rlang_test_r_on_exit",              (r_void_fn) &rlang_on_exit, 2},
  {"rlang_test_is_special_op_sym",      (r_void_fn) &rlang_test_is_special_op_sym, 1},
  {"rlang_test_base_ns_get",            (r_void_fn) &rlang_test_base_ns_get, 1},
  {"rlang_test_current_frame",          (r_void_fn) &r_current_frame, 0},
  {"rlang_test_parse",                  (r_void_fn) &rlang_test_parse, 1},
  {"rlang_test_parse_eval",             (r_void_fn) &rlang_test_parse_eval, 2},
  {"rlang_test_node_list_clone_until",  (r_void_fn) &rlang_test_node_list_clone_until, 2},
  {"rlang_test_attrib_set",             (r_void_fn) &r_attrib_set, 3},
  {"rlang_test_sys_frame",              (r_void_fn) &rlang_test_sys_frame, 1},
  {"rlang_test_sys_call",               (r_void_fn) &rlang_test_sys_call, 1},
  {"rlang_test_Rf_warningcall",         (r_void_fn) &rlang_test_Rf_warningcall, 2},
  {"rlang_test_Rf_errorcall",           (r_void_fn) &rlang_test_Rf_errorcall, 2},
  {"rlang_test_lgl_sum",                (r_void_fn) &rlang_test_lgl_sum, 2},
  {"rlang_test_lgl_which",              (r_void_fn) &rlang_test_lgl_which, 2},
  {"rlang_r_string",                    (r_void_fn) &rlang_r_string, 1},
  {"rlang_exprs_interp",                (r_void_fn) &rlang_exprs_interp, 6},
  {"rlang_quos_interp",                 (r_void_fn) &rlang_quos_interp, 6},
  {"rlang_dots_list",                   (r_void_fn) &rlang_dots_list, 7},
  {"rlang_dots_flat_list",              (r_void_fn) &rlang_dots_flat_list, 7},
  {"rlang_dots_pairlist",               (r_void_fn) &rlang_dots_pairlist, 7},
  {"rlang_new_formula",                 (r_void_fn) &r_new_formula, 3},
  {"rlang_new_quosure",                 (r_void_fn) &rlang_new_quosure, 2},
  {"rlang_enexpr",                      (r_void_fn) &rlang_enexpr, 2},
  {"rlang_ensym",                       (r_void_fn) &rlang_ensym, 2},
  {"rlang_enquo",                       (r_void_fn) &rlang_enquo, 2},
  {"rlang_get_expression",              (r_void_fn) &rlang_get_expression, 2},
  {"rlang_vec_alloc",                   (r_void_fn) &rlang_vec_alloc, 2},
  {"rlang_vec_coerce",                  (r_void_fn) &rlang_vec_coerce, 2},
  {"rlang_quo_is_symbol",               (r_void_fn) &rlang_quo_is_symbol, 1},
  {"rlang_quo_is_call",                 (r_void_fn) &rlang_quo_is_call, 1},
  {"rlang_quo_is_symbolic",             (r_void_fn) &rlang_quo_is_symbolic, 1},
  {"rlang_quo_is_missing",              (r_void_fn) &rlang_quo_is_missing, 1},
  {"rlang_quo_is_null",                 (r_void_fn) &rlang_quo_is_null, 1},
  {"rlang_quo_get_expr",                (r_void_fn) &rlang_quo_get_expr, 1},
  {"rlang_quo_set_expr",                (r_void_fn) &rlang_quo_set_expr, 2},
  {"rlang_quo_get_env",                 (r_void_fn) &rlang_quo_get_env, 1},
  {"rlang_quo_set_env",                 (r_void_fn) &rlang_quo_set_env, 2},
  {"rlang_vec_poke_n",                  (r_void_fn) &rlang_vec_poke_n, 5},
  {"rlang_vec_poke_range",              (r_void_fn) &rlang_vec_poke_range, 5},
  {"rlang_which_operator",              (r_void_fn) &rlang_which_operator, 1},
  {"rlang_call_has_precedence",         (r_void_fn) &rlang_call_has_precedence, 3},
  {"rlang_new_data_mask",               (r_void_fn) &rlang_new_data_mask, 2},
  {"rlang_as_data_mask",                (r_void_fn) &rlang_as_data_mask, 1},
  {"rlang_is_data_mask",                (r_void_fn) &rlang_is_data_mask, 1},
  {"rlang_data_pronoun_get",            (r_void_fn) &rlang_data_pronoun_get, 2},
  {"rlang_data_mask_clean",             (r_void_fn) &rlang_data_mask_clean, 1},
  {"rlang_as_data_pronoun",             (r_void_fn) &rlang_as_data_pronoun, 1},
  {"rlang_env_binding_types",           (r_void_fn) &r_env_binding_types, 2},
  {"rlang_env_get",                     (r_void_fn) &rlang_env_get, 4},
  {"rlang_env_get_list",                (r_void_fn) &rlang_env_get_list, 4},
  {"rlang_env_unlock",                  (r_void_fn) &rlang_env_unlock, 1},
  {"rlang_interrupt",                   (r_void_fn) &rlang_interrupt, 0},
  {"rlang_is_list",                     (r_void_fn) &rlang_is_list, 2},
  {"rlang_is_atomic",                   (r_void_fn) &rlang_is_atomic, 2},
  {"rlang_is_vector",                   (r_void_fn) &rlang_is_vector, 2},
  {"rlang_is_finite",                   (r_void_fn) &rlang_is_finite, 1},
  {"rlang_is_logical",                  (r_void_fn) &rlang_is_logical, 2},
  {"rlang_is_integer",                  (r_void_fn) &rlang_is_integer, 2},
  {"rlang_is_double",                   (r_void_fn) &rlang_is_double, 3},
  {"rlang_is_integerish",               (r_void_fn) &rlang_is_integerish, 3},
  {"rlang_is_character",                (r_void_fn) &rlang_is_character, 2},
  {"rlang_is_raw",                      (r_void_fn) &rlang_is_raw, 2},
  {"rlang_cnd_type",                    (r_void_fn) &rlang_cnd_type, 1},
  {"rlang_env_inherits",                (r_void_fn) &rlang_env_inherits, 2},
  {"rlang_eval_top",                    (r_void_fn) &rlang_eval_top, 2},
  {"rlang_named",                       (r_void_fn) &rlang_named, 2},
  {"rlang_pairlist_rev",                (r_void_fn) &r_pairlist_rev, 1},
  {"rlang_new_splice_box",              (r_void_fn) &rlang_new_splice_box, 1},
  {"rlang_is_splice_box",               (r_void_fn) &rlang_is_splice_box, 1},
  {"rlang_new_function",                (r_void_fn) &rlang_new_function, 3},
  {"rlang_is_string",                   (r_void_fn) &rlang_is_string, 2},
  {"rlang_new_weakref",                 (r_void_fn) &rlang_new_weakref, 4},
  {"rlang_wref_key",                    (r_void_fn) &rlang_wref_key, 1},
  {"rlang_wref_value",                  (r_void_fn) &rlang_wref_value, 1},
  {"rlang_is_weakref",                  (r_void_fn) &rlang_is_weakref, 1},
  {"rlang_find_var",                    (r_void_fn) &rlang_find_var, 2},
  {"rlang_env_bind_list",               (r_void_fn) &rlang_env_bind_list, 3},
  {"rlang_glue_is_there",               (r_void_fn) &rlang_glue_is_there, 0},
  {"rlang_linked_version",              (r_void_fn) &rlang_linked_version, 0},
  {"rlang_names2",                      (r_void_fn) &rlang_names2, 2},
  {"rlang_set_names",                   (r_void_fn) &rlang_set_names, 4},
  {"rlang_chr_get",                     (r_void_fn) &rlang_chr_get, 2},
  {"rlang_env_has",                     (r_void_fn) &rlang_env_has, 3},
  {"rlang_env_poke",                    (r_void_fn) &rlang_env_poke, 5},
  {"rlang_env_bind",                    (r_void_fn) &rlang_env_bind, 5},
  {"rlang_raw_deparse_str",             (r_void_fn) &rlang_raw_deparse_str, 3},
  {"rlang_env_browse",                  (r_void_fn) &rlang_env_browse, 2},
  {"rlang_env_is_browsed",              (r_void_fn) &rlang_env_is_browsed, 1},
  {"rlang_ns_registry_env",             (r_void_fn) &rlang_ns_registry_env, 0},
  {"rlang_hash",                        (r_void_fn) &rlang_hash, 1},
  {"rlang_new_dict",                    (r_void_fn) &rlang_new_dict, 2},
  {"rlang_dict_put",                    (r_void_fn) &rlang_dict_put, 3},
  {"rlang_dict_has",                    (r_void_fn) &rlang_dict_has, 2},
  {"rlang_dict_get",                    (r_void_fn) &rlang_dict_get, 2},
  {"rlang_dict_resize",                 (r_void_fn) &rlang_dict_resize, 2},
  {NULL, NULL, 0}
};


extern sexp* rlang_ext_arg_match0(sexp*);
extern sexp* rlang_ext_capturearginfo(sexp*);
extern sexp* rlang_ext_capturedots(sexp*);
extern sexp* rlang_ext_dots_values(sexp*);

extern sexp* rlang_ext2_call2(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_ext2_exec(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_ext2_eval(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_ext2_eval_tidy(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_ext2_tilde_eval(sexp*, sexp*, sexp*, sexp*);


static const r_external externals[] = {
  {"rlang_ext_arg_match0",              (r_void_fn) &rlang_ext_arg_match0, 3},
  {"rlang_ext_capturearginfo",          (r_void_fn) &rlang_ext_capturearginfo, 2},
  {"rlang_ext_capturedots",             (r_void_fn) &rlang_ext_capturedots, 1},
  {"rlang_ext_dots_values",             (r_void_fn) &rlang_ext_dots_values, 7},

  {"rlang_ext2_call2",                  (r_void_fn) &rlang_ext2_call2, 2},
  {"rlang_ext2_exec",                   (r_void_fn) &rlang_ext2_exec, 2},
  {"rlang_ext2_eval",                   (r_void_fn) &rlang_ext2_eval, 2},
  {"rlang_ext2_eval_tidy",              (r_void_fn) &rlang_ext2_eval_tidy, 3},
  {"rlang_ext2_tilde_eval",             (r_void_fn) &rlang_ext2_tilde_eval, 3},
  {NULL, NULL, 0}
};


extern bool is_splice_box(sexp*);
extern sexp* rlang_env_dots_values(sexp*);
extern sexp* rlang_env_dots_list(sexp*);
extern sexp* rlang_eval_tidy(sexp*, sexp*, sexp*);
extern void rlang_print_backtrace(bool full);

// From xxhash.h
extern uint64_t XXH3_64bits(const void*, size_t);

export void R_init_rlang(r_dll_info* dll) {
  r_register_c_callable("rlang", "rlang_new_quosure", (r_void_fn) &rlang_new_quosure);
  r_register_c_callable("rlang", "rlang_is_quosure", (r_void_fn) &rlang_is_quosure);
  r_register_c_callable("rlang", "rlang_quo_get_expr", (r_void_fn) &rlang_quo_get_expr);
  r_register_c_callable("rlang", "rlang_quo_set_expr", (r_void_fn) &rlang_quo_set_expr);
  r_register_c_callable("rlang", "rlang_quo_get_env", (r_void_fn) &rlang_quo_get_env);
  r_register_c_callable("rlang", "rlang_quo_set_env", (r_void_fn) &rlang_quo_set_env);
  r_register_c_callable("rlang", "rlang_as_data_pronoun", (r_void_fn) &rlang_as_data_pronoun);
  r_register_c_callable("rlang", "rlang_as_data_mask_3.0.0", (r_void_fn) &rlang_as_data_mask);
  r_register_c_callable("rlang", "rlang_new_data_mask_3.0.0", (r_void_fn) &rlang_new_data_mask);
  r_register_c_callable("rlang", "rlang_eval_tidy", (r_void_fn) &rlang_eval_tidy);

  r_register_c_callable("rlang", "rlang_xxh3_64bits", (r_void_fn) &XXH3_64bits);

  // Maturing
  r_register_c_callable("rlang", "rlang_is_splice_box", (r_void_fn) &is_splice_box);
  r_register_c_callable("rlang", "rlang_unbox", (r_void_fn) &rlang_unbox);
  r_register_c_callable("rlang", "rlang_env_dots_values", (r_void_fn) &rlang_env_dots_values);
  r_register_c_callable("rlang", "rlang_env_dots_list", (r_void_fn) &rlang_env_dots_list);
  r_register_c_callable("rlang", "rlang_sym_as_character", (r_void_fn) &rlang_sym_as_character);
  r_register_c_callable("rlang", "rlang_str_as_symbol", (r_void_fn) &r_str_as_symbol);

  // Experimental
  r_register_c_callable("rlang", "rlang_squash_if", (r_void_fn) &r_squash_if);

  // Compatibility
  r_register_c_callable("rlang", "rlang_as_data_mask", (r_void_fn) &rlang_as_data_mask_compat);
  r_register_c_callable("rlang", "rlang_new_data_mask", (r_void_fn) &rlang_new_data_mask_compat);

  // Only for debugging - no stability guaranteed
  r_register_c_callable("rlang", "rlang_print_backtrace", (r_void_fn) &rlang_print_backtrace);

  r_register_r_callables(dll, r_callables, externals);
}


// From "../internal/internal.h"
void rlang_init_internal(sexp* ns);

sexp* rlang_library_load(sexp* ns) {
  rlang_init_internal(ns);
  return r_null;
}

sexp* rlang_library_unload() {
  return r_null;
}
