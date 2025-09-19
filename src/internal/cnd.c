#include <rlang.h>
#include "utils.h"


#define BUFSIZE 8192

#define INTERP(BUF, FMT, DOTS)                  \
  {                                             \
    va_list dots;                               \
    va_start(dots, FMT);                        \
    vsnprintf(BUF, BUFSIZE, FMT, dots);         \
    va_end(dots);                               \
                                                \
    BUF[BUFSIZE - 1] = '\0';                    \
  }

#include "decl/cnd-decl.h"


r_obj* ffi_format_error_arg(r_obj* arg) {
  switch (r_typeof(arg)) {
  case R_TYPE_symbol: arg = r_sym_as_utf8_character(arg); break;
  case R_TYPE_string: arg = r_str_as_character(arg); break;
  case R_TYPE_call: arg = r_as_label(arg); break;
  case R_TYPE_character: if (r_length(arg) == 1) break; else goto error;
  default: error: r_abort("`arg` must be a string or an expression.");
  }
  KEEP(arg);

  r_obj* out = r_eval_with_x(format_arg_call, arg, rlang_ns_env);

  FREE(1);
  return out;
}

const char* rlang_format_error_arg(r_obj* arg) {
  arg = KEEP(ffi_format_error_arg(arg));

  const char* arg_str = r_chr_get_c_string(arg, 0);
  int n = strlen(arg_str) + 1;

  // Uses the vmax protection stack.
  char* out = R_alloc(n, sizeof(char));
  r_memcpy(out, arg_str, n);

  FREE(1);
  return out;
}


struct without_winch_data {
  r_obj* old_on_error;
  r_obj* old_use_winch;
};
struct stop_internal_data {
  const char* file;
  int line;
  r_obj* call;
  const char* msg;
};

r_no_return
void rlang_stop_internal2(const char* file,
                          int line,
                          r_obj* call,
                          const char* fmt,
                          ...) {
  R_CheckStack2(BUFSIZE);
  char msg[BUFSIZE];
  INTERP(msg, fmt, ...);

  struct stop_internal_data stop_internal_data = {
    .file = file,
    .line = line,
    .call = call,
    .msg = msg
  };

  struct r_pair_callback with_winch_data = {
    .fn = &stop_internal_cb,
    .data = &stop_internal_data
  };
  struct without_winch_data without_winch_data = {
    .old_on_error = KEEP(r_peek_option("rlang_backtrace_on_error")),
    .old_use_winch = KEEP(r_peek_option("rlang_trace_use_winch"))
  };

  R_ExecWithCleanup(&with_winch, &with_winch_data,
                    &without_winch, &without_winch_data);

  r_abort("unreachable");
}

// For compatibility, exported as C callable `rlang_stop_internal`
r_no_return
void rlang_stop_internal(const char* fn,
                         const char* fmt,
                         ...) {
  R_CheckStack2(BUFSIZE);
  char msg[BUFSIZE];
  INTERP(msg, fmt, ...);

  r_obj* call = KEEP(r_call(r_sym(fn)));

  rlang_stop_internal2("", -1, call, msg);
  r_abort("unreachable");
}

static
r_no_return
r_obj* stop_internal_cb(void* payload) {
  struct stop_internal_data* data = (struct stop_internal_data*) payload;

  struct r_pair args[] = {
    { r_sym("file"), KEEP(r_chr(data->file)) },
    { r_sym("line"), KEEP(r_int(data->line)) },
    { r_sym("call"), data->call },
    { r_sym("message"), KEEP(r_chr(data->msg)) },
    { r_sym("frame"), KEEP(r_peek_frame()) }
  };

  r_exec_mask_n(r_null, r_sym("stop_internal_c_lib"),
                args, R_ARR_SIZEOF(args),
                rlang_ns_env);
  r_abort("unreachable");
}

static
r_obj* with_winch(void* payload) {
  r_poke_option("rlang_backtrace_on_error", r_chrs.full);
  r_poke_option("rlang_trace_use_winch", r_true);

  struct r_pair_callback* data = (struct r_pair_callback*) payload;
  return data->fn(data->data);
}
static
void without_winch(void* payload) {
  struct without_winch_data* data = (struct without_winch_data*) payload;

  r_poke_option("rlang_backtrace_on_error", data->old_on_error);
  r_poke_option("rlang_trace_use_winch", data->old_use_winch);
}

r_obj* ffi_test_stop_internal(void) {
  r_stop_internal("foo");
  return r_null;
}


// Probably should be implemented at R level
r_obj* ffi_new_condition(r_obj* class,
                         r_obj* msg,
                         r_obj* data) {
  if (msg == r_null) {
    msg = r_chrs.empty_string;
  } else if (r_typeof(msg) != R_TYPE_character) {
    const char* arg = r_format_error_arg(r_sym("message"));
    const char* what = r_obj_type_friendly(msg);
    r_abort("%s must be a character vector, not %s.", arg, what);
  }

  if (r_typeof(class) != R_TYPE_character) {
    const char* arg = r_format_error_arg(r_sym("class"));
    const char* what = r_obj_type_friendly(class);
    r_abort("%s must be a character vector, not %s.", arg, what);
  }

  r_ssize n_data = r_length(data);
  r_obj* cnd = KEEP(r_alloc_list(n_data + 1));

  r_list_poke(cnd, 0, msg);
  r_vec_poke_n(cnd, 1, data, 0, r_length(cnd) - 1);

  r_attrib_poke_names(cnd, KEEP(new_condition_names(data)));
  r_attrib_poke_class(cnd, KEEP(chr_append(class, KEEP(r_str("condition")))));

  if (Rf_any_duplicated(r_names(cnd), FALSE)) {
    r_abort("Condition fields can't have the same name.");
  }

  FREE(4);
  return cnd;
}
static
r_obj* new_condition_names(r_obj* data) {
  if (!r_is_named(data)) {
    r_abort("Conditions must have named data fields");
  }

  r_obj* data_nms = r_names(data);

  if (r_chr_has_any(data_nms, (const char* []) { "message", NULL })) {
    r_abort("Conditions can't have a `message` data field");
  }

  r_obj* nms = KEEP(r_alloc_character(r_length(data) + 1));
  r_chr_poke(nms, 0, r_str("message"));
  r_vec_poke_n(nms, 1, data_nms, 0, r_length(nms) - 1);

  FREE(1);
  return nms;
}

// `length` is no longer a valid argument
const char* rlang_obj_type_friendly_full(r_obj* x, bool value, bool _length) {
  r_obj* out_obj = KEEP(r_eval_with_xy(obj_type_friendly_call,
                                       x,
                                       r_lgl(value),
                                       rlang_ns_env));

  if (!r_is_string(out_obj)) {
    r_stop_unexpected_type(r_typeof(out_obj));
  }
  const char* out_str = r_chr_get_c_string(out_obj, 0);

  // Uses the vmax protection stack.
  int n = strlen(out_str) + 1;
  char* out = R_alloc(n, sizeof(char));
  r_memcpy(out, out_str, n);

  FREE(1);
  return out;
}

void rlang_init_cnd(r_obj* ns) {
  format_arg_call = r_parse("format_arg(x)");
  r_preserve(format_arg_call);

  obj_type_friendly_call = r_parse("obj_type_friendly(x, y)");
  r_preserve(obj_type_friendly_call);
}

static
r_obj* format_arg_call = NULL;

static
r_obj* obj_type_friendly_call = NULL;
