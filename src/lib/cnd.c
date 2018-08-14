#include "rlang.h"

sexp* rlang_ns_get(const char* name);


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


void r_inform(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  sexp* buf_chr = KEEP(r_scalar_chr(buf));
  sexp* lang = KEEP(r_build_call_node(r_sym("message"), buf_chr));
  r_eval(lang, R_BaseEnv);
  FREE(2);
}
void r_warn(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  sexp* args = KEEP(r_build_node(Rf_ScalarLogical(0), r_null));
  r_node_poke_tag(args, r_sym("call."));

  args = KEEP(r_build_node(r_scalar_chr(buf), args));
  sexp* lang = KEEP(r_build_call_node(r_sym("warning"), args));

  r_eval(lang, R_BaseEnv);
  FREE(3);
}
void r_abort(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  Rf_errorcall(r_null, buf);

  while (1); // No return
}
sexp* r_interp_str(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  return r_scalar_chr(buf);
}

static sexp* new_condition_names(sexp* data) {
  if (!r_is_named(data)) {
    r_abort("Conditions must have named data fields");
  }

  sexp* data_nms = r_vec_names(data);

  if (r_chr_has(data_nms, "message")) {
    r_abort("Conditions can't have a `message` data field");
  }

  sexp* nms = KEEP(r_new_vector(STRSXP, r_length(data) + 1));
  r_chr_poke(nms, 0, r_string("message"));
  r_vec_poke_n(nms, 1, data_nms, 0, r_length(nms) - 1);

  FREE(1);
  return nms;
}
sexp* r_new_condition(sexp* type, sexp* data, sexp* msg) {
  if (!r_is_null(msg) && !r_is_scalar_character(msg)) {
    r_abort("Condition message must be a string");
  }

  r_ssize_t n_data = r_length(data);
  sexp* cnd = KEEP(r_new_vector(VECSXP, n_data + 1));

  r_list_poke(cnd, 0, msg);
  r_vec_poke_n(cnd, 1, data, 0, r_length(cnd) - 1);

  r_poke_names(cnd, KEEP(new_condition_names(data)));
  r_poke_class(cnd, KEEP(chr_append(type, KEEP(r_string("condition")))));

  FREE(4);
  return cnd;
}

static sexp* with_muffle_lang(sexp* signal) {
  static sexp* muffle_node = NULL;
  if (!muffle_node) {
    muffle_node = r_build_pairlist(rlang_ns_get("muffle"));
    R_PreserveObject(muffle_node);
    r_node_poke_tag(muffle_node, r_sym("muffle"));
  }

  sexp* args = KEEP(r_build_node(signal, muffle_node));
  sexp* lang = r_build_call_node(r_sym("withRestarts"), args);

  FREE(1);
  return lang;
}
static void cnd_signal_impl(const char* signaller, sexp* cnd, bool mufflable) {
  int n_protect = 0;

  if (r_typeof(cnd) == STRSXP) {
    cnd = KEEP_N(r_new_condition(cnd, r_null, r_null), n_protect);
  } else if (!r_is_condition(cnd)) {
    r_abort("`cnd` must be a condition");
  }

  sexp* lang = KEEP_N(r_build_call1(r_sym(signaller), cnd), n_protect);

  if (mufflable) {
    sexp* muffable_str = KEEP_N(r_string("mufflable"), n_protect);
    sexp* classes = KEEP_N(chr_prepend(r_get_class(cnd), muffable_str), n_protect);
    SETCADR(lang, r_set_class(cnd, classes));

    lang = KEEP_N(with_muffle_lang(lang), n_protect);
  }

  r_eval(lang, R_BaseEnv);
  FREE(n_protect);
}

void r_cnd_signal(sexp* cnd, bool mufflable) {
  cnd_signal_impl("signalCondition", cnd, mufflable);
}
void r_cnd_inform(sexp* cnd, bool mufflable) {
  cnd_signal_impl("message", cnd, mufflable);
}
void r_cnd_warn(sexp* cnd, bool mufflable) {
  cnd_signal_impl("warning", cnd, mufflable);
}
void r_cnd_abort(sexp* cnd, bool mufflable) {
  cnd_signal_impl("stop", cnd, mufflable);
}
