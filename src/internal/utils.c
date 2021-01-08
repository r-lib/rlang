#include <rlang.h>


sexp* new_preserved_empty_list() {
  sexp* empty_list = r_new_vector(r_type_list, 0);
  r_mark_precious(empty_list);
  r_mark_shared(empty_list);

  sexp* nms = KEEP(r_new_vector(r_type_character, 0));
  r_poke_names(empty_list, nms);
  FREE(1);

  return empty_list;
}


/* For debugging with gdb or lldb. Exported as a C callable.
 * Usage with lldb:
 *
 * ```
 * // Full backtrace:
 * expr R_GetCCallable("rlang", "rlang_print_backtrace")(true)
 *
 * // Linear backtrace:
 * expr R_GetCCallable("rlang", "rlang_print_backtrace")(false)
 * ```
 */
void rlang_print_backtrace(bool full) {
  sexp* env = KEEP(r_current_frame());
  sexp* trace = KEEP(r_parse_eval("rlang::trace_back()", env));

  const char* source = full ?
    "print(x, simplify = 'none')" :
    "print(x, simplify = 'branch')";
  sexp* call = KEEP(r_parse(source));

  r_eval_with_x(call, r_base_env, trace);

  FREE(3);
  return;
}


static sexp* signal_soft_deprecated_call = NULL;
void signal_soft_deprecated(const char* msg,
                            const char* id,
                            sexp* env) {
  id = id ? id : msg;
  env = env ? env : r_empty_env;
  if (!msg) {
    r_abort("Internal error: NULL `msg` in r_signal_soft_deprecated()");
  }

  sexp* msg_ = KEEP(r_chr(msg));
  sexp* id_ = KEEP(r_chr(id));

  r_eval_with_xyz(signal_soft_deprecated_call, r_base_env, msg_, id_, env);

  FREE(2);
}

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

static void signal_retirement(const char* source, const char* buf);
static sexp* warn_deprecated_call = NULL;

void warn_deprecated(const char* id, const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);
  sexp* msg_ = KEEP(r_chr(buf));

  id = id ? id : buf;
  sexp* id_ = KEEP(r_chr(id));

  r_eval_with_xy(warn_deprecated_call, r_base_env, msg_, id_);
  FREE(2);
}

void stop_defunct(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  signal_retirement("stop_defunct(msg = x)", buf);

  r_abort("Internal error: Unexpected return after `.Defunct()`");
}

static void signal_retirement(const char* source, const char* buf) {
  sexp* call = KEEP(r_parse(source));
  sexp* msg = KEEP(r_chr(buf));

  r_eval_with_x(call, r_ns_env("rlang"), msg);

  FREE(2);
}

void rlang_init_utils() {
  warn_deprecated_call = r_parse("rlang:::warn_deprecated(x, id = y)");
  r_mark_precious(warn_deprecated_call);

  signal_soft_deprecated_call = r_parse("rlang:::signal_soft_deprecated(x, id = y, env = z)");
  r_mark_precious(signal_soft_deprecated_call);
}
