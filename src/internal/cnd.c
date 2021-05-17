#include "rlang.h"


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


struct without_winch_data {
  r_obj* old_on_error;
  r_obj* old_use_winch;
};
struct stop_internal_data {
  const char* fn;
  const char* msg;
};

__attribute__((noreturn))
void rlang_stop_internal(const char* fn, const char* fmt, ...) {
  R_CheckStack2(BUFSIZE);

  char msg[BUFSIZE];
  INTERP(msg, fmt, ...);

  struct stop_internal_data stop_internal_data = {
    .fn = fn,
    .msg = msg
  };

  if (!r_is_installed("winch")) {
    stop_internal_cb(&stop_internal_data);
  }

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

  r_abort("Unreached.");
}

static
__attribute__((noreturn))
r_obj* stop_internal_cb(void* payload) {
  struct stop_internal_data* data = (struct stop_internal_data*) payload;
  r_abort("Internal error in `%s()`: %s", data->fn, data->msg);
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
