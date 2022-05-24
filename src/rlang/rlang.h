#ifndef RLANG_RLANG_H
#define RLANG_RLANG_H

/*
 * `_ISOC99_SOURCE` is defined to avoid warnings on Windows UCRT builds where
 * usage of `PRIx64` in Microsoft's `printf()` can generate the warnings shown
 * below. Defining this before including `<stdio.h>` forces usage of MinGW's
 * custom `printf()`, which is C99 compliant.
 * warning: unknown conversion type character 'l' in format [-Wformat]
 * warning: too many arguments for format [-Wformat-extra-args]
 *
 * The conventional define for this is `__USE_MINGW_ANSI_STDIO`, but according
 * to the thread below it is recommended to instead use a feature test macro
 * (such as `_ISOC99_SOURCE`) which will indirectly define the internal
 * `__USE_MINGW_ANSI_STDIO` macro for us.
 * https://osdn.net/projects/mingw/lists/archive/users/2019-January/000199.html
 */
#ifndef _ISOC99_SOURCE
#define _ISOC99_SOURCE
#endif
#include <stdio.h>

#define R_NO_REMAP
#include <Rinternals.h>
#include <Rversion.h>
#include <inttypes.h>
#include <stdbool.h>
#include "rlang-types.h"


r_obj* r_init_library(r_obj* ns);

r_ssize r_arg_as_ssize(r_obj* n, const char* arg);

static inline
r_ssize r_as_ssize(r_obj* n) {
  return r_arg_as_ssize(n, "n");
}

extern
bool _r_use_local_precious_list;


#include "obj.h"
#include "globals.h"

#include "altrep.h"
#include "arg.h"
#include "attrib.h"
#include "debug.h"
#include "c-utils.h"
#include "call.h"
#include "cnd.h"
#include "dict.h"
#include "df.h"
#include "dyn-array.h"
#include "dyn-list-of.h"
#include "env.h"
#include "env-binding.h"
#include "eval.h"
#include "export.h"
#include "fn.h"
#include "formula.h"
#include "node.h"
#include "parse.h"
#include "quo.h"
#include "session.h"
#include "stack.h"
#include "state.h"
#include "sym.h"
#include "vec.h"
#include "vec-chr.h"
#include "vec-lgl.h"
#include "vendor.h"
#include "walk.h"


#define r_abort_lazy_call(LAZY, ...) \
  r_abort_call(KEEP(r_lazy_eval(LAZY)), __VA_ARGS__)


#endif
