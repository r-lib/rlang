#include <rlang.h>
#include "globals.h"

struct syms syms;

void rlang_init_globals(r_obj* ns) {
  syms.arg_nm = r_sym("arg_nm");
}
