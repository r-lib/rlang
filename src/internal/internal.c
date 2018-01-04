#include <rlang.h>
#include "internal.h"

void rlang_init_internal() {
  overscope_sym = r_sym("_tidyeval_overscope");
}
