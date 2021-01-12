#include "rlang.h"

uint64_t (*r_xxh3_64bits)(const void*, size_t);

void r_init_library_vendor() {
  r_xxh3_64bits = (uint64_t (*)(const void*, size_t)) r_peek_c_callable("rlang", "rlang_xxh3_64bits");
}
