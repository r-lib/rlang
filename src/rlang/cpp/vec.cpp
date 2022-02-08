#include <rlang.hpp>
#include <algorithm>

extern "C" {

int* r_int_unique0(int* v_data, r_ssize size) {
  try {
    return std::unique(v_data, v_data + size);
  } catch (...) {
    rcc_abort("r_int_unique0");
  }
}

bool r_list_all_of0(r_obj* const * v_first,
                    r_ssize size,
                    bool (*predicate)(r_obj* x)) {
  try {
    return std::all_of(v_first, v_first + size, predicate);
  } catch (...) {
    rcc_abort("r_list_all_of");
  }
}

}
