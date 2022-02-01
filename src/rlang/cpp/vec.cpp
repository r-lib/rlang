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

}
