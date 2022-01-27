#include <rlang.h>

void* r_shelter_deref(r_obj* x) {
  enum r_type type = r_typeof(x);

  switch (type) {
  case R_TYPE_list:
    if (r_length(x) < 1) {
      r_abort("Shelter must have at least one element");
    }
    x = r_list_get(x, 0);
    type = r_typeof(x);
    break;
  case R_TYPE_pairlist:
    x = r_node_car(x);
    type = r_typeof(x);
    break;
  case R_TYPE_raw:
    break;
  default:
    r_stop_unimplemented_type(type);
  }

  if (type != R_TYPE_raw) {
    r_stop_unexpected_type(type);
  }

  return r_raw_begin(x);
}
