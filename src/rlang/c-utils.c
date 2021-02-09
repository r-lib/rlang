#include <rlang.h>

void* r_shelter_deref(sexp* x) {
  enum r_type type = r_typeof(x);

  switch (type) {
  case r_type_list:
    if (r_length(x) < 1) {
      r_abort("Shelter must have at least one element");
    }
    x = r_list_get(x, 0);
    type = r_typeof(x);
    break;
  case r_type_pairlist:
    x = r_node_car(x);
    type = r_typeof(x);
    break;
  case r_type_raw:
    break;
  default:
    r_stop_unimplemented_type("r_shelter_deref", type);
  }

  if (type != r_type_raw) {
    r_stop_unexpected_type("r_shelter_deref", type);
  }

  return r_raw_deref(x);
}
