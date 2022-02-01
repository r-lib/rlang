#include "rlang.h"
#include "decl/df-decl.h"


r_obj* r_alloc_df_list(r_ssize n_rows,
                       r_obj* names,
                       const enum r_type* v_types,
                       r_ssize types_size) {
  r_obj* out = KEEP(r_alloc_list(types_size));

  if (r_typeof(names) != R_TYPE_character) {
    r_abort("`names` must be a character vector.");
  }
  if (r_length(names) != types_size) {
    r_abort("`names` must match the number of columns.");
  }
  r_attrib_push(out, r_syms.names, names);

  for (r_ssize i = 0; i < types_size; ++i) {
    // A nil type stands for no column allocation
    enum r_type type = v_types[i];
    if (type != R_TYPE_null) {
      r_obj* col = r_alloc_vector(type, n_rows);
      r_list_poke(out, i, col);
    }
  }

  FREE(1);
  return out;
}


void r_init_data_frame(r_obj* x, r_ssize n_rows) {
  init_compact_rownames(x, n_rows);
  r_attrib_poke(x, r_syms.class_, r_classes.data_frame);
}
void r_init_tibble(r_obj* x, r_ssize n_rows) {
  r_init_data_frame(x, n_rows);
  r_attrib_poke(x, r_syms.class_, r_classes.tibble);
}

static
void init_compact_rownames(r_obj* x, r_ssize n_rows) {
  r_obj* rn = KEEP(new_compact_rownames(n_rows));
  r_attrib_poke(x, r_syms.row_names, rn);
  FREE(1);
}

static
r_obj* new_compact_rownames(r_ssize n_rows) {
  if (n_rows <= 0) {
    return r_globals.empty_int;
  }

  r_obj* out = r_alloc_integer(2);
  int* p_out = r_int_begin(out);
  p_out[0] = r_globals.na_int;
  p_out[1] = -n_rows;

  return out;
}
