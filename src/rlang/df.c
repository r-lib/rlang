#include "rlang.h"

sexp* r_classes_data_frame = NULL;
sexp* r_classes_tibble = NULL;

#include "decl/df-decl.h"


sexp* r_alloc_df_list(r_ssize n_rows,
                      sexp* names,
                      const enum r_type* v_types,
                      r_ssize types_size) {
  sexp* out = KEEP(r_new_list(types_size));

  if (r_typeof(names) != r_type_character) {
    r_abort("`names` must be a character vector.");
  }
  if (r_length(names) != types_size) {
    r_abort("`names` must match the number of columns.");
  }
  r_attrib_push(out, r_syms_names, names);

  for (r_ssize i = 0; i < types_size; ++i) {
    sexp* col = r_new_vector(v_types[i], n_rows);
    r_list_poke(out, i, col);
  }

  FREE(1);
  return out;
}


void r_init_data_frame(sexp* x, r_ssize n_rows) {
  init_compact_rownames(x, n_rows);
  r_attrib_poke(x, r_syms_class, r_classes_data_frame);
}
void r_init_tibble(sexp* x, r_ssize n_rows) {
  r_init_data_frame(x, n_rows);
  r_attrib_poke(x, r_syms_class, r_classes_tibble);
}

static
void init_compact_rownames(sexp* x, r_ssize n_rows) {
  sexp* rn = KEEP(new_compact_rownames(n_rows));
  r_attrib_poke(x, r_syms_row_names, rn);
  FREE(1);
}

static
sexp* new_compact_rownames(r_ssize n_rows) {
  if (n_rows <= 0) {
    return r_ints_empty;
  }

  sexp* out = r_new_integer(2);
  int* p_out = r_int_deref(out);
  p_out[0] = r_ints_na;
  p_out[1] = -n_rows;

  return out;
}


void r_init_library_df() {
  r_classes_data_frame = r_preserve_global(r_chr("data.frame"));

  const char* v_tibble_class[] = { "tbl_df", "tbl", "data.frame" };
  r_classes_tibble = r_chr_n(v_tibble_class, R_ARR_SIZEOF(v_tibble_class));
  r_preserve_global(r_classes_tibble);
}
