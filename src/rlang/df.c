#include "rlang.h"

sexp* r_classes_data_frame = NULL;
sexp* r_classes_tibble = NULL;

#include "decl/df-decl.h"


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
