#include "rlang-types.h"
#include "sym.h"


struct r_globals_classes r_classes;
struct r_globals_syms r_syms;


void r_init_library_globals() {
  r_classes.data_frame = r_preserve_global(r_chr("data.frame"));

  const char* v_tibble_class[] = { "tbl_df", "tbl", "data.frame" };
  r_classes.tibble = r_chr_n(v_tibble_class, R_ARR_SIZEOF(v_tibble_class));
  r_preserve_global(r_classes.tibble);
}

void r_init_library_globals_syms() {
  r_syms.class = R_ClassSymbol;
  r_syms.colon2 = R_DoubleColonSymbol;
  r_syms.colon3 = R_TripleColonSymbol;
  r_syms.dots = R_DotsSymbol;
  r_syms.missing = R_MissingArg;
  r_syms.names = R_NamesSymbol;
  r_syms.row_names = R_RowNamesSymbol;
  r_syms.unbound = R_UnboundValue;

  r_syms.dot_environment = r_sym(".Environment");
  r_syms.dot_fn = r_sym(".fn");
  r_syms.dot_x = r_sym(".x");
  r_syms.dot_y = r_sym(".y");
  r_syms.function = r_sym("function");
  r_syms.srcref = r_sym("srcref");
  r_syms.tilde = r_sym("~");
  r_syms.w = r_sym("w");
  r_syms.x = r_sym("x");
  r_syms.y = r_sym("y");
  r_syms.z = r_sym("z");
}
