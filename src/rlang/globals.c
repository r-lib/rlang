#include "rlang-types.h"
#include "sym.h"


struct r_globals r_globals;
struct r_globals_chrs r_chrs;
struct r_globals_classes r_classes;
struct r_globals_syms r_syms;

sexp* r_true = NULL;
sexp* r_false = NULL;


void r_init_library_globals() {
  r_classes.data_frame = r_preserve_global(r_chr("data.frame"));

  const char* v_tibble_class[] = { "tbl_df", "tbl", "data.frame" };
  r_classes.tibble = r_chr_n(v_tibble_class, R_ARR_SIZEOF(v_tibble_class));
  r_preserve_global(r_classes.tibble);

  r_chrs.empty_string = r_preserve_global(r_chr(""));
  r_globals.empty_str = r_chr_get(r_chrs.empty_string, 0);

  r_globals.empty_lgl = r_preserve_global(r_alloc_logical(0));
  r_globals.empty_int = r_preserve_global(r_alloc_integer(0));
  r_globals.empty_dbl = r_preserve_global(r_alloc_double(0));
  r_globals.empty_cpl = r_preserve_global(r_alloc_complex(0));
  r_globals.empty_raw = r_preserve_global(r_alloc_raw(0));
  r_globals.empty_chr = r_preserve_global(r_alloc_character(0));
  r_globals.empty_list = r_preserve_global(r_alloc_list(0));

  r_globals.na_lgl = NA_LOGICAL;
  r_globals.na_int = NA_INTEGER;
  r_globals.na_dbl = NA_REAL;
  r_globals.na_str = NA_STRING;

  r_false = r_preserve_global(r_lgl(0));
  r_true = r_preserve_global(r_lgl(1));
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
