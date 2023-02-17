#include "rlang-types.h"
#include "sym.h"


struct r_globals r_globals;
struct r_globals_chrs r_chrs;
struct r_globals_classes r_classes;
struct r_globals_strs r_strs;
struct r_globals_syms r_syms;
struct r_globals_envs r_envs;

r_obj* r_true = NULL;
r_obj* r_false = NULL;


void r_init_library_globals(r_obj* ns) {
  r_preserve_global(r_classes.data_frame = r_chr("data.frame"));

  const char* v_tibble_class[] = { "tbl_df", "tbl", "data.frame" };
  r_preserve_global(r_globals.empty_lgl = r_alloc_logical(0));
  r_preserve_global(r_globals.empty_int = r_alloc_integer(0));
  r_preserve_global(r_globals.empty_dbl = r_alloc_double(0));
  r_preserve_global(r_globals.empty_cpl = r_alloc_complex(0));
  r_preserve_global(r_globals.empty_raw = r_alloc_raw(0));
  r_preserve_global(r_globals.empty_chr = r_alloc_character(0));
  r_preserve_global(r_globals.empty_list = r_alloc_list(0));

  r_globals.na_lgl = NA_LOGICAL;
  r_globals.na_int = NA_INTEGER;
  r_globals.na_dbl = NA_REAL;
  r_globals.na_cpl = (r_complex) { NA_REAL, NA_REAL };
  r_globals.na_str = NA_STRING;

  r_preserve_global(r_chrs.empty_string = r_chr(""));
  r_preserve_global(r_chrs.full = r_chr("full"));

  r_classes.tibble = r_chr_n(v_tibble_class, R_ARR_SIZEOF(v_tibble_class));
  r_preserve_global(r_classes.tibble);

  r_strs.dots = r_sym_string(r_syms.dots);
  r_strs.condition = r_sym_string(r_syms.condition);
  r_strs.empty = r_chr_get(r_chrs.empty_string, 0);
  r_strs.error = r_sym_string(r_syms.error);
  r_strs.interrupt = r_sym_string(r_syms.interrupt);
  r_strs.na = r_globals.na_str;
  r_strs.message = r_sym_string(r_syms.message);
  r_strs.warning = r_sym_string(r_syms.warning);

  r_preserve_global(r_false = r_lgl(0));
  r_preserve_global(r_true = r_lgl(1));

  r_envs.empty = R_EmptyEnv;
  r_envs.base = R_BaseEnv;
  r_envs.global = R_GlobalEnv;
  r_envs.ns = ns;
}

void r_init_library_globals_syms(void) {
  r_syms.abort = r_sym("abort");
  r_syms.arg = r_sym("arg");
  r_syms.brace = R_BraceSymbol;
  r_syms.brackets = R_BracketSymbol;
  r_syms.brackets2 = R_Bracket2Symbol;
  r_syms.call = r_sym("call");
  r_syms.class_ = R_ClassSymbol;
  r_syms.colon2 = R_DoubleColonSymbol;
  r_syms.colon3 = R_TripleColonSymbol;
  r_syms.condition = r_sym("condition");
  r_syms.dots = R_DotsSymbol;
  r_syms.error = r_sym("error");
  r_syms.error_arg = r_sym("error_arg");
  r_syms.error_call = r_sym("error_call");
  r_syms.error_call_flag = r_sym(".__error_call__.");
  r_syms.expr = r_sym("expr");
  r_syms.interrupt = r_sym("interrupt");
  r_syms.missing = R_MissingArg;
  r_syms.message = r_sym("message");
  r_syms.names = R_NamesSymbol;
  r_syms.options = r_sym("options");
  r_syms.dim = R_DimSymbol;
  r_syms.dim_names = R_DimNamesSymbol;
  r_syms.row_names = R_RowNamesSymbol;
  r_syms.stack_overflow_error = r_sym("stackOverflowError");
  r_syms.unbound = R_UnboundValue;
  r_syms.warning = r_sym("warning");

  r_syms.dot_environment = r_sym(".Environment");
  r_syms.dot_fn = r_sym(".fn");
  r_syms.dot_x = r_sym(".x");
  r_syms.dot_y = r_sym(".y");
  r_syms.function = r_sym("function");
  r_syms.srcfile = r_sym("srcfile");
  r_syms.srcref = r_sym("srcref");
  r_syms.tilde = r_sym("~");
  r_syms.w = r_sym("w");
  r_syms.wholeSrcref = r_sym("wholeSrcref");
  r_syms.x = r_sym("x");
  r_syms.y = r_sym("y");
  r_syms.z = r_sym("z");
}
