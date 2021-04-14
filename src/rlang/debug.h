#ifndef RLANG_DEBUG_H
#define RLANG_DEBUG_H


#define r_printf Rprintf
void r_sexp_inspect(r_obj* x);
void r_browse(r_obj* x);
void r_browse_at(r_obj* env);
void r_dbg_str(r_obj* x);


#endif
