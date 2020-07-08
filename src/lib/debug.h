#ifndef RLANG_DEBUG_H
#define RLANG_DEBUG_H


#define r_printf Rprintf
void r_sexp_inspect(sexp* x);
void r_browse(SEXP x);
void r_browse_at(sexp* env);


#endif
