#ifndef RLANG_CND_H
#define RLANG_CND_H

#include <stdbool.h>


void r_inform(const char* fmt, ...);
void r_warn(const char* fmt, ...);
void r_abort(const char* fmt, ...);

SEXP interp_str(const char* fmt, ...);

SEXP new_condition(SEXP type, SEXP data, SEXP msg);

void cnd_signal(SEXP cnd, bool mufflable);
void cnd_signal_error(SEXP cnd, bool mufflable);


#endif
