#ifndef RLANG_RLANG_H
#define RLANG_RLANG_H


#define R_NO_REMAP
#include <stdbool.h>
#include <Rinternals.h>

typedef struct SEXPREC sexp;
typedef R_len_t r_size_t;
typedef Rbyte r_byte_t;
typedef Rcomplex r_complex_t;

#include "cnd.h"
#include "env.h"
#include "eval.h"
#include "formula.h"
#include "lang.h"
#include "node.h"
#include "quo.h"
#include "sexp.h"
#include "stack.h"
#include "sym.h"
#include "utils.h"
#include "vec.h"
#include "vec-chr.h"
#include "vec-lgl.h"
#include "vec-list.h"

#define KEEP PROTECT
#define FREE UNPROTECT


#endif
