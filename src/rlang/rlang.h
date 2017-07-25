#ifndef RLANG_RLANG_H
#define RLANG_RLANG_H


#define R_NO_REMAP
#include <stdbool.h>
#include <Rinternals.h>

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
#include "vector.h"
#include "vector-chr.h"
#include "vector-lgl.h"
#include "vector-list.h"

#define KEEP PROTECT
#define FREE UNPROTECT


#endif
