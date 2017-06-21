#ifndef RLANG_RLANG_H
#define RLANG_RLANG_H


#define R_NO_REMAP
#include <stdbool.h>
#include <Rinternals.h>

#include "utils.h"
#include "sym.h"
#include "formula.h"
#include "quo.h"
#include "env.h"
#include "node.h"
#include "lang.h"
#include "sexp.h"
#include "vector.h"
#include "vector-chr.h"
#include "vector-list.h"
#include "eval.h"
#include "cnd.h"
#include "stack.h"

#define KEEP PROTECT
#define FREE UNPROTECT


#endif
