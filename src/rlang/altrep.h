// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_ALTREP_H
#define RLANG_ALTREP_H

#include "rlang-types.h"

#if (R_VERSION < R_Version(3, 5, 0)) ||                 \
  (defined(_WIN32) && R_VERSION == R_Version(3, 5, 0))
# define R_HAS_ALTREP 0
#else
# define R_HAS_ALTREP 1
#endif

#if !R_HAS_ALTREP
# define ALTREP(x) false
#endif


#endif
