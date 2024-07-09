// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_SESSION_H
#define RLANG_SESSION_H

#include "rlang-types.h"

bool r_is_installed(const char* pkg);
bool r_has_colour(void);
r_obj* r_getppid(void);


#endif
