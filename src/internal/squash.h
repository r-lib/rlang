#ifndef RLANG_INTERNAL_SQUASH_H
#define RLANG_INTERNAL_SQUASH_H

#include <rlang.h>


r_obj* r_squash_if(r_obj* dots, enum r_type kind, bool (*is_spliceable)(r_obj*), int depth);


#endif
