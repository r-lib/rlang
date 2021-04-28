#ifndef RLANG_INTERNAL_FILE_H
#define RLANG_INTERNAL_FILE_H

#include <stdio.h>

FILE* r_fopen(r_obj* path, const char* mode);

#endif
