#ifndef RLANG_PARSE_H
#define RLANG_PARSE_H


r_obj* r_parse(const char* str);
r_obj* r_parse_eval(const char* str, r_obj* env);


#endif
