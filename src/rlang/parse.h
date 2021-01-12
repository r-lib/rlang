#ifndef RLANG_PARSE_H
#define RLANG_PARSE_H


sexp* r_parse(const char* str);
sexp* r_parse_eval(const char* str, sexp* env);


#endif
