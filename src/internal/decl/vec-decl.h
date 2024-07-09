static
bool list_match(r_obj* const * v_x,
                r_ssize n,
                r_obj* value,
                enum option_bool match);

r_obj* rlang_ns_get(const char* name);

// From rlang/vec.c
void r_vec_poke_n(r_obj* x, r_ssize offset,
                  r_obj* y, r_ssize from, r_ssize n);
