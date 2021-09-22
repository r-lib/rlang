static r_obj* cnd_signal_call;

// From vec-chr.c
r_obj* chr_append(r_obj* chr, r_obj* r_string);

// From rlang/vec.c
void r_vec_poke_n(r_obj* x, r_ssize offset,
                  r_obj* y, r_ssize from, r_ssize n);

static
r_obj* new_condition_names(r_obj* data);
