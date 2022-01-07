static
r_obj* with_winch(void* payload);

static
void without_winch(void* payload);

static
__attribute__((noreturn))
r_obj* stop_internal_cb(void* payload);

// From rlang/vec-chr.c
r_obj* chr_append(r_obj* chr, r_obj* r_string);

// From rlang/vec.c
void r_vec_poke_n(r_obj* x, r_ssize offset,
                  r_obj* y, r_ssize from, r_ssize n);

static
r_obj* new_condition_names(r_obj* data);

static
r_obj* format_arg_call;

static
r_obj* friendly_type_of_call;
