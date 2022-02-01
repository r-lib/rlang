static
r_obj* c_fn;
static
r_obj* as_character_call;
static r_obj* names_call;
static r_obj* set_names_call;
static r_obj* length_call;

static
r_obj* node_names(r_obj* x);

static
r_obj* names_dispatch(r_obj* x, r_obj* env);

static inline
r_obj* eval_fn_dots(r_obj* fn, r_obj* x, r_obj* dots, r_obj* env);

static inline
r_obj* eval_as_character(r_obj* x, r_obj* env);

static inline
r_obj* set_names_dispatch(r_obj* x, r_obj* nm, r_obj* env);

static inline
r_ssize length_dispatch(r_obj* x, r_obj* env);
