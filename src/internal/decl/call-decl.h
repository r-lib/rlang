static
bool call_is_namespaced(r_obj* x, r_obj* ns);

static inline
r_obj* call_unnamespace(r_obj* x);

static
bool is_callable(r_obj* x);

static
void call_zap_inline(r_obj* x);

static
void node_zap_inline(r_obj* x);

static
r_obj* call_zap_one(r_obj* x);

static
void call_zap_fn(r_obj* x);

static
r_obj* type_sum(r_obj* x);

static
r_obj* type_sum_call;
