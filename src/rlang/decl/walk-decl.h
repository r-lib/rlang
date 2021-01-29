
struct sexp_stack;

static
struct sexp_stack* new_sexp_stack();

static inline
struct sexp_stack_info sexp_stack_pop(struct sexp_stack* p_stack);

static
void sexp_stack_push(struct sexp_stack* p_stack, struct sexp_stack_info info);

static
bool sexp_iterate_recurse(struct sexp_stack* p_stack,
                          sexp* x,
                          int depth,
                          sexp* parent,
                          enum r_node_relation rel,
                          r_ssize i,
                          sexp_iterator_fn* it,
                          void* data);

static inline
sexp* sexp_node_attrib(sexp* x, enum r_type type);

static inline
sexp* sexp_node_car(sexp* x, enum r_type type,
                    enum r_node_relation* p_rel);

static inline
sexp* sexp_node_cdr(sexp* x, enum r_type type,
                    enum r_node_relation* p_rel);

static inline
sexp* sexp_node_tag(sexp* x, enum r_type type,
                    enum r_node_relation* p_rel);

static inline
sexp* const * sexp_node_arr(sexp* x, enum r_type type,
                            enum r_node_relation* p_rel);
