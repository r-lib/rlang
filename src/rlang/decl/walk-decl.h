
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
                          enum r_node_raw_relation rel,
                          r_ssize i,
                          sexp_iterator_fn* it,
                          void* data);
