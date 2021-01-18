
struct sexp_stack;

static struct sexp_stack* new_sexp_stack();
static inline struct sexp_stack_info sexp_stack_pop(struct sexp_stack* p_stack);
static void sexp_stack_push(struct sexp_stack* p_stack, struct sexp_stack_info info);
