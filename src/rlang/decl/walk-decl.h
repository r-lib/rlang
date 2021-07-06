static inline
enum sexp_iterator_type sexp_iterator_type(enum r_type type,
                                           r_obj* x);

static inline
r_obj* sexp_node_attrib(enum r_type type, r_obj* x);

static inline
r_obj* sexp_node_car(enum r_type type,
                     r_obj* x,
                     enum r_sexp_it_relation* p_rel);

static inline
r_obj* sexp_node_cdr(enum r_type type,
                     r_obj* x,
                     enum r_sexp_it_relation* p_rel);

static inline
r_obj* sexp_node_tag(enum r_type type,
                     r_obj* x,
                     enum r_sexp_it_relation* p_rel);

static inline
void init_incoming_stack_info(struct sexp_stack_info* p_info,
                              enum sexp_iterator_type it_type,
                              bool has_attrib);

static
bool sexp_next_incoming(struct r_sexp_iterator* p_it,
                        struct sexp_stack_info* p_info);
