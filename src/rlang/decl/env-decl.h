r_obj* eval_with_x(r_obj* call, r_obj* x);
r_obj* eval_with_xy(r_obj* call, r_obj* x, r_obj* y);
r_obj* eval_with_xyz(r_obj* call, r_obj* x, r_obj* y, r_obj* z);

#if R_VERSION < R_Version(4, 1, 0)
static
r_obj* new_env_call;

static
r_obj* new_env__parent_node;

static
r_obj* new_env__size_node;
#endif

static
r_obj* exists_call;

static
r_obj* remove_call;

static
r_obj* poke_lazy_call;

static
r_obj* poke_lazy_value_node;


static
r_obj* env2list_call;

static
r_obj* list2env_call;

static
r_obj* missing_prim;

#if R_VERSION < R_Version(4, 0, 0)
static
r_obj* env_as_list_compat(r_obj* env, r_obj* out);
#endif

static
void env_coalesce_plain(r_obj* env, r_obj* from, r_obj* nms);
