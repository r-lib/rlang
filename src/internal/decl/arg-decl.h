static r_obj* stop_arg_match_call;

static
enum r_type arg_match_arg_nm_type(r_obj* arg_nm);

static
r_obj* wrap_chr(r_obj* arg);

static
r_obj* lazy_wrap_chr(struct r_lazy arg);

static
int arg_match1(r_obj* arg,
               r_obj* values,
               struct r_lazy error_arg,
               struct r_lazy error_call);
