static r_obj* stop_arg_match_call;

static
enum r_type arg_match_arg_nm_type(r_obj* arg_nm);

static
r_obj* wrap_chr(r_obj* arg);

static
int arg_match1(r_obj* arg,
               r_obj* values,
               r_obj* error_arg,
               r_obj* error_call);
