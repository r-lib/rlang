static
r_obj* with_winch(void* payload);

static
void without_winch(void* payload);

static
__attribute__((noreturn))
r_obj* stop_internal_cb(void* payload);


static
r_obj* format_arg_call;
