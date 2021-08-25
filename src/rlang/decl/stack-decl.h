// From env.c
r_obj* rlang_ns_get(const char* name);

static r_obj* peek_frame_call;
static r_obj* caller_env_call;
