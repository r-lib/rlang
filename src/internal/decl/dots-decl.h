r_obj* rlang_ns_get(const char* name);

static
bool should_auto_name(r_obj* named);

static
int arg_match_ignore_empty(r_obj* ignore_empty);

static
enum dots_homonyms arg_match_homonyms(r_obj* homonyms);
