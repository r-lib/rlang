r_obj* names_as_unique(r_obj* names, bool quiet);

static
bool any_has_suffix(r_obj* names);

static
bool is_unique_names(r_obj* names);

static
ptrdiff_t suffix_pos(const char* name);

static
bool needs_suffix(r_obj* str);

static
void names_inform_repair(r_obj* old_names, r_obj* new_names);

static
void stop_large_name(void);

static
bool is_dotdotint(const char* name);
