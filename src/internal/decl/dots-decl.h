static r_obj* as_label_call;
static r_obj* empty_spliced_arg;
static r_obj* splice_box_attrib;
static r_obj* quosures_attrib;
static r_obj* auto_name_call;
static r_obj* glue_unquote_fn;

r_obj* rlang_ns_get(const char* name);

static
bool should_auto_name(r_obj* named);

static
int arg_match_ignore_empty(r_obj* ignore_empty);

static
enum dots_homonyms arg_match_homonyms(r_obj* homonyms);
