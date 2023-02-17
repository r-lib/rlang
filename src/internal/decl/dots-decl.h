static r_obj* auto_name_call;
static r_obj* dots_homonyms_values;
static r_obj* dots_ignore_empty_values;
static r_obj* empty_spliced_arg;
static r_obj* glue_embrace_fn;
static r_obj* quosures_attrib;
static r_obj* splice_box_attrib;
static r_obj* abort_dots_homonyms_ns_sym;

static struct r_lazy dots_homonyms_arg;
static struct r_lazy dots_ignore_empty_arg;

r_obj* rlang_ns_get(const char* name);

static
enum dots_ignore_empty arg_match_ignore_empty(r_obj* ignore_empty);

static
enum dots_homonyms arg_match_homonyms(r_obj* homonyms);

static
enum arg_named arg_match_named(r_obj* named);

static inline
bool should_ignore(struct dots_capture_info* p_capture_info,
                   r_obj* expr,
                   r_obj* name,
                   bool last);

static inline
void ignore(struct dots_capture_info* p_capture_info,
            r_obj* node);
