static void hash_object(XXH3_state_t* p_state, r_obj* x);
static r_obj* hash_attribs_cb(r_obj* tag, r_obj* value, void* data);
static void hash_feed_vector_data(
    XXH3_state_t* p_state,
    r_obj* x,
    int type,
    r_ssize n
);
static r_obj* hash_impl(void* p_data);
static void hash_cleanup(void* p_data);
static r_obj* hash_file_impl(void* p_data);
