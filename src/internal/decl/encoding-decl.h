static
r_obj* chr_encode_utf8(r_obj* x);

static inline
r_ssize chr_find_encoding_start(r_obj* x, r_ssize size);

static
r_obj* list_encode_utf8(r_obj* x);

static
r_obj* obj_attrib_encode_utf8(r_obj* x, r_obj* attrib);

static
r_obj* attrib_encode_utf8(r_obj* x);

static inline
bool str_is_ascii_or_utf8(r_obj* x);

static inline
r_obj* str_as_utf8(r_obj* x);
