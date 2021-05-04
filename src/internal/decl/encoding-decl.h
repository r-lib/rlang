static
r_obj* chr_normalise_encoding(r_obj* x);

static inline
r_ssize chr_find_normalise_start(r_obj* x, r_ssize size);

static
r_obj* list_normalise_encoding(r_obj* x);

static
r_obj* r_attrib_normalise_encoding(r_obj* x, r_obj* attrib);

static
r_obj* attrib_normalise_encoding(r_obj* x);

static inline
r_obj* str_normalise(r_obj* x);

static inline
bool str_is_normalised(r_obj* x);

static inline
bool str_is_ascii_or_utf8(r_obj* x);
