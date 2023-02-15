static
enum is_number int_standalone_check_number(r_obj* x,
                                           r_obj* ffi_min,
                                           r_obj* ffi_max,
                                           r_obj* allow_na,
                                           r_obj* allow_null);

static
enum is_number dbl_standalone_check_number(r_obj* x,
                                           r_obj* allow_decimal,
                                           r_obj* ffi_min,
                                           r_obj* ffi_max,
                                           r_obj* allow_infinite,
                                           r_obj* allow_na,
                                           r_obj* allow_null);
