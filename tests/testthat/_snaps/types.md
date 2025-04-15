# is_string2() matches on `empty`

    Code
      is_string2("foo", empty = 1)
    Condition <rlang_error>
      Error in `is_string2()`:
      ! `empty` must be `NULL` or a logical value.
    Code
      is_string2("foo", empty = NA)
    Condition <rlang_error>
      Error in `is_string2()`:
      ! `empty` must be `NULL` or a logical value.
    Code
      is_string2("foo", "foo", empty = TRUE)
    Condition <rlang_error>
      Error in `is_string2()`:
      ! Exactly one of `string` and `empty` must be supplied.

