# is_string() matches on `empty`

    Code
      (expect_error(is_string("foo", empty = 1)))
    Output
      <error/rlang_error>
      Error in `is_string()`: `empty` must be `NULL` or a logical value.
    Code
      (expect_error(is_string("foo", empty = NA)))
    Output
      <error/rlang_error>
      Error in `is_string()`: `empty` must be `NULL` or a logical value.
    Code
      (expect_error(is_string("foo", "foo", empty = TRUE)))
    Output
      <error/rlang_error>
      Error in `is_string()`: Exactly one of `string` and `empty` must be supplied.

# is_character() matches empty and missing values

    Code
      (expect_error(is_character("", empty = TRUE, missing = TRUE)))
    Output
      <error/rlang_error>
      Error in `is_character()`: Exactly one of `missing` and `empty` must be supplied.

