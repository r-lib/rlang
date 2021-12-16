# is_string2() matches on `empty`

    Code
      (expect_error(is_string2("foo", empty = 1)))
    Output
      <error/rlang_error>
      Error in `is_string2()`:
      ! `empty` must be `NULL` or a logical value.
    Code
      (expect_error(is_string2("foo", empty = NA)))
    Output
      <error/rlang_error>
      Error in `is_string2()`:
      ! `empty` must be `NULL` or a logical value.
    Code
      (expect_error(is_string2("foo", "foo", empty = TRUE)))
    Output
      <error/rlang_error>
      Error in `is_string2()`:
      ! Exactly one of `string` and `empty` must be supplied.

# is_character2() matches empty and missing values

    Code
      (expect_error(is_character2("", empty = TRUE, missing = TRUE)))
    Output
      <error/rlang_error>
      Error in `is_character2()`:
      ! Exactly one of `missing` and `empty` can be `TRUE`.

