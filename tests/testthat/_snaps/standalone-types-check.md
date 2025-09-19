# `check_logical()` checks

    Code
      err(checker(, check_logical))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a logical vector, not absent.
    Code
      err(checker(NULL, check_logical))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a logical vector, not `NULL`.
    Code
      err(checker(NA_integer_, check_logical))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a logical vector, not an integer `NA`.
    Code
      err(checker(1, check_logical))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a logical vector, not the number 1.
    Code
      err(checker(list("foo", "bar"), check_logical, allow_null = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a logical vector or `NULL`, not a list.
    Code
      err(checker(NA, check_logical, allow_na = FALSE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` can't contain NA values.

