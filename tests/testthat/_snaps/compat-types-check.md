# `check_bool()` checks

    Code
      err(checker(NA, check_bool))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be `TRUE` or `FALSE`, not `NA`.
    Code
      err(checker(NULL, check_bool))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be `TRUE` or `FALSE`, not NULL.
    Code
      err(checker(lgl(), check_bool, allow_na = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be `TRUE`, `FALSE`, or `NA`, not an empty logical vector.
    Code
      err(checker(c(TRUE, FALSE), check_bool, allow_na = TRUE, allow_null = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be `TRUE`, `FALSE`, `NA`, or `NULL`, not a logical vector.
    Code
      err(checker(1, check_bool))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be `TRUE` or `FALSE`, not a number.

# `check_string()` checks

    Code
      err(checker("", check_string, allow_empty = FALSE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a single string, not `""`.
    Code
      err(checker(NA, check_string))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a single string, not `NA`.
    Code
      err(checker(NULL, check_string))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a single string, not NULL.
    Code
      err(checker(chr(), check_string, allow_na = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a single string or `NA`, not an empty character vector.
    Code
      err(checker(na_chr, check_string))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a single string, not a character `NA`.
    Code
      err(checker(c("", ""), check_string, allow_na = TRUE, allow_null = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a single string, `NA`, or `NULL`, not a character vector.
    Code
      err(checker(1, check_string))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a single string, not a number.

# `check_number()` checks

    Code
      err(checker(NA, check_number))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a round number, not `NA`.
    Code
      err(checker(NULL, check_number))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a round number, not NULL.
    Code
      err(checker(int(), check_number, allow_na = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a round number or `NA`, not an empty integer vector.
    Code
      err(checker(na_dbl, check_number))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a round number, not a numeric `NA`.
    Code
      err(checker(na_int, check_number))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a round number, not an integer `NA`.
    Code
      err(checker(10:11, check_number, allow_na = TRUE, allow_null = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a round number, `NA`, or `NULL`, not an integer vector.
    Code
      err(checker(10.5, check_number))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a round number, not a number.

# `check_symbol()` checks

    Code
      err(checker(NULL, check_symbol))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a symbol, not NULL.
    Code
      err(checker(TRUE, check_symbol, allow_na = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a symbol or `NA`, not `TRUE`.
    Code
      err(checker(alist(foo, bar), check_symbol, allow_na = TRUE, allow_null = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a symbol, `NA`, or `NULL`, not a list.
    Code
      err(checker("foo", check_symbol))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a symbol, not a string.
    Code
      err(checker(quote(foo()), check_symbol))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a symbol, not a call.

# `check_call()` checks

    Code
      err(checker(NULL, check_call))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a defused call, not NULL.
    Code
      err(checker(TRUE, check_call, allow_na = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a defused call or `NA`, not `TRUE`.
    Code
      err(checker(alist(foo(), bar()), check_call, allow_na = TRUE, allow_null = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a defused call, `NA`, or `NULL`, not a list.
    Code
      err(checker(quote(foo), check_call))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a defused call, not a symbol.

# `check_environment()` checks

    Code
      err(checker(NULL, check_environment))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be an environment, not NULL.
    Code
      err(checker(FALSE, check_environment, allow_na = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be an environment or `NA`, not `FALSE`.
    Code
      err(checker(list(env(), env()), check_environment, allow_na = TRUE, allow_null = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be an environment, `NA`, or `NULL`, not a list.

# `check_character()` checks

    Code
      err(checker(NULL, check_character))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a character vector, not NULL.
    Code
      err(checker(NA, check_character))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a character vector, not `NA`.
    Code
      err(checker(1, check_character, allow_na = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a character vector or `NA`, not a number.
    Code
      err(checker(list("foo", "bar"), check_character, allow_na = TRUE, allow_null = TRUE))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `foo` must be a character vector, `NA`, or `NULL`, not a list.

