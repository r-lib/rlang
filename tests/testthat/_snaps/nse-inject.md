# englue() has good error messages (#1531)

    Code
      fn <- (function(x) englue(c("a", "b")))
      (expect_error(fn()))
    Output
      <error/rlang_error>
      Error in `englue()`:
      ! `x` must be a single string, not a character vector.
    Code
      fn <- (function(x) englue(env()))
      (expect_error(fn()))
    Output
      <error/rlang_error>
      Error in `englue()`:
      ! `x` must be a single string, not an environment.
    Code
      fn <- (function(x) glue_embrace("{{ x }}_foo"))
      (expect_error(fn()))
    Output
      <error/rlang_error>
      Error in `fn()`:
      ! `x` is absent but must be supplied.
    Code
      fn <- (function(x) englue("{{ x }}_foo"))
      (expect_error(fn()))
    Output
      <error/rlang_error>
      Error in `fn()`:
      ! `x` is absent but must be supplied.
    Code
      fn <- (function(x) list2("{{ x }}_foo" := NULL))
      (expect_error(fn()))
    Output
      <error/rlang_error>
      Error in `fn()`:
      ! `x` is absent but must be supplied.

# can wrap englue() (#1565)

    Code
      (expect_error(my_englue(c("a", "b"))))
    Output
      <error/rlang_error>
      Error in `my_englue()`:
      ! `text` must be a single string, not a character vector.
    Code
      (expect_error(my_englue(env())))
    Output
      <error/rlang_error>
      Error in `my_englue()`:
      ! `text` must be a single string, not an environment.
    Code
      (expect_error(fn()))
    Output
      <error/rlang_error>
      Error in `fn()`:
      ! `x` is absent but must be supplied.

# englue() checks for the size of its result (#1492)

    Code
      fn <- (function(x) englue("{{ x }} {NULL}"))
      (expect_error(fn(foo)))
    Output
      <error/rlang_error>
      Error in `englue()`:
      ! The glue string must be size 1, not 0.
    Code
      fn <- (function(x) list2("{{ x }} {NULL}" := NULL))
      (expect_error(fn(foo)))
    Output
      <error/rlang_error>
      Error in `englue()`:
      ! The glue string must be size 1, not 0.

