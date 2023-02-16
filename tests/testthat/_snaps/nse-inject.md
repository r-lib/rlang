# englue() has good error messages (#1531)

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

# englue() works

    Code
      err(englue("{'foo'}"), "Must use")
    Output
      <error/rlang_error>
      Error in `englue()`:
      ! Must use `{{`.
      i Use `glue::glue()` for interpolation with `{`.

