# englue() has good error messages (#1531)

    Code
      fn <- (function(x) englue(c("a", "b")))
      fn()
    Condition <rlang_error>
      Error in `englue()`:
      ! `x` must be a single string, not a character vector.
    Code
      fn <- (function(x) englue(env()))
      fn()
    Condition <rlang_error>
      Error in `englue()`:
      ! `x` must be a single string, not an environment.
    Code
      fn <- (function(x) glue_embrace("{{ x }}_foo"))
      fn()
    Condition <rlang_error>
      Error in `fn()`:
      ! `x` is absent but must be supplied.
    Code
      fn <- (function(x) englue("{{ x }}_foo"))
      fn()
    Condition <rlang_error>
      Error in `fn()`:
      ! `x` is absent but must be supplied.
    Code
      fn <- (function(x) list2("{{ x }}_foo" := NULL))
      fn()
    Condition <rlang_error>
      Error in `fn()`:
      ! `x` is absent but must be supplied.

# can wrap englue() (#1565)

    Code
      my_englue(c("a", "b"))
    Condition <rlang_error>
      Error in `my_englue()`:
      ! `text` must be a single string, not a character vector.
    Code
      my_englue(env())
    Condition <rlang_error>
      Error in `my_englue()`:
      ! `text` must be a single string, not an environment.
    Code
      fn()
    Condition <rlang_error>
      Error in `fn()`:
      ! `x` is absent but must be supplied.

# englue() checks for the size of its result (#1492)

    Code
      fn <- (function(x) englue("{{ x }} {NULL}"))
      fn(foo)
    Condition <rlang_error>
      Error in `englue()`:
      ! The glue string must be size 1, not 0.
    Code
      fn <- (function(x) list2("{{ x }} {NULL}" := NULL))
      fn(foo)
    Condition <rlang_error>
      Error in `englue()`:
      ! The glue string must be size 1, not 0.

