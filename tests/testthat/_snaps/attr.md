# inputs must be valid

    Code
      set_names(environment())
    Condition <rlang_error>
      Error in `set_names()`:
      ! `x` must be a vector
    Code
      set_names(1:10, letters[1:4])
    Condition <rlang_error>
      Error in `set_names()`:
      ! The size of `nm` (4) must be compatible with the size of `x` (10).

