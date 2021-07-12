# inputs must be valid

    Code
      (expect_error(set_names(environment())))
    Output
      <error/rlang_error>
      `x` must be a vector
      Context: `rlang::abort()`
    Code
      (expect_error(set_names(1:10, letters[1:4])))
    Output
      <error/rlang_error>
      The size of `nm` (4) must be compatible with the size of `x` (10).
      Context: `rlang::abort()`

