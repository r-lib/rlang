# stop_input_type() handles I() in `arg` (#1607)

    Code
      err(checker(1, stop_input_type, what = "a logical", arg = I("Element 1 of `x`")))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! Element 1 of `x` must be a logical, not the number 1.

# stop_input_type() reports 1D arrays differently from vectors

    Code
      err(checker(array(1), stop_input_type, what = "a double vector", arg = "x"))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `x` must be a double vector, not a double array.

---

    Code
      err(checker(array(list(1)), stop_input_type, what = "a list", arg = "x"))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! `x` must be a list, not a list array.

