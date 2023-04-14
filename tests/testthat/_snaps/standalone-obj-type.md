# stop_input_type() handles I() in `arg` (#1607)

    Code
      err(checker(1, stop_input_type, what = "a logical", arg = I("Element 1 of `x`")))
    Output
      <error/rlang_error>
      Error in `checker()`:
      ! Element 1 of `x` must be a logical, not the number 1.

