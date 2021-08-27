# as_function() has nice errors

    Code
      (expect_error(as_function(1)))
    Output
      <error/rlang_error>
      Can't convert `1`, a double vector, to a function.
    Code
      (expect_error(as_function(1, arg = "foo")))
    Output
      <error/rlang_error>
      Can't convert `foo`, a double vector, to a function.
    Code
      (expect_error(my_function(1 + 2)))
    Output
      <error/rlang_error>
      Can't convert `my_arg`, a double vector, to a function.
      Call: `my_function()`
    Code
      (expect_error(my_function(1)))
    Output
      <error/rlang_error>
      Can't convert `my_arg`, a double vector, to a function.
      Call: `my_function()`
    Code
      (expect_error(my_function(a ~ b)))
    Output
      <error/rlang_error>
      Can't convert `my_arg`, a two-sided formula, to a function.
      Call: `my_function()`

