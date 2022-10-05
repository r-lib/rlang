# as_function() has nice errors

    Code
      (expect_error(as_function(1)))
    Output
      <error/rlang_error>
      Error:
      ! Can't convert `1`, a double vector, to a function.
    Code
      (expect_error(as_function(1, arg = "foo")))
    Output
      <error/rlang_error>
      Error:
      ! Can't convert `foo`, a double vector, to a function.
    Code
      (expect_error(my_function(1 + 2)))
    Output
      <error/rlang_error>
      Error in `my_function()`:
      ! Can't convert `my_arg`, a double vector, to a function.
    Code
      (expect_error(my_function(1)))
    Output
      <error/rlang_error>
      Error in `my_function()`:
      ! Can't convert `my_arg`, a double vector, to a function.
    Code
      (expect_error(my_function(a ~ b)))
    Output
      <error/rlang_error>
      Error in `my_function()`:
      ! Can't convert `my_arg`, a two-sided formula, to a function.

# check inputs in function accessors

    Code
      (expect_error(fn_fmls(1)))
    Output
      <error/rlang_error>
      Error in `fn_fmls()`:
      ! `fn` must be an R function, not the number 1.
    Code
      (expect_error(fn_body(1)))
    Output
      <error/rlang_error>
      Error in `fn_body()`:
      ! `fn` must be an R function, not the number 1.
    Code
      (expect_error(fn_env(1)))
    Output
      <error/rlang_error>
      Error in `fn_env()`:
      ! `fn` must be a function, not the number 1.

