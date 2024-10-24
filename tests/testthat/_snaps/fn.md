# as_function() has nice errors

    Code
      as_function(1)
    Condition <rlang_error>
      Error:
      ! Can't convert `1`, a double vector, to a function.
    Code
      as_function(1, arg = "foo")
    Condition <rlang_error>
      Error:
      ! Can't convert `foo`, a double vector, to a function.
    Code
      my_function(1 + 2)
    Condition <rlang_error>
      Error in `my_function()`:
      ! Can't convert `my_arg`, a double vector, to a function.
    Code
      my_function(1)
    Condition <rlang_error>
      Error in `my_function()`:
      ! Can't convert `my_arg`, a double vector, to a function.
    Code
      my_function(a ~ b)
    Condition <rlang_error>
      Error in `my_function()`:
      ! Can't convert `my_arg`, a two-sided formula, to a function.

# check inputs in function accessors

    Code
      fn_fmls(1)
    Condition <rlang_error>
      Error in `fn_fmls()`:
      ! `fn` must be an R function, not the number 1.
    Code
      fn_body(1)
    Condition <rlang_error>
      Error in `fn_body()`:
      ! `fn` must be an R function, not the number 1.
    Code
      fn_env(1)
    Condition <rlang_error>
      Error in `fn_env()`:
      ! `fn` must be a function, not the number 1.

