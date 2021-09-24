# call_name() and call_ns() detect `::` calls (#670)

    Code
      (expect_error(call_name(quote(foo::bar))))
    Output
      <error/rlang_error>
      Error in `call_name()`: `call` must be a quoted call.
    Code
      (expect_error(call_name(quote(foo:::bar))))
    Output
      <error/rlang_error>
      Error in `call_name()`: `call` must be a quoted call.
    Code
      (expect_error(call_ns(quote(foo::bar))))
    Output
      <error/rlang_error>
      Error in `call_ns()`: `call` must be a quoted call.
    Code
      (expect_error(call_ns(quote(foo:::bar))))
    Output
      <error/rlang_error>
      Error in `call_ns()`: `call` must be a quoted call.

