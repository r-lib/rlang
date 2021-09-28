# call_name() and call_ns() detect `::` calls (#670)

    Code
      (expect_error(call_name(quote(foo::bar))))
    Output
      <error/rlang_error>
      Error in `call_name()`: `call` must be a simple call.
      i Calls to `::` or `:::` are not simple calls.
      i See `?is_call_simple`.
    Code
      (expect_error(call_name(quote(foo:::bar))))
    Output
      <error/rlang_error>
      Error in `call_name()`: `call` must be a simple call.
      i Calls to `::` or `:::` are not simple calls.
      i See `?is_call_simple`.
    Code
      (expect_error(call_ns(quote(foo::bar))))
    Output
      <error/rlang_error>
      Error in `call_ns()`: `call` must be a simple call.
      i Calls to `::` or `:::` are not simple calls.
      i See `?is_call_simple`.
    Code
      (expect_error(call_ns(quote(foo:::bar))))
    Output
      <error/rlang_error>
      Error in `call_ns()`: `call` must be a simple call.
      i Calls to `::` or `:::` are not simple calls.
      i See `?is_call_simple`.

