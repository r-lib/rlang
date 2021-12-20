# parse_expr() throws meaningful error messages

    Code
      err(parse_expr(""))
    Output
      <error/rlang_error>
      Error in `parse_expr()`:
      ! `x` must contain exactly 1 expression, not 0.
    Code
      err(parse_expr("foo; bar"))
    Output
      <error/rlang_error>
      Error in `parse_expr()`:
      ! `x` must contain exactly 1 expression, not 2.

