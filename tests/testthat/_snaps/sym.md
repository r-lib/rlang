# ensym() fails with calls

    Code
      err(capture_sym(foo(bar)))
    Output
      <error/rlang_error>
      Error in `ensym()`:
      ! Can't convert to a symbol.

# must supply strings to sym()

    Code
      err(sym(letters))
    Output
      <error/rlang_error>
      Error in `sym()`:
      ! Can't convert a character vector to a symbol.
    Code
      err(sym(1:2))
    Output
      <error/rlang_error>
      Error in `sym()`:
      ! Can't convert an integer vector to a symbol.

