# `.homonyms` = 'error' fails with homonyms

    Code
      (expect_error(list_error(1, a = 2, a = 3)))
    Output
      <error/rlang_error>
      Error in `list_error()`:
      ! Arguments can't have the same name.
      x Multiple arguments named `a` at positions 2 and 3.
    Code
      (expect_error(list_error(1, a = 2, b = 3, 4, b = 5, b = 6, 7, a = 8)))
    Output
      <error/rlang_error>
      Error in `list_error()`:
      ! Arguments can't have the same name.
      x Multiple arguments named `a` at positions 2 and 8.
      x Multiple arguments named `b` at positions 3, 5, and 6.
    Code
      (expect_error(list_error(1, a = 2, b = 3, 4, b = 5, b = 6, 7, a = 8)))
    Output
      <error/rlang_error>
      Error in `list_error()`:
      ! Arguments can't have the same name.
      x Multiple arguments named `a` at positions 2 and 8.
      x Multiple arguments named `b` at positions 3, 5, and 6.

# `.ignore_empty` is matched

    Code
      (expect_error(dots_list(.ignore_empty = "t")))
    Output
      <error/rlang_error>
      Error in `dots_list()`:
      ! `.ignore_empty` must be one of "trailing", "none", or "all", not "t".
      i Did you mean "trailing"?
    Code
      foo <- (function() dots_list(.ignore_empty = "t"))
      (expect_error(foo()))
    Output
      <error/rlang_error>
      Error in `dots_list()`:
      ! `.ignore_empty` must be one of "trailing", "none", or "all", not "t".
      i Did you mean "trailing"?

