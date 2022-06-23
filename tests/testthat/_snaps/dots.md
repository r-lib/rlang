# `.homonyms` = 'error' fails with homonyms

    Code
      (expect_error(list_error(1, a = 2, a = 3)))
    Output
      <error/rlang_error>
      Error in `list_error()`:
      ! Arguments in `...` must have unique names.
      x Multiple arguments named `a` at positions 2 and 3.
    Code
      (expect_error(list_error(1, a = 2, b = 3, 4, b = 5, b = 6, 7, a = 8)))
    Output
      <error/rlang_error>
      Error in `list_error()`:
      ! Arguments in `...` must have unique names.
      x Multiple arguments named `a` at positions 2 and 8.
      x Multiple arguments named `b` at positions 3, 5, and 6.
    Code
      (expect_error(list_error(1, a = 2, b = 3, 4, b = 5, b = 6, 7, a = 8)))
    Output
      <error/rlang_error>
      Error in `list_error()`:
      ! Arguments in `...` must have unique names.
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

# `.homonyms` error is thrown

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! Arguments in `...` must have unique names.
      x Multiple arguments named `a` at positions 1 and 2.

# `list2(!!!x)` returns `x` without duplication

    Code
      x <- as.list(1:100)
      with_memory_prof(out <- list2(!!!x))
    Output
      [1] 0 B
    Code
      expect_equal(out, as.list(x))
      x <- 1:100 + 0L
      with_memory_prof(out <- list2(!!!x))
    Output
      [1] 848 B
    Code
      expect_equal(out, as.list(x))

