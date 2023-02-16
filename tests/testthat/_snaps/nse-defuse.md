# corner cases are handled when interpolating dot names

    Code
      (expect_error(quos(!!var := NULL)))
    Output
      <error/rlang_error>
      Error in `quos()`:
      ! The LHS of `:=` must be a string, not `NULL`.
    Code
      (expect_error(list2(!!c("a", "b") := NULL)))
    Output
      <error/rlang_error>
      Error in `list2()`:
      ! The LHS of `:=` must be a string, not a character vector.

# ensyms() captures multiple symbols

    Code
      err(fn(foo()))
    Output
      <error/rlang_error>
      Error in `sym()`:
      ! Can't convert a call to a symbol.

# ensym() unwraps quosures

    Code
      err(fn(!!quo(foo())))
    Output
      <error/rlang_error>
      Error in `ensym()`:
      ! Can't convert to a symbol.

# ensyms() unwraps quosures

    Code
      err(fn(!!!quos(foo, bar())))
    Output
      <error/rlang_error>
      Error in `sym()`:
      ! Can't convert a call to a symbol.

# auto-named expressions can be unique-repaired

    Code
      expect_equal(dots_names(1, foo = 1, 1, foo = 2), c("1...1", "foo", "1...3",
        "foo"))
    Message <rlib_message_name_repair>
      New names:
      * `1` -> `1...1`
      * `1` -> `1...3`
    Code
      expect_equal(dots_names(bar, foo = 1, bar, foo = 2), c("bar...1", "foo",
        "bar...3", "foo"))
    Message <rlib_message_name_repair>
      New names:
      * `bar` -> `bar...1`
      * `bar` -> `bar...3`

