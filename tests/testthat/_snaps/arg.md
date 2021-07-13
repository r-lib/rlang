# matches arg

    `arg` must be one of "bar" or "baz", not "foo".

# gives an error with more than one arg

    `arg` must be one of "bar" or "baz", not "bar".
    i Did you mean "bar"?

# gives error with different than rearranged arg vs value

    `myarg` must be one of "fun" or "bar", not "foo".

---

    `x` must be one of "foo" or "bar", not "foo".
    i Did you mean "foo"?

# `arg_match()` has informative error messages

    Code
      (expect_error(arg_match_wrapper("continuuos", c("discrete", "continuous"),
      "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "discrete" or "continuous", not "continuuos".
      i Did you mean "continuous"?
      Context: `arg_match0_wrapper()`
    Code
      (expect_error(arg_match_wrapper("fou", c("bar", "foo"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "bar" or "foo", not "fou".
      i Did you mean "foo"?
      Context: `arg_match0_wrapper()`
    Code
      (expect_error(arg_match_wrapper("fu", c("ba", "fo"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "ba" or "fo", not "fu".
      i Did you mean "fo"?
      Context: `arg_match0_wrapper()`
    Code
      (expect_error(arg_match_wrapper("baq", c("foo", "baz", "bas"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "foo", "baz", or "bas", not "baq".
      i Did you mean "baz"?
      Context: `arg_match0_wrapper()`
    Code
      (expect_error(arg_match_wrapper("", character(), "my_arg")))
    Output
      <error/rlang_error>
      `values` must have at least one element.
    Code
      (expect_error(arg_match_wrapper("fo", "foo", quote(f()))))
    Output
      <error/rlang_error>
      `arg_nm` must be a string or symbol.

# `arg_match()` provides no suggestion when the edit distance is too large

    Code
      (expect_error(arg_match0_wrapper("foobaz", c("fooquxs", "discrete"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "fooquxs" or "discrete", not "foobaz".
      Context: `arg_match0_wrapper()`
    Code
      (expect_error(arg_match0_wrapper("a", c("b", "c"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "b" or "c", not "a".
      Context: `arg_match0_wrapper()`

# `arg_match()` makes case-insensitive match

    Code
      (expect_error(arg_match0_wrapper("a", c("A", "B"), "my_arg"),
      "Did you mean \"A\"?"))
    Output
      <error/rlang_error>
      `my_arg` must be one of "A" or "B", not "a".
      i Did you mean "A"?
      Context: `arg_match0_wrapper()`
    Code
      (expect_error(arg_match0_wrapper("aa", c("AA", "aA"), "my_arg"),
      "Did you mean \"aA\"?"))
    Output
      <error/rlang_error>
      `my_arg` must be one of "AA" or "aA", not "aa".
      i Did you mean "aA"?
      Context: `arg_match0_wrapper()`

# arg_require() checks argument is supplied (#1118)

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      `x` must be supplied.
      Context: `f()`
    Code
      (expect_error(g()))
    Output
      <error/rlang_error>
      `x` must be supplied.
      Context: `f()`

# arg_match() supports symbols and scalar strings

    Code
      (expect_error(arg_match0_wrapper(chr_get("fo", 0L), c("bar", "foo"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "bar" or "foo", not "fo".
      i Did you mean "foo"?
      Context: `arg_match0_wrapper()`

# arg_match() requires an argument symbol

    Code
      (expect_error(wrapper()))
    Output
      <error/rlang_error>
      `arg` must be a symbol.
      ! This is an internal error, please report it to the package authors.
      Context: `wrapper()`

