# `arg_match()` has informative error messages

    Code
      (expect_error(arg_match0("continuuos", c("discrete", "continuous"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "discrete" or "continuous", not "continuuos".
      i Did you mean "continuous"?
      Context: `stop_arg_match()`
    Code
      (expect_error(arg_match0("fou", c("bar", "foo"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "bar" or "foo", not "fou".
      i Did you mean "foo"?
      Context: `stop_arg_match()`
    Code
      (expect_error(arg_match0("fu", c("ba", "fo"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "ba" or "fo", not "fu".
      i Did you mean "fo"?
      Context: `stop_arg_match()`
    Code
      (expect_error(arg_match0("baq", c("foo", "baz", "bas"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "foo", "baz", or "bas", not "baq".
      i Did you mean "baz"?
      Context: `stop_arg_match()`
    Code
      (expect_error(arg_match0("", character(), "my_arg")))
    Output
      <error/rlang_error>
      `values` must have at least one element.
      Context: `rlang::abort()`
    Code
      (expect_error(arg_match0("fo", "foo", quote(f()))))
    Output
      <error/rlang_error>
      `arg_nm` must be a string or symbol.
      Context: `rlang::abort()`

# `arg_match()` provides no suggestion when the edit distance is too large

    Code
      (expect_error(arg_match0("foobaz", c("fooquxs", "discrete"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "fooquxs" or "discrete", not "foobaz".
      Context: `stop_arg_match()`
    Code
      (expect_error(arg_match0("a", c("b", "c"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "b" or "c", not "a".
      Context: `stop_arg_match()`

# `arg_match()` makes case-insensitive match

    Code
      (expect_error(arg_match0("a", c("A", "B"), "my_arg"), "Did you mean \"A\"?"))
    Output
      <error/rlang_error>
      `my_arg` must be one of "A" or "B", not "a".
      i Did you mean "A"?
      Context: `stop_arg_match()`
    Code
      (expect_error(arg_match0("aa", c("AA", "aA"), "my_arg"), "Did you mean \"aA\"?")
      )
    Output
      <error/rlang_error>
      `my_arg` must be one of "AA" or "aA", not "aa".
      i Did you mean "aA"?
      Context: `stop_arg_match()`

# arg_require() checks argument is supplied (#1118)

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      `f()` requires the argument `x` to be supplied.
      Context: `arg_require()`
    Code
      (expect_error(g()))
    Output
      <error/rlang_error>
      `f()` requires the argument `x` to be supplied.
      Context: `arg_require()`

# arg_match() supports symbols and scalar strings

    Code
      (expect_error(arg_match0(chr_get("fo", 0L), c("bar", "foo"), "my_arg")))
    Output
      <error/rlang_error>
      `my_arg` must be one of "bar" or "foo", not "fo".
      i Did you mean "foo"?
      Context: `stop_arg_match()`

