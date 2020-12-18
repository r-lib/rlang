# `arg_match()` has informative error messages

    Code
      (expect_error(arg_match0("continuuos", c("discrete", "continuous"))))
    Output
      <error/rlang_error>
      `"continuuos"` must be one of "discrete" or "continuous".
      Did you mean "continuous"?
    Code
      (expect_error(arg_match0("fou", c("bar", "foo"))))
    Output
      <error/rlang_error>
      `"fou"` must be one of "bar" or "foo".
      Did you mean "foo"?
    Code
      (expect_error(arg_match0("fu", c("ba", "fo"))))
    Output
      <error/rlang_error>
      `"fu"` must be one of "ba" or "fo".
      Did you mean "fo"?
    Code
      (expect_error(arg_match0("baq", c("foo", "baz", "bas"), arg_nm = "arg")))
    Output
      <error/rlang_error>
      `arg` must be one of "foo", "baz", or "bas".
      Did you mean "baz"?
    Code
      (expect_error(arg_match0("", character())))
    Output
      <error/rlang_error>
      `values` must have at least one element.

# `arg_match()` provides no suggestion when the edit distance is too large

    Code
      (expect_error(arg_match0("foobaz", c("fooquxs", "discrete"))))
    Output
      <error/rlang_error>
      `"foobaz"` must be one of "fooquxs" or "discrete".
    Code
      (expect_error(arg_match0("a", c("b", "c"))))
    Output
      <error/rlang_error>
      `"a"` must be one of "b" or "c".

