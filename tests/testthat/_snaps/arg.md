# `arg_match()` has informative error messages

    Code
      cnd_cat(expect_error(arg_match0("continuuos", c("discrete", "continuous"))))
    Output
      `"continuuos"` must be one of "discrete" or "continuous".
      Did you mean "continuous"?
    Code
      cnd_cat(expect_error(arg_match0("fou", c("bar", "foo"))))
    Output
      `"fou"` must be one of "bar" or "foo".
      Did you mean "foo"?
    Code
      cnd_cat(expect_error(arg_match0("fu", c("ba", "fo"))))
    Output
      `"fu"` must be one of "ba" or "fo".
      Did you mean "fo"?
    Code
      cnd_cat(expect_error(arg_match0("baq", c("foo", "baz", "bas"), arg_nm = "arg")))
    Output
      `arg` must be one of "foo", "baz", or "bas".
      Did you mean "baz"?
    Code
      cnd_cat(expect_error(arg_match0("", character())))
    Output
      `values` must have at least one element.

# `arg_match()` provides no suggestion when the edit distance is too large

    Code
      cnd_cat(expect_error(arg_match0("foobaz", c("fooquxs", "discrete"))))
    Output
      `"foobaz"` must be one of "fooquxs" or "discrete".
    Code
      cnd_cat(expect_error(arg_match0("a", c("b", "c"))))
    Output
      `"a"` must be one of "b" or "c".

