# pronouns fail informatively

    Code
      # Fake pronouns
      f <- (function() .data$foo)
      (expect_error(f(), "subset"))
    Output
      <error/rlang_error>
      Error in `f()`: Can't subset `.data` outside of a data mask context.
    Code
      f <- (function() .data[["foo"]])
      (expect_error(f(), "subset"))
    Output
      <error/rlang_error>
      Error in `f()`: Can't subset `.data` outside of a data mask context.

