# fake pronoun fails informatively

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

# `.data` pronoun fails informatively

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error: Can't subset `.data` outside of a data mask context.
    Code
      (expect_error(f(mtcars)))
    Output
      <error/rlang_error_data_pronoun_not_found>
      Error: Column `foo` not found in `.data`
    Code
      g <- (function(data) h(.data[[2]], data))
      (expect_error(f(mtcars)))
    Output
      <error/rlang_error>
      Error in `.data[[2]]`: Must subset the data pronoun with a string, not a double vector.

