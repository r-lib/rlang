# fake pronoun fails informatively

    Code
      # Fake pronouns
      f <- (function() .data$foo)
      (expect_error(f(), "subset"))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! Can't subset `.data` outside of a data mask context.
    Code
      f <- (function() .data[["foo"]])
      (expect_error(f(), "subset"))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! Can't subset `.data` outside of a data mask context.

# `.data` pronoun fails informatively

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `g()`:
      ! Can't subset `.data` outside of a data mask context.
    Code
      (expect_error(f(mtcars)))
    Output
      <error/rlang_error_data_pronoun_not_found>
      Error in `.data$foo`:
      ! Column `foo` not found in `.data`.
    Code
      g <- (function(data) h(.data[[2]], data))
      (expect_error(f(mtcars)))
    Output
      <error/rlang_error>
      Error in `.data[[2]]`:
      ! Must subset the data pronoun with a string, not the number 2.
    Code
      g <- (function(data) h(.data["foo"], data = data))
      (expect_error(f(mtcars)))
    Output
      <error/rlang_error>
      Error in `.data["foo"]`:
      ! `[` is not supported by the `.data` pronoun, use `[[` or $ instead.
    Code
      g <- (function(data) h(.data[["foo"]] <- 1, data = data))
      (expect_error(f(mtcars)))
    Output
      <error/rlang_error>
      Error in `.data[["foo"]] <- ...`:
      ! Can't modify the data pronoun.
    Code
      g <- (function(data) h(.data$foo <- 1, data = data))
      (expect_error(f(mtcars)))
    Output
      <error/rlang_error>
      Error in `.data$"foo" <- ...`:
      ! Can't modify the data pronoun.
    Code
      g <- (function(data) h(.env["foo"], data = data))
      (expect_error(f(mtcars)))
    Output
      <error/rlang_error>
      Error in `.env["foo"]`:
      ! `[` is not supported by the `.env` pronoun, use `[[` or $ instead.
    Code
      g <- (function(data) h(.env$foo <- 1, data = data))
      (expect_error(f(mtcars)))
    Output
      <error/rlang_error>
      Error in `.env$"foo" <- ...`:
      ! Can't modify the context pronoun.
    Code
      g <- (function(data) h(.env[["foo"]] <- 1, data = data))
      (expect_error(f(mtcars)))
    Output
      <error/rlang_error>
      Error in `.env[["foo"]] <- ...`:
      ! Can't modify the context pronoun.

