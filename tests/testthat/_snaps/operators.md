# %|% fails with wrong types

    Code
      (expect_error(c(1L, NA) %|% 2))
    Output
      <error/rlang_error>
      Replacement values must have type integer, not type double
    Code
      (expect_error(c(1, NA) %|% ""))
    Output
      <error/rlang_error>
      Replacement values must have type double, not type character
    Code
      (expect_error(c(1, NA) %|% call("fn")))
    Output
      <error/rlang_error>
      Replacement values must have type double, not type language
    Code
      (expect_error(call("fn") %|% 1))
    Output
      <error/rlang_error>
      Cannot replace missing values in an object of type language

# %|% fails with wrong length

    Code
      (expect_error(c(1L, NA) %|% 1:3))
    Output
      <error/rlang_error>
      The replacement values must have size 1 or 2, not 3
    Code
      (expect_error(1:10 %|% 1:4))
    Output
      <error/rlang_error>
      The replacement values must have size 1 or 10, not 4
    Code
      (expect_error(1L %|% 1:4))
    Output
      <error/rlang_error>
      The replacement values must have size 1, not 4

