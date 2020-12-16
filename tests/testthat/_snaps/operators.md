# %|% fails with wrong types

    Code
      cnd_cat(expect_error(c(1L, NA) %|% 2))
    Output
      Replacement values must have type integer, not type double
    Code
      cnd_cat(expect_error(c(1, NA) %|% ""))
    Output
      Replacement values must have type double, not type character
    Code
      cnd_cat(expect_error(c(1, NA) %|% call("fn")))
    Output
      Replacement values must have type double, not type language
    Code
      cnd_cat(expect_error(call("fn") %|% 1))
    Output
      Cannot replace missing values in an object of type language

# %|% fails with wrong length

    Code
      cnd_cat(expect_error(c(1L, NA) %|% 1:3))
    Output
      The replacement values must have size 1 or 2, not 3
    Code
      cnd_cat(expect_error(1:10 %|% 1:4))
    Output
      The replacement values must have size 1 or 10, not 4
    Code
      cnd_cat(expect_error(1L %|% 1:4))
    Output
      The replacement values must have size 1, not 4

