# error if dots not used by another function

    Code
      expect_error(f(x = 10, c = 3), class = "rlib_error_dots_unused")

# error if dots named

    Code
      (expect_error(f(1, 2, 3, xy = 4, x = 5), class = "rlib_error_dots_named"))
    Output
      <error/rlib_error_dots_named>
      Error in `f()`:
      Arguments in `...` can't be named.
      x Problematic arguments:
      * `xy`
      * `x`

# error if if dots not empty

    Code
      (expect_error(f(xy = 4), class = "rlib_error_dots_nonempty"))
    Output
      <error/rlib_error_dots_nonempty>
      Error in `f()`:
      `...` must be empty.
      x Problematic arguments:
      * `xy`
    Code
      (expect_error(f0(xy = 4), class = "rlib_error_dots_nonempty"))
    Output
      <error/rlib_error_dots_nonempty>
      Error in `f0()`:
      `...` must be empty.
      x Problematic arguments:
      * `xy`

