# error if dots not used by another function

    Code
      expect_error(f(x = 10, c = 3), class = "rlib_error_dots_unused")

# error if dots named

    Code
      (expect_error(f(1, 2, 3, xy = 4, x = 5), class = "rlib_error_dots_named"))
    Output
      <error/rlib_error_dots_named>
      2 arguments in `...` had unexpected names.
      x We detected these problematic arguments:
      * `xy`
      * `x`
      i Did you misspecify an argument?

# error if if dots not empty

    Code
      (expect_error(f(xy = 4), class = "rlib_error_dots_nonempty"))
    Output
      <error/rlib_error_dots_nonempty>
      `...` is not empty.
      i These dots only exist to allow future extensions and should be empty.
      x We detected these problematic arguments:
      * `xy`
      i Did you misspecify an argument?
      Context: `action_dots()`
    Code
      (expect_error(f0(xy = 4), class = "rlib_error_dots_nonempty"))
    Output
      <error/rlib_error_dots_nonempty>
      `...` is not empty.
      i These dots only exist to allow future extensions and should be empty.
      x We detected these problematic arguments:
      * `xy`
      i Did you misspecify an argument?
      Context: `action_dots()`

