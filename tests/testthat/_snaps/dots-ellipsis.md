# error if dots not used by another function

    Code
      expect_error(f(x = 10, c = 3), class = "rlib_error_dots_unused")

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

