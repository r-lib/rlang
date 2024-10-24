# error if dots not used by another function

    Code
      f(x = 10, c = 3)
    Condition <rlib_error_dots_unused>
      Error in `f()`:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * c = 3
      i Did you misspell an argument name?

# error if dots named

    Code
      f(1, 2, 3, xy = 4, x = 5)
    Condition <rlib_error_dots_named>
      Error in `f()`:
      ! Arguments in `...` must be passed by position, not name.
      x Problematic arguments:
      * xy = 4
      * x = 5

# error if if dots not empty

    Code
      f(xy = 4)
    Condition <rlib_error_dots_nonempty>
      Error in `f()`:
      ! `...` must be empty.
      x Problematic argument:
      * xy = 4
    Code
      f0(xy = 4)
    Condition <rlib_error_dots_nonempty>
      Error in `f0()`:
      ! `...` must be empty.
      x Problematic argument:
      * xy = 4

# expression contents are mentioned

    Code
      f("foo")
    Condition
      Error in `f()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = "foo"
      i Did you forget to name an argument?
    Code
      f(foo)
    Condition
      Error in `f()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = foo
      i Did you forget to name an argument?
    Code
      inject(f(!!letters))
    Condition
      Error in `f()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = <chr>
      i Did you forget to name an argument?
    Code
      f(a = {
        1
        2
      })
    Condition
      Error in `f()`:
      ! `...` must be empty.
      x Problematic argument:
      * a = { ... }
    Code
      f(a = toupper(letters))
    Condition
      Error in `f()`:
      ! `...` must be empty.
      x Problematic argument:
      * a = toupper(letters)

# empty dots error mentions info bullets if any unnamed element

    Code
      f(1)
    Condition
      Error in `f()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?
    Code
      f(a = 1)
    Condition
      Error in `f()`:
      ! `...` must be empty.
      x Problematic argument:
      * a = 1
    Code
      f(a = 1, 2)
    Condition
      Error in `f()`:
      ! `...` must be empty.
      x Problematic arguments:
      * a = 1
      * ..2 = 2
      i Did you forget to name an argument?

# check_dots_empty() allows trailing missing arg (#1390)

    Code
      fn(a = 1, b = )
    Condition <rlib_error_dots_nonempty>
      Error in `fn()`:
      ! `...` must be empty.
      x Problematic argument:
      * b = <empty>

