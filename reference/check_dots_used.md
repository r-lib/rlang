# Check that all dots have been used

When `...` arguments are passed to a method, the method should match and
use these arguments. If this isn't the case, this often indicates a
programming error. Call `check_dots_used()` to fail with an error when
unused arguments are detected.

## Usage

``` r
check_dots_used(
  env = caller_env(),
  call = caller_env(),
  error = NULL,
  action = deprecated()
)
```

## Arguments

- env:

  Environment in which to look for `...` and to set up handler.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) for more
  information.

- error:

  An optional error handler passed to
  [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md). Use
  this e.g. to demote an error into a warning.

- action:

  **\[deprecated\]**

## Details

In packages, document `...` with this standard tag:

     @inheritParams rlang::args_dots_used

`check_dots_used()` implicitly calls
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) to check that all
elements of `...` have been used when the function exits. If you use
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) elsewhere in your
function, make sure to use `add = TRUE` so that you don't override the
handler set up by `check_dots_used()`.

## See also

Other dots checking functions:
[`check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.md),
[`check_dots_unnamed()`](https://rlang.r-lib.org/reference/check_dots_unnamed.md)

## Examples

``` r
f <- function(...) {
  check_dots_used()
  g(...)
}

g <- function(x, y, ...) {
  x + y
}
f(x = 1, y = 2)
#> [1] 3

try(f(x = 1, y = 2, z = 3))
#> Error in f(x = 1, y = 2, z = 3) : Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • z = 3
#> ℹ Did you misspell an argument name?

try(f(x = 1, y = 2, 3, 4, 5))
#> Error in f(x = 1, y = 2, 3, 4, 5) : 
#>   Arguments in `...` must be used.
#> ✖ Problematic arguments:
#> • ..1 = 3
#> • ..2 = 4
#> • ..3 = 5
#> ℹ Did you misspell an argument name?

# Use an `error` handler to handle the error differently.
# For instance to demote the error to a warning:
fn <- function(...) {
  check_dots_empty(
    error = function(cnd) {
      warning(cnd)
    }
  )
  "out"
}
fn()
#> [1] "out"
```
