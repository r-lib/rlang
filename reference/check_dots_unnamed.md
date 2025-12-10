# Check that all dots are unnamed

In functions like [`paste()`](https://rdrr.io/r/base/paste.html), named
arguments in `...` are often a sign of misspelled argument names. Call
`check_dots_unnamed()` to fail with an error when named arguments are
detected.

## Usage

``` r
check_dots_unnamed(
  env = caller_env(),
  error = NULL,
  call = caller_env(),
  action = abort
)
```

## Arguments

- env:

  Environment in which to look for `...`.

- error:

  An optional error handler passed to
  [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md). Use
  this e.g. to demote an error into a warning.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) for more
  information.

- action:

  **\[deprecated\]**

## See also

Other dots checking functions:
[`check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.md),
[`check_dots_used()`](https://rlang.r-lib.org/reference/check_dots_used.md)

## Examples

``` r
f <- function(..., foofy = 8) {
  check_dots_unnamed()
  c(...)
}

f(1, 2, 3, foofy = 4)
#> [1] 1 2 3

try(f(1, 2, 3, foof = 4))
#> Error in f(1, 2, 3, foof = 4) : 
#>   Arguments in `...` must be passed by position, not name.
#> ✖ Problematic argument:
#> • foof = 4
```
