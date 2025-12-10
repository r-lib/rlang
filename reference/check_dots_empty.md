# Check that dots are empty

`...` can be inserted in a function signature to force users to fully
name the details arguments. In this case, supplying data in `...` is
almost always a programming error. This function checks that `...` is
empty and fails otherwise.

## Usage

``` r
check_dots_empty(
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

## Details

In packages, document `...` with this standard tag:

     @inheritParams rlang::args_dots_empty

## See also

Other dots checking functions:
[`check_dots_unnamed()`](https://rlang.r-lib.org/reference/check_dots_unnamed.md),
[`check_dots_used()`](https://rlang.r-lib.org/reference/check_dots_used.md)

## Examples

``` r
f <- function(x, ..., foofy = 8) {
  check_dots_empty()
  x + foofy
}

# This fails because `foofy` can't be matched positionally
try(f(1, 4))
#> Error in f(1, 4) : `...` must be empty.
#> ✖ Problematic argument:
#> • ..1 = 4
#> ℹ Did you forget to name an argument?

# This fails because `foofy` can't be matched partially by name
try(f(1, foof = 4))
#> Error in f(1, foof = 4) : `...` must be empty.
#> ✖ Problematic argument:
#> • foof = 4

# Thanks to `...`, it must be matched exactly
f(1, foofy = 4)
#> [1] 5
```
