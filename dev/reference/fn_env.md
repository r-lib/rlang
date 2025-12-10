# Return the closure environment of a function

Closure environments define the scope of functions (see
[`env()`](https://rlang.r-lib.org/dev/reference/env.md)). When a
function call is evaluated, R creates an evaluation frame that inherits
from the closure environment. This makes all objects defined in the
closure environment and all its parents available to code executed
within the function.

## Usage

``` r
fn_env(fn)

fn_env(x) <- value
```

## Arguments

- fn, x:

  A function.

- value:

  A new closure environment for the function.

## Details

`fn_env()` returns the closure environment of `fn`. There is also an
assignment method to set a new closure environment.

## Examples

``` r
env <- child_env("base")
fn <- with_env(env, function() NULL)
identical(fn_env(fn), env)
#> [1] TRUE

other_env <- child_env("base")
fn_env(fn) <- other_env
identical(fn_env(fn), other_env)
#> [1] TRUE
```
