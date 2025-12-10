# Convert to function

`as_function()` transforms a one-sided formula into a function. This
powers the lambda syntax in packages like purrr.

## Usage

``` r
as_function(
  x,
  env = global_env(),
  ...,
  arg = caller_arg(x),
  call = caller_env()
)

is_lambda(x)
```

## Arguments

- x:

  A function or formula.

  If a **function**, it is used as is.

  If a **formula**, e.g. `~ .x + 2`, it is converted to a function with
  up to two arguments: `.x` (single argument) or `.x` and `.y` (two
  arguments). The `.` placeholder can be used instead of `.x`. This
  allows you to create very compact anonymous functions (lambdas) with
  up to two inputs. Functions created from formulas have a special
  class. Use `is_lambda()` to test for it.

  If a **string**, the function is looked up in `env`. Note that this
  interface is strictly for user convenience because of the scoping
  issues involved. Package developers should avoid supplying functions
  by name and instead supply them by value.

- env:

  Environment in which to fetch the function in case `x` is a string.

- ...:

  These dots are for future extensions and must be empty.

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) for more
  information.

## Examples

``` r
f <- as_function(~ .x + 1)
f(10)
#> [1] 11

g <- as_function(~ -1 * .)
g(4)
#> [1] -4

h <- as_function(~ .x - .y)
h(6, 3)
#> [1] 3

# Functions created from a formula have a special class:
is_lambda(f)
#> [1] TRUE
is_lambda(as_function(function() "foo"))
#> [1] FALSE
```
