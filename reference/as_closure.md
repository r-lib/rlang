# Transform to a closure

`as_closure()` is like
[`as_function()`](https://rlang.r-lib.org/reference/as_function.md) but
also wraps primitive functions inside closures. Some special control
flow primitives like `if`, `for`, or
[`break`](https://rdrr.io/r/base/Control.html) can't be wrapped and will
cause an error.

## Usage

``` r
as_closure(x, env = caller_env())
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
  class. Use
  [`is_lambda()`](https://rlang.r-lib.org/reference/as_function.md) to
  test for it.

  If a **string**, the function is looked up in `env`. Note that this
  interface is strictly for user convenience because of the scoping
  issues involved. Package developers should avoid supplying functions
  by name and instead supply them by value.

- env:

  Environment in which to fetch the function in case `x` is a string.

## Examples

``` r
# Primitive functions are regularised as closures
as_closure(list)
#> function (...) 
#> .Primitive("list")(...)
as_closure("list")
#> function (...) 
#> .Primitive("list")(...)

# Operators have `.x` and `.y` as arguments, just like lambda
# functions created with the formula syntax:
as_closure(`+`)
#> function (e1, e2, .x = e1, .y = e2) 
#> {
#>     if (missing(.x)) {
#>         if (missing(e1)) {
#>             abort("Must supply `e1` or `.x` to binary operator.")
#>         }
#>         .x <- e1
#>     }
#>     else if (!missing(e1)) {
#>         abort("Can't supply both `e1` and `.x` to binary operator.")
#>     }
#>     if (missing(.y) && !missing(e2)) {
#>         .y <- e2
#>     }
#>     else if (!missing(e2)) {
#>         abort("Can't supply both `e2` and `.y` to binary operator.")
#>     }
#>     if (missing(.y)) 
#>         .x
#>     else .x + .y
#> }
#> <environment: 0x560f9801bc40>
as_closure(`~`)
#> function (.x, .y) 
#> {
#>     if (is_missing(substitute(.y))) {
#>         new_formula(NULL, substitute(.x), caller_env())
#>     }
#>     else {
#>         new_formula(substitute(.x), substitute(.y), caller_env())
#>     }
#> }
#> <bytecode: 0x560f977ed830>
#> <environment: 0x560f95da3580>
```
