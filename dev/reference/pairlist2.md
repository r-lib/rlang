# Collect dynamic dots in a pairlist

This pairlist constructor uses [dynamic
dots](https://rlang.r-lib.org/dev/reference/dyn-dots.md). Use it to
manually create argument lists for calls or parameter lists for
functions.

## Usage

``` r
pairlist2(...)
```

## Arguments

- ...:

  \<[dynamic](https://rlang.r-lib.org/dev/reference/dyn-dots.md)\>
  Arguments stored in the pairlist. Empty arguments are preserved.

## Examples

``` r
# Unlike `exprs()`, `pairlist2()` evaluates its arguments.
new_function(pairlist2(x = 1, y = 3 * 6), quote(x * y))
#> function (x = 1, y = 18) 
#> x * y
#> <environment: 0x55eb8a04e4a0>
new_function(exprs(x = 1, y = 3 * 6), quote(x * y))
#> function (x = 1, y = 3 * 6) 
#> x * y
#> <environment: 0x55eb8a04e4a0>

# It preserves missing arguments, which is useful for creating
# parameters without defaults:
new_function(pairlist2(x = , y = 3 * 6), quote(x * y))
#> function (x, y = 18) 
#> x * y
#> <environment: 0x55eb8a04e4a0>
```
