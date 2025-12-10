# How many arguments are currently forwarded in dots?

This returns the number of arguments currently forwarded in `...` as an
integer.

## Usage

``` r
dots_n(...)
```

## Arguments

- ...:

  Forwarded arguments.

## Examples

``` r
fn <- function(...) dots_n(..., baz)
fn(foo, bar)
#> [1] 3
```
