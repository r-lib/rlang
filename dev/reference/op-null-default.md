# Default value for `NULL`

This infix function makes it easy to replace `NULL`s with a default
value. It's inspired by the way that Ruby's or operation (`||`) works.

## Usage

``` r
x %||% y
```

## Arguments

- x, y:

  If `x` is NULL, will return `y`; otherwise returns `x`.

## Examples

``` r
1 %||% 2
#> [1] 1
NULL %||% 2
#> [1] 2
```
