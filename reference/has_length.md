# How long is an object?

This is a function for the common task of testing the length of an
object. It checks the length of an object in a non-generic way:
[`base::length()`](https://rdrr.io/r/base/length.html) methods are
ignored.

## Usage

``` r
has_length(x, n = NULL)
```

## Arguments

- x:

  A R object.

- n:

  A specific length to test `x` with. If `NULL`, `has_length()` returns
  `TRUE` if `x` has length greater than zero, and `FALSE` otherwise.

## Examples

``` r
has_length(list())
#> [1] FALSE
has_length(list(), 0)
#> [1] TRUE

has_length(letters)
#> [1] TRUE
has_length(letters, 20)
#> [1] FALSE
has_length(letters, 26)
#> [1] TRUE
```
