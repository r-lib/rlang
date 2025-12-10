# Is object identical to TRUE or FALSE?

These functions bypass R's automatic conversion rules and check that `x`
is literally `TRUE` or `FALSE`.

## Usage

``` r
is_true(x)

is_false(x)
```

## Arguments

- x:

  object to test

## Examples

``` r
is_true(TRUE)
#> [1] TRUE
is_true(1)
#> [1] FALSE

is_false(FALSE)
#> [1] TRUE
is_false(0)
#> [1] FALSE
```
