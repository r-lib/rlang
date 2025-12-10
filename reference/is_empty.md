# Is object an empty vector or NULL?

Is object an empty vector or NULL?

## Usage

``` r
is_empty(x)
```

## Arguments

- x:

  object to test

## Examples

``` r
is_empty(NULL)
#> [1] TRUE
is_empty(list())
#> [1] TRUE
is_empty(list(NULL))
#> [1] FALSE
```
