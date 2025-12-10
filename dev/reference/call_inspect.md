# Inspect a call

This function is a wrapper around
[`base::match.call()`](https://rdrr.io/r/base/match.call.html). It
returns its own function call.

## Usage

``` r
call_inspect(...)
```

## Arguments

- ...:

  Arguments to display in the returned call.

## Examples

``` r
# When you call it directly, it simply returns what you typed
call_inspect(foo(bar), "" %>% identity())
#> call_inspect(foo(bar), "" %>% identity())

# Pass `call_inspect` to functionals like `lapply()` or `map()` to
# inspect the calls they create around the supplied function
lapply(1:3, call_inspect)
#> [[1]]
#> FUN(X[[i]])
#> 
#> [[2]]
#> FUN(X[[i]])
#> 
#> [[3]]
#> FUN(X[[i]])
#> 
```
