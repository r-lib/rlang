# What type is a condition?

Use `cnd_type()` to check what type a condition is.

## Usage

``` r
cnd_type(cnd)
```

## Arguments

- cnd:

  A condition object.

## Value

A string, either `"condition"`, `"message"`, `"warning"`, `"error"` or
`"interrupt"`.

## Examples

``` r
cnd_type(catch_cnd(abort("Abort!")))
#> [1] "error"
cnd_type(catch_cnd(interrupt()))
#> [1] "interrupt"
```
