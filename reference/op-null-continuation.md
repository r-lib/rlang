# Default value for non-`NULL`

This infix operator is the conceptual opposite of `%||%`, providing a
fallback only if `x` is defined.

## Usage

``` r
x %&&% y
```

## Arguments

- x, y:

  If `x` is NULL, will return `x`; otherwise returns `y`.

## See also

[op-null-default](https://rlang.r-lib.org/reference/op-null-default.md)

## Examples

``` r
1 %&&% 2
#> [1] 2
NULL %&&% 2
#> NULL
```
