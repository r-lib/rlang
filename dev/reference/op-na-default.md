# Replace missing values

**Note**: This operator is now out of scope for rlang. It will be
replaced by a vctrs-powered operator (probably in the [funs
package](https://github.com/tidyverse/funs)) at which point the rlang
version of `%|%` will be deprecated.

This infix function is similar to `%||%` but is vectorised and provides
a default value for missing elements. It is faster than using
[`base::ifelse()`](https://rdrr.io/r/base/ifelse.html) and does not
perform type conversions.

## Usage

``` r
x %|% y
```

## Arguments

- x:

  The original values.

- y:

  The replacement values. Must be of length 1 or the same length as `x`.

## See also

[op-null-default](https://rlang.r-lib.org/dev/reference/op-null-default.md)

## Examples

``` r
c("a", "b", NA, "c") %|% "default"
#> [1] "a"       "b"       "default" "c"      
c(1L, NA, 3L, NA, NA) %|% (6L:10L)
#> [1]  1  7  3  9 10
```
