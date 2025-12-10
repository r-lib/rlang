# Squash a quosure

**\[deprecated\]** This function is deprecated, please use
[`quo_squash()`](https://rlang.r-lib.org/reference/quo_squash.md)
instead.

## Usage

``` r
quo_expr(quo, warn = FALSE)
```

## Arguments

- quo:

  A quosure or expression.

- warn:

  Whether to warn if the quosure contains other quosures (those will be
  collapsed). This is useful when you use
  [`quo_squash()`](https://rlang.r-lib.org/reference/quo_squash.md) in
  order to make a non-tidyeval API compatible with quosures. In that
  case, getting rid of the nested quosures is likely to cause subtle
  bugs and it is good practice to warn the user about it.
