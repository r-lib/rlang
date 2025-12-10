# Check that dots are empty (low level variant)

`check_dots_empty0()` is a more efficient version of
[`check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.md)
with a slightly different interface. Instead of inspecting the current
environment for dots, it directly takes `...`. It is only meant for very
low level functions where a couple microseconds make a difference.

## Usage

``` r
check_dots_empty0(..., call = caller_env())
```

## Arguments

- ...:

  Dots which should be empty.
