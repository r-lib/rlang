# Box a final value for early termination

A value boxed with `done()` signals to its caller that it should stop
iterating. Use it to shortcircuit a loop.

## Usage

``` r
done(x)

is_done_box(x, empty = NULL)
```

## Arguments

- x:

  For `done()`, a value to box. For `is_done_box()`, a value to test.

- empty:

  Whether the box is empty. If `NULL`, `is_done_box()` returns `TRUE`
  for all done boxes. If `TRUE`, it returns `TRUE` only for empty boxes.
  Otherwise it returns `TRUE` only for non-empty boxes.

## Value

A [boxed](https://rlang.r-lib.org/reference/box.md) value.

## Examples

``` r
done(3)
#> <done>
#> [1] 3

x <- done(3)
is_done_box(x)
#> [1] TRUE
```
