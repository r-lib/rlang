# Convert object to a box

- `as_box()` boxes its input only if it is not already a box. The class
  is also checked if supplied.

- `as_box_if()` boxes its input only if it not already a box, or if the
  predicate `.p` returns `TRUE`.

## Usage

``` r
as_box(x, class = NULL)

as_box_if(.x, .p, .class = NULL, ...)
```

## Arguments

- x, .x:

  An R object.

- class, .class:

  A box class. If the input is already a box of that class, it is
  returned as is. If the input needs to be boxed, `class` is passed to
  [`new_box()`](https://rlang.r-lib.org/dev/reference/box.md).

- .p:

  A predicate function.

- ...:

  Arguments passed to `.p`.
