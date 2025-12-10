# Squash a quosure

`quo_squash()` flattens all nested quosures within an expression. For
example it transforms `^foo(^bar(), ^baz)` to the bare expression
`foo(bar(), baz)`.

This operation is safe if the squashed quosure is used for labelling or
printing (see
[`as_label()`](https://rlang.r-lib.org/dev/reference/as_label.md), but
note that
[`as_label()`](https://rlang.r-lib.org/dev/reference/as_label.md)
squashes quosures automatically). However if the squashed quosure is
evaluated, all expressions of the flattened quosures are resolved in a
single environment. This is a source of bugs so it is good practice to
set `warn` to `TRUE` to let the user know about the lossy squashing.

## Usage

``` r
quo_squash(quo, warn = FALSE)
```

## Arguments

- quo:

  A quosure or expression.

- warn:

  Whether to warn if the quosure contains other quosures (those will be
  collapsed). This is useful when you use `quo_squash()` in order to
  make a non-tidyeval API compatible with quosures. In that case,
  getting rid of the nested quosures is likely to cause subtle bugs and
  it is good practice to warn the user about it.

## Examples

``` r
# Quosures can contain nested quosures:
quo <- quo(wrapper(!!quo(wrappee)))
quo
#> <quosure>
#> expr: ^wrapper(^wrappee)
#> env:  0x55b26f6b64c0

# quo_squash() flattens all the quosures and returns a simple expression:
quo_squash(quo)
#> wrapper(wrappee)
```
