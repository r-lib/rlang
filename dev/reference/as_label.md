# Create a default name for an R object

`as_label()` transforms R objects into a short, human-readable
description. You can use labels to:

- Display an object in a concise way, for example to labellise axes in a
  graphical plot.

- Give default names to columns in a data frame. In this case, labelling
  is the first step before name repair.

See also [`as_name()`](https://rlang.r-lib.org/dev/reference/as_name.md)
for transforming symbols back to a string. Unlike `as_label()`,
[`as_name()`](https://rlang.r-lib.org/dev/reference/as_name.md) is a
well defined operation that guarantees the roundtrip symbol -\> string
-\> symbol.

In general, if you don't know for sure what kind of object you're
dealing with (a call, a symbol, an unquoted constant), use `as_label()`
and make no assumption about the resulting string. If you know you have
a symbol and need the name of the object it refers to, use
[`as_name()`](https://rlang.r-lib.org/dev/reference/as_name.md). For
instance, use `as_label()` with objects captured with
[`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md) and
[`as_name()`](https://rlang.r-lib.org/dev/reference/as_name.md) with
symbols captured with
[`ensym()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md).

## Usage

``` r
as_label(x)
```

## Arguments

- x:

  An object.

## Transformation to string

- Quosures are
  [squashed](https://rlang.r-lib.org/dev/reference/quo_squash.md) before
  being labelled.

- Symbols are transformed to string with
  [`as_string()`](https://rlang.r-lib.org/dev/reference/as_string.md).

- Calls are abbreviated.

- Numbers are represented as such.

- Other constants are represented by their type, such as `<dbl>` or
  `<data.frame>`.

## See also

[`as_name()`](https://rlang.r-lib.org/dev/reference/as_name.md) for
transforming symbols back to a string deterministically.

## Examples

``` r
# as_label() is useful with quoted expressions:
as_label(expr(foo(bar)))
#> [1] "foo(bar)"

as_label(expr(foobar))
#> [1] "foobar"

# It works with any R object. This is also useful for quoted
# arguments because the user might unquote constant objects:
as_label(1:3)
#> [1] "<int>"

as_label(base::list)
#> [1] "<fn>"
```
