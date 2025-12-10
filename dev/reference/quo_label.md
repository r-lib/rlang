# Format quosures for printing or labelling

**\[superseded\]**

**Note:** You should now use
[`as_label()`](https://rlang.r-lib.org/dev/reference/as_label.md) or
[`as_name()`](https://rlang.r-lib.org/dev/reference/as_name.md) instead
of `quo_name()`. See life cycle section below.

These functions take an arbitrary R object, typically an
[expression](https://rlang.r-lib.org/dev/reference/is_expression.md),
and represent it as a string.

- `quo_name()` returns an abbreviated representation of the object as a
  single line string. It is suitable for default names.

- `quo_text()` returns a multiline string. For instance block
  expressions like `{ foo; bar }` are represented on 4 lines (one for
  each symbol, and the curly braces on their own lines).

These deparsers are only suitable for creating default names or printing
output at the console. The behaviour of your functions should not depend
on deparsed objects. If you are looking for a way of transforming
symbols to strings, use
[`as_string()`](https://rlang.r-lib.org/dev/reference/as_string.md)
instead of `quo_name()`. Unlike deparsing, the transformation between
symbols and strings is non-lossy and well defined.

## Usage

``` r
quo_label(quo)

quo_text(quo, width = 60L, nlines = Inf)

quo_name(quo)
```

## Arguments

- quo:

  A quosure or expression.

- width:

  Width of each line.

- nlines:

  Maximum number of lines to extract.

## Life cycle

These functions are superseded.

- [`as_label()`](https://rlang.r-lib.org/dev/reference/as_label.md) and
  [`as_name()`](https://rlang.r-lib.org/dev/reference/as_name.md) should
  be used instead of `quo_name()`.
  [`as_label()`](https://rlang.r-lib.org/dev/reference/as_label.md)
  transforms any R object to a string but should only be used to create
  a default name. Labelisation is not a well defined operation and no
  assumption should be made about the label. On the other hand,
  [`as_name()`](https://rlang.r-lib.org/dev/reference/as_name.md) only
  works with (possibly quosured) symbols, but is a well defined and
  deterministic operation.

- We don't have a good replacement for `quo_text()` yet. See
  <https://github.com/r-lib/rlang/issues/636> to follow discussions
  about a new deparsing API.

## See also

[`expr_label()`](https://rlang.r-lib.org/dev/reference/expr_label.md),
[`f_label()`](https://rlang.r-lib.org/dev/reference/f_text.md)

## Examples

``` r
# Quosures can contain nested quosures:
quo <- quo(foo(!! quo(bar)))
quo
#> <quosure>
#> expr: ^foo(^bar)
#> env:  0x5570ad0e55e0

# quo_squash() unwraps all quosures and returns a raw expression:
quo_squash(quo)
#> foo(bar)

# This is used by quo_text() and quo_label():
quo_text(quo)
#> [1] "foo(bar)"

# Compare to the unwrapped expression:
expr_text(quo)
#> [1] "~foo(~bar)"

# quo_name() is helpful when you need really short labels:
quo_name(quo(sym))
#> [1] "sym"
quo_name(quo(!! sym))
#> [1] "function (x) ..."
```
