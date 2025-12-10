# Turn RHS of formula into a string or label

Equivalent of
[`expr_text()`](https://rlang.r-lib.org/dev/reference/expr_label.md) and
[`expr_label()`](https://rlang.r-lib.org/dev/reference/expr_label.md)
for formulas.

## Usage

``` r
f_text(x, width = 60L, nlines = Inf)

f_name(x)

f_label(x)
```

## Arguments

- x:

  A formula.

- width:

  Width of each line.

- nlines:

  Maximum number of lines to extract.

## Examples

``` r
f <- ~ a + b + bc
f_text(f)
#> [1] "a + b + bc"
f_label(f)
#> [1] "`a + b + bc`"

# Names a quoted with ``
f_label(~ x)
#> [1] "`x`"
# Strings are encoded
f_label(~ "a\nb")
#> [1] "\"a\\nb\""
# Long expressions are collapsed
f_label(~ foo({
  1 + 2
  print(x)
}))
#> [1] "`foo(...)`"
```
