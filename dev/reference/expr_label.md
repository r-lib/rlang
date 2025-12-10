# Turn an expression to a label

**\[questioning\]**

`expr_text()` turns the expression into a single string, which might be
multi-line. `expr_name()` is suitable for formatting names. It works
best with symbols and scalar types, but also accepts calls.
`expr_label()` formats the expression nicely for use in messages.

## Usage

``` r
expr_label(expr)

expr_name(expr)

expr_text(expr, width = 60L, nlines = Inf)
```

## Arguments

- expr:

  An expression to labellise.

- width:

  Width of each line.

- nlines:

  Maximum number of lines to extract.

## Examples

``` r
# To labellise a function argument, first capture it with
# substitute():
fn <- function(x) expr_label(substitute(x))
fn(x:y)
#> [1] "`x:y`"

# Strings are encoded
expr_label("a\nb")
#> [1] "\"a\\nb\""

# Names and expressions are quoted with ``
expr_label(quote(x))
#> [1] "`x`"
expr_label(quote(a + b + c))
#> [1] "`a + b + c`"

# Long expressions are collapsed
expr_label(quote(foo({
  1 + 2
  print(x)
})))
#> [1] "`foo(...)`"
```
