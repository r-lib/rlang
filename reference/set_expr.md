# Set and get an expression

These helpers are useful to make your function work generically with
quosures and raw expressions. First call `get_expr()` to extract an
expression. Once you're done processing the expression, call
`set_expr()` on the original object to update the expression. You can
return the result of `set_expr()`, either a formula or an expression
depending on the input type. Note that `set_expr()` does not change its
input, it creates a new object.

## Usage

``` r
set_expr(x, value)

get_expr(x, default = x)
```

## Arguments

- x:

  An expression, closure, or one-sided formula. In addition,
  `set_expr()` accept frames.

- value:

  An updated expression.

- default:

  A default expression to return when `x` is not an expression wrapper.
  Defaults to `x` itself.

## Value

The updated original input for `set_expr()`. A raw expression for
`get_expr()`.

## See also

[`quo_get_expr()`](https://rlang.r-lib.org/reference/quosure-tools.md)
and
[`quo_set_expr()`](https://rlang.r-lib.org/reference/quosure-tools.md)
for versions of `get_expr()` and `set_expr()` that only work on
quosures.

## Examples

``` r
f <- ~foo(bar)
e <- quote(foo(bar))
frame <- identity(identity(ctxt_frame()))
#> Warning: `ctxt_frame()` is deprecated as of rlang 0.3.0.
#> This warning is displayed once every 8 hours.

get_expr(f)
#> foo(bar)
get_expr(e)
#> foo(bar)
get_expr(frame)
#> identity(ctxt_frame())

set_expr(f, quote(baz))
#> ~baz
#> <environment: 0x5577a0094cb0>
set_expr(e, quote(baz))
#> baz
```
