# Defuse an R expression

`expr()` [defuses](https://rlang.r-lib.org/reference/topic-defuse.md) an
R expression with
[injection](https://rlang.r-lib.org/reference/injection-operator.md)
support.

It is equivalent to
[`base::bquote()`](https://rdrr.io/r/base/bquote.html).

## Arguments

- expr:

  An expression to defuse.

## See also

- [Defusing R
  expressions](https://rlang.r-lib.org/reference/topic-defuse.md) for an
  overview.

- [`enquo()`](https://rlang.r-lib.org/reference/enquo.md) to defuse
  non-local expressions from function arguments.

- [Advanced defusal
  operators](https://rlang.r-lib.org/reference/defusing-advanced.md).

- [`sym()`](https://rlang.r-lib.org/reference/sym.md) and
  [`call2()`](https://rlang.r-lib.org/reference/call2.md) for building
  expressions (symbols and calls respectively) programmatically.

- [`base::eval()`](https://rdrr.io/r/base/eval.html) and
  [`eval_bare()`](https://rlang.r-lib.org/reference/eval_bare.md) for
  resuming evaluation of a defused expression.

## Examples

``` r
# R normally returns the result of an expression
1 + 1
#> [1] 2

# `expr()` defuses the expression that you have supplied and
# returns it instead of its value
expr(1 + 1)
#> 1 + 1

expr(toupper(letters))
#> toupper(letters)

# It supports _injection_ with `!!` and `!!!`. This is a convenient
# way of modifying part of an expression by injecting other
# objects.
var <- "cyl"
expr(with(mtcars, mean(!!sym(var))))
#> with(mtcars, mean(cyl))

vars <- c("cyl", "am")
expr(with(mtcars, c(!!!syms(vars))))
#> with(mtcars, c(cyl, am))

# Compare to the normal way of building expressions
call("with", call("mean", sym(var)))
#> with(mean(cyl))

call("with", call2("c", !!!syms(vars)))
#> with(c(cyl, am))
```
