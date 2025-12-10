# Parse R code

These functions parse and transform text into R expressions. This is the
first step to interpret or evaluate a piece of R code written by a
programmer.

- `parse_expr()` returns one expression. If the text contains more than
  one expression (separated by semicolons or new lines), an error is
  issued. On the other hand `parse_exprs()` can handle multiple
  expressions. It always returns a list of expressions (compare to
  [`base::parse()`](https://rdrr.io/r/base/parse.html) which returns a
  base::expression vector). All functions also support R connections.

- `parse_expr()` concatenates `x` with `\\n` separators prior to parsing
  in order to support the roundtrip `parse_expr(expr_deparse(x))`
  (deparsed expressions might be multiline). On the other hand,
  `parse_exprs()` doesn't do any concatenation because it's designed to
  support named inputs. The names are matched to the expressions in the
  output, which is useful when a single named string creates multiple
  expressions.

  In other words, `parse_expr()` supports vector of lines whereas
  `parse_exprs()` expects vectors of complete deparsed expressions.

- `parse_quo()` and `parse_quos()` are variants that create a
  [quosure](https://rlang.r-lib.org/dev/reference/defusing-advanced.md).
  Supply `env = current_env()` if you're parsing code to be evaluated in
  your current context. Supply `env = global_env()` when you're parsing
  external user input to be evaluated in user context.

  Unlike quosures created with
  [`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md),
  [`enquos()`](https://rlang.r-lib.org/dev/reference/enquo.md), or `{{`,
  a parsed quosure never contains injected quosures. It is thus safe to
  evaluate them with [`eval()`](https://rdrr.io/r/base/eval.html)
  instead of
  [`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md),
  though the latter is more convenient as you don't need to extract
  `expr` and `env`.

## Usage

``` r
parse_expr(x)

parse_exprs(x)

parse_quo(x, env)

parse_quos(x, env)
```

## Arguments

- x:

  Text containing expressions to parse_expr for `parse_expr()` and
  `parse_exprs()`. Can also be an R connection, for instance to a file.
  If the supplied connection is not open, it will be automatically
  closed and destroyed.

- env:

  The environment for the quosures. The [global
  environment](https://rlang.r-lib.org/dev/reference/search_envs.md)
  (the default) may be the right choice when you are parsing external
  user inputs. You might also want to evaluate the R code in an isolated
  context (perhaps a child of the global environment or of the [base
  environment](https://rlang.r-lib.org/dev/reference/search_envs.md)).

## Value

`parse_expr()` returns an
[expression](https://rlang.r-lib.org/dev/reference/is_expression.md),
`parse_exprs()` returns a list of expressions. Note that for the plural
variants the length of the output may be greater than the length of the
input. This would happen is one of the strings contain several
expressions (such as `"foo; bar"`). The names of `x` are preserved (and
recycled in case of multiple expressions). The `_quo` suffixed variants
return quosures.

## Details

Unlike [`base::parse()`](https://rdrr.io/r/base/parse.html), these
functions never retain source reference information, as doing so is slow
and rarely necessary.

## See also

[`base::parse()`](https://rdrr.io/r/base/parse.html)

## Examples

``` r
# parse_expr() can parse any R expression:
parse_expr("mtcars %>% dplyr::mutate(cyl_prime = cyl / sd(cyl))")
#> mtcars %>% dplyr::mutate(cyl_prime = cyl/sd(cyl))

# A string can contain several expressions separated by ; or \n
parse_exprs("NULL; list()\n foo(bar)")
#> [[1]]
#> NULL
#> 
#> [[2]]
#> list()
#> 
#> [[3]]
#> foo(bar)
#> 

# Use names to figure out which input produced an expression:
parse_exprs(c(foo = "1; 2", bar = "3"))
#> $foo
#> [1] 1
#> 
#> $foo
#> [1] 2
#> 
#> $bar
#> [1] 3
#> 

# You can also parse source files by passing a R connection. Let's
# create a file containing R code:
path <- tempfile("my-file.R")
cat("1; 2; mtcars", file = path)

# We can now parse it by supplying a connection:
parse_exprs(file(path))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> mtcars
#> 
```
