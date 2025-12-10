# Evaluate an expression within a given environment

**\[deprecated\]**

These functions evaluate `expr` within a given environment (`env` for
`with_env()`, or the child of the current environment for `locally`).
They rely on
[`eval_bare()`](https://rlang.r-lib.org/dev/reference/eval_bare.md)
which features a lighter evaluation mechanism than base R
[`base::eval()`](https://rdrr.io/r/base/eval.html), and which also has
some subtle implications when evaluting stack sensitive functions (see
help for
[`eval_bare()`](https://rlang.r-lib.org/dev/reference/eval_bare.md)).

`locally()` is equivalent to the base function
[`base::local()`](https://rdrr.io/r/base/eval.html) but it produces a
much cleaner evaluation stack, and has stack-consistent semantics. It is
thus more suited for experimenting with the R language.

## Usage

``` r
with_env(env, expr)

locally(expr)
```

## Arguments

- env:

  An environment within which to evaluate `expr`. Can be an object with
  a [`get_env()`](https://rlang.r-lib.org/dev/reference/get_env.md)
  method.

- expr:

  An expression to evaluate.

## Examples

``` r
# with_env() is handy to create formulas with a given environment:
env <- child_env("rlang")
f <- with_env(env, ~new_formula())
identical(f_env(f), env)
#> [1] TRUE

# Or functions with a given enclosure:
fn <- with_env(env, function() NULL)
identical(get_env(fn), env)
#> [1] TRUE


# Unlike eval() it doesn't create duplicates on the evaluation
# stack. You can thus use it e.g. to create non-local returns:
fn <- function() {
  g(current_env())
  "normal return"
}
g <- function(env) {
  with_env(env, return("early return"))
}
fn()
#> [1] "early return"


# Since env is passed to as_environment(), it can be any object with an
# as_environment() method. For strings, the pkg_env() is returned:
with_env("base", ~mtcars)
#> ~mtcars
#> <environment: base>

# This can be handy to put dictionaries in scope:
with_env(mtcars, cyl)
#>  [1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4
```
