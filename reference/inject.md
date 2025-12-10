# Inject objects in an R expression

`inject()` evaluates an expression with
[injection](https://rlang.r-lib.org/reference/topic-inject.md) support.
There are three main usages:

- [Splicing](https://rlang.r-lib.org/reference/splice-operator.md) lists
  of arguments in a function call.

- Inline objects or other expressions in an expression with `!!` and
  `!!!`. For instance to create functions or formulas programmatically.

- Pass arguments to NSE functions that
  [defuse](https://rlang.r-lib.org/reference/topic-defuse.md) their
  arguments without injection support (see for instance
  [`enquo0()`](https://rlang.r-lib.org/reference/defusing-advanced.md)).
  You can use `{{ arg }}` with functions documented to support quosures.
  Otherwise, use `!!enexpr(arg)`.

## Usage

``` r
inject(expr, env = caller_env())
```

## Arguments

- expr:

  An argument to evaluate. This argument is immediately evaluated in
  `env` (the current environment by default) with injected objects and
  expressions.

- env:

  The environment in which to evaluate `expr`. Defaults to the current
  environment. For expert use only.

## Examples

``` r
# inject() simply evaluates its argument with injection
# support. These expressions are equivalent:
2 * 3
#> [1] 6
inject(2 * 3)
#> [1] 6
inject(!!2 * !!3)
#> [1] 6

# Injection with `!!` can be useful to insert objects or
# expressions within other expressions, like formulas:
lhs <- sym("foo")
rhs <- sym("bar")
inject(!!lhs ~ !!rhs + 10)
#> foo ~ bar + 10
#> <environment: 0x560f9975c7a0>

# Injection with `!!!` splices lists of arguments in function
# calls:
args <- list(na.rm = TRUE, finite = 0.2)
inject(mean(1:10, !!!args))
#> [1] 5.5
```
