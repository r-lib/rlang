# Create a quosure from components

- `new_quosure()` wraps any R object (including expressions, formulas,
  or other quosures) into a
  [quosure](https://rlang.r-lib.org/dev/reference/topic-quosure.md).

- `as_quosure()` is similar but it does not rewrap formulas and
  quosures.

## Usage

``` r
new_quosure(expr, env = caller_env())

as_quosure(x, env = NULL)

is_quosure(x)
```

## Arguments

- expr:

  An expression to wrap in a quosure.

- env:

  The environment in which the expression should be evaluated. Only used
  for symbols and calls. This should normally be the environment in
  which the expression was created.

- x:

  An object to test.

## See also

- [`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md) and
  [`quo()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md)
  for creating a quosure by [argument
  defusal](https://rlang.r-lib.org/dev/reference/topic-defuse.md).

- [What are quosures and when are they
  needed?](https://rlang.r-lib.org/dev/reference/topic-quosure.md)

## Examples

``` r
# `new_quosure()` creates a quosure from its components. These are
# equivalent:
new_quosure(quote(foo), current_env())
#> <quosure>
#> expr: ^foo
#> env:  0x5570af3fb778

quo(foo)
#> <quosure>
#> expr: ^foo
#> env:  0x5570af3fb778

# `new_quosure()` always rewraps its input into a new quosure, even
# if the input is itself a quosure:
new_quosure(quo(foo))
#> <quosure>
#> expr: ^<quosure>
#> env:  0x5570af3fb778

# This is unlike `as_quosure()` which preserves its input if it's
# already a quosure:
as_quosure(quo(foo))
#> <quosure>
#> expr: ^foo
#> env:  0x5570af3fb778


# `as_quosure()` uses the supplied environment with naked expressions:
env <- env(var = "thing")
as_quosure(quote(var), env)
#> <quosure>
#> expr: ^var
#> env:  0x5570b008beb8

# If the expression already carries an environment, this
# environment is preserved. This is the case for formulas and
# quosures:
as_quosure(~foo, env)
#> <quosure>
#> expr: ^foo
#> env:  0x5570af3fb778

as_quosure(~foo)
#> <quosure>
#> expr: ^foo
#> env:  0x5570af3fb778

# An environment must be supplied when the input is a naked
# expression:
try(
  as_quosure(quote(var))
)
#> Warning: `as_quosure()` requires an explicit environment as of rlang 0.3.0.
#> Please supply `env`.
#> This warning is displayed once every 8 hours.
#> <quosure>
#> expr: ^var
#> env:  0x5570af3fb778
```
