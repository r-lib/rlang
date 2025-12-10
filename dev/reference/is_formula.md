# Is object a formula?

`is_formula()` tests whether `x` is a call to `~`. `is_bare_formula()`
tests in addition that `x` does not inherit from anything else than
`"formula"`.

**Note**: When we first implemented `is_formula()`, we thought it best
to treat unevaluated formulas as formulas by default (see section
below). Now we think this default introduces too many edge cases in
normal code. We recommend always supplying `scoped = TRUE`. Unevaluated
formulas can be handled via a `is_call(x, "~")` branch.

## Usage

``` r
is_formula(x, scoped = NULL, lhs = NULL)

is_bare_formula(x, scoped = TRUE, lhs = NULL)
```

## Arguments

- x:

  An object to test.

- scoped:

  A boolean indicating whether the quosure is scoped, that is, has a
  valid environment attribute and inherits from `"formula"`. If `NULL`,
  the scope is not inspected.

- lhs:

  A boolean indicating whether the formula has a left-hand side. If
  `NULL`, the LHS is not inspected and `is_formula()` returns `TRUE` for
  both one- and two-sided formulas.

## Dealing with unevaluated formulas

At parse time, a formula is a simple call to `~` and it does not have a
class or an environment. Once evaluated, the `~` call becomes a properly
structured formula. Unevaluated formulas arise by quotation, e.g.
`~~foo`, `quote(~foo)`, or `substitute(arg)` with `arg` being supplied a
formula. Use the `scoped` argument to check whether the formula carries
an environment.

## Examples

``` r
is_formula(~10)
#> [1] TRUE
is_formula(10)
#> [1] FALSE

# If you don't supply `lhs`, both one-sided and two-sided formulas
# will return `TRUE`
is_formula(disp ~ am)
#> [1] TRUE
is_formula(~am)
#> [1] TRUE

# You can also specify whether you expect a LHS:
is_formula(disp ~ am, lhs = TRUE)
#> [1] TRUE
is_formula(disp ~ am, lhs = FALSE)
#> [1] FALSE
is_formula(~am, lhs = TRUE)
#> [1] FALSE
is_formula(~am, lhs = FALSE)
#> [1] TRUE

# Handling of unevaluated formulas is a bit tricky. These formulas
# are special because they don't inherit from `"formula"` and they
# don't carry an environment (they are not scoped):
f <- quote(~foo)
f_env(f)
#> NULL

# By default unevaluated formulas are treated as formulas
is_formula(f)
#> [1] TRUE

# Supply `scoped = TRUE` to ensure you have an evaluated formula
is_formula(f, scoped = TRUE)
#> [1] FALSE

# By default unevaluated formulas not treated as bare formulas
is_bare_formula(f)
#> [1] FALSE

# If you supply `scoped = TRUE`, they will be considered bare
# formulas even though they don't inherit from `"formula"`
is_bare_formula(f, scoped = TRUE)
#> [1] FALSE
```
