# Get or set formula components

`f_rhs` extracts the right-hand side, `f_lhs` extracts the left-hand
side, and `f_env` extracts the environment in which the formula was
defined. All functions throw an error if `f` is not a formula.

## Usage

``` r
f_rhs(f)

f_rhs(x) <- value

f_lhs(f)

f_lhs(x) <- value

f_env(f)

f_env(x) <- value
```

## Arguments

- f, x:

  A formula

- value:

  The value to replace with.

## Value

`f_rhs` and `f_lhs` return language objects (i.e. atomic vectors of
length 1, a name, or a call). `f_env` returns an environment.

## Examples

``` r
f_rhs(~ 1 + 2 + 3)
#> 1 + 2 + 3
f_rhs(~ x)
#> x
f_rhs(~ "A")
#> [1] "A"
f_rhs(1 ~ 2)
#> [1] 2

f_lhs(~ y)
#> NULL
f_lhs(x ~ y)
#> x

f_env(~ x)
#> <environment: 0x55eb8941edf8>
f <- as.formula("y ~ x", env = new.env())
f_env(f)
#> <environment: 0x55eb87ea1d08>
```
