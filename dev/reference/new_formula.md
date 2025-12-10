# Create a formula

Create a formula

## Usage

``` r
new_formula(lhs, rhs, env = caller_env())
```

## Arguments

- lhs, rhs:

  A call, name, or atomic vector.

- env:

  An environment.

## Value

A formula object.

## See also

[`new_quosure()`](https://rlang.r-lib.org/dev/reference/new_quosure.md)

## Examples

``` r
new_formula(quote(a), quote(b))
#> a ~ b
#> <environment: 0x55b26d935430>
new_formula(NULL, quote(b))
#> ~b
#> <environment: 0x55b26d935430>
```
