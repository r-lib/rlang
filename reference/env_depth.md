# Depth of an environment chain

This function returns the number of environments between `env` and the
[empty environment](https://rlang.r-lib.org/reference/empty_env.md),
including `env`. The depth of `env` is also the number of parents of
`env` (since the empty environment counts as a parent).

## Usage

``` r
env_depth(env)
```

## Arguments

- env:

  An environment.

## Value

An integer.

## See also

The section on inheritance in
[`env()`](https://rlang.r-lib.org/reference/env.md) documentation.

## Examples

``` r
env_depth(empty_env())
#> [1] 0
env_depth(pkg_env("rlang"))
#> [1] 9
```
