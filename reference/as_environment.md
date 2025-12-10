# Coerce to an environment

`as_environment()` coerces named vectors (including lists) to an
environment. The names must be unique. If supplied an unnamed string, it
returns the corresponding package environment (see
[`pkg_env()`](https://rlang.r-lib.org/reference/search_envs.md)).

## Usage

``` r
as_environment(x, parent = NULL)
```

## Arguments

- x:

  An object to coerce.

- parent:

  A parent environment,
  [`empty_env()`](https://rlang.r-lib.org/reference/empty_env.md) by
  default. This argument is only used when `x` is data actually coerced
  to an environment (as opposed to data representing an environment,
  like `NULL` representing the empty environment).

## Details

If `x` is an environment and `parent` is not `NULL`, the environment is
duplicated before being set a new parent. The return value is therefore
a different environment than `x`.

## Examples

``` r
# Coerce a named vector to an environment:
env <- as_environment(mtcars)

# By default it gets the empty environment as parent:
identical(env_parent(env), empty_env())
#> [1] TRUE


# With strings it is a handy shortcut for pkg_env():
as_environment("base")
#> <environment: base>
as_environment("rlang")
#> <environment: package:rlang>
#> attr(,"name")
#> [1] "package:rlang"
#> attr(,"path")
#> [1] "/home/runner/work/_temp/Library/rlang"

# With NULL it returns the empty environment:
as_environment(NULL)
#> <environment: R_EmptyEnv>
```
