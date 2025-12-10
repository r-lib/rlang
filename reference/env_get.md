# Get an object in an environment

`env_get()` extracts an object from an enviroment `env`. By default, it
does not look in the parent environments. `env_get_list()` extracts
multiple objects from an environment into a named list.

## Usage

``` r
env_get(env = caller_env(), nm, default, inherit = FALSE, last = empty_env())

env_get_list(
  env = caller_env(),
  nms,
  default,
  inherit = FALSE,
  last = empty_env()
)
```

## Arguments

- env:

  An environment.

- nm:

  Name of binding, a string.

- default:

  A default value in case there is no binding for `nm` in `env`.

- inherit:

  Whether to look for bindings in the parent environments.

- last:

  Last environment inspected when `inherit` is `TRUE`. Can be useful in
  conjunction with
  [`base::topenv()`](https://rdrr.io/r/base/ns-topenv.html).

- nms:

  Names of bindings, a character vector.

## Value

An object if it exists. Otherwise, throws an error.

## See also

[`env_cache()`](https://rlang.r-lib.org/reference/env_cache.md) for a
variant of `env_get()` designed to cache a value in an environment.

## Examples

``` r
parent <- child_env(NULL, foo = "foo")
env <- child_env(parent, bar = "bar")

# This throws an error because `foo` is not directly defined in env:
# env_get(env, "foo")

# However `foo` can be fetched in the parent environment:
env_get(env, "foo", inherit = TRUE)
#> [1] "foo"

# You can also avoid an error by supplying a default value:
env_get(env, "foo", default = "FOO")
#> [1] "FOO"
```
