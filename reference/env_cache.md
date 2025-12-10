# Cache a value in an environment

`env_cache()` is a wrapper around
[`env_get()`](https://rlang.r-lib.org/reference/env_get.md) and
[`env_poke()`](https://rlang.r-lib.org/reference/env_poke.md) designed
to retrieve a cached value from `env`.

- If the `nm` binding exists, it returns its value.

- Otherwise, it stores the default value in `env` and returns that.

## Usage

``` r
env_cache(env, nm, default)
```

## Arguments

- env:

  An environment.

- nm:

  Name of binding, a string.

- default:

  The default value to store in `env` if `nm` does not exist yet.

## Value

Either the value of `nm` or `default` if it did not exist yet.

## Examples

``` r
e <- env(a = "foo")

# Returns existing binding
env_cache(e, "a", "default")
#> [1] "foo"

# Creates a `b` binding and returns its default value
env_cache(e, "b", "default")
#> [1] "default"

# Now `b` is defined
e$b
#> [1] "default"
```
