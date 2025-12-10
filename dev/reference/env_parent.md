# Get parent environments

- `env_parent()` returns the parent environment of `env` if called with
  `n = 1`, the grandparent with `n = 2`, etc.

- `env_tail()` searches through the parents and returns the one which
  has
  [`empty_env()`](https://rlang.r-lib.org/dev/reference/empty_env.md) as
  parent.

- `env_parents()` returns the list of all parents, including the empty
  environment. This list is named using
  [`env_name()`](https://rlang.r-lib.org/dev/reference/env_name.md).

See the section on *inheritance* in
[`env()`](https://rlang.r-lib.org/dev/reference/env.md)'s documentation.

## Usage

``` r
env_parent(env = caller_env(), n = 1)

env_tail(env = caller_env(), last = global_env())

env_parents(env = caller_env(), last = global_env())
```

## Arguments

- env:

  An environment.

- n:

  The number of generations to go up.

- last:

  The environment at which to stop. Defaults to the global environment.
  The empty environment is always a stopping condition so it is safe to
  leave the default even when taking the tail or the parents of an
  environment on the search path.

  `env_tail()` returns the environment which has `last` as parent and
  `env_parents()` returns the list of environments up to `last`.

## Value

An environment for `env_parent()` and `env_tail()`, a list of
environments for `env_parents()`.

## Examples

``` r
# Get the parent environment with env_parent():
env_parent(global_env())
#> <environment: package:rlang>
#> attr(,"name")
#> [1] "package:rlang"
#> attr(,"path")
#> [1] "/home/runner/work/_temp/Library/rlang"

# Or the tail environment with env_tail():
env_tail(global_env())
#> <environment: base>

# By default, env_parent() returns the parent environment of the
# current evaluation frame. If called at top-level (the global
# frame), the following two expressions are equivalent:
env_parent()
#> <environment: 0x5570af7b64c8>
env_parent(base_env())
#> <environment: R_EmptyEnv>

# This default is more handy when called within a function. In this
# case, the enclosure environment of the function is returned
# (since it is the parent of the evaluation frame):
enclos_env <- env()
fn <- set_env(function() env_parent(), enclos_env)
identical(enclos_env, fn())
#> [1] TRUE
```
