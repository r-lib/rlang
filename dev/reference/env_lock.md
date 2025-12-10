# Lock an environment

**\[experimental\]**

Locked environments cannot be modified. An important example is
namespace environments which are locked by R when loaded in a session.
Once an environment is locked it normally cannot be unlocked.

Note that only the environment as a container is locked, not the
individual bindings. You can't remove or add a binding but you can still
modify the values of existing bindings. See
[`env_binding_lock()`](https://rlang.r-lib.org/dev/reference/env_binding_lock.md)
for locking individual bindings.

## Usage

``` r
env_lock(env)

env_is_locked(env)
```

## Arguments

- env:

  An environment.

## Value

The old value of `env_is_locked()` invisibly.

## See also

[`env_binding_lock()`](https://rlang.r-lib.org/dev/reference/env_binding_lock.md)

## Examples

``` r
# New environments are unlocked by default:
env <- env(a = 1)
env_is_locked(env)
#> [1] FALSE

# Use env_lock() to lock them:
env_lock(env)
env_is_locked(env)
#> [1] TRUE

# Now that `env` is locked, it is no longer possible to remove or
# add bindings. If run, the following would fail:
# env_unbind(env, "a")
# env_bind(env, b = 2)

# Note that even though the environment as a container is locked,
# the individual bindings are still unlocked and can be modified:
env$a <- 10
```
