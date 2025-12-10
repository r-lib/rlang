# Lock or unlock environment bindings

**\[experimental\]**

Locked environment bindings trigger an error when an attempt is made to
redefine the binding.

## Usage

``` r
env_binding_lock(env, nms = NULL)

env_binding_unlock(env, nms = NULL)

env_binding_are_locked(env, nms = NULL)
```

## Arguments

- env:

  An environment.

- nms:

  Names of bindings. Defaults to all bindings in `env`.

## Value

`env_binding_are_unlocked()` returns a logical vector as long as `nms`
and named after it. `env_binding_lock()` and `env_binding_unlock()`
return the old value of `env_binding_are_unlocked()` invisibly.

## See also

[`env_lock()`](https://rlang.r-lib.org/dev/reference/env_lock.md) for
locking an environment.

## Examples

``` r
# Bindings are unlocked by default:
env <- env(a = "A", b = "B")
env_binding_are_locked(env)
#>     a     b 
#> FALSE FALSE 

# But can optionally be locked:
env_binding_lock(env, "a")
env_binding_are_locked(env)
#>     a     b 
#>  TRUE FALSE 

# If run, the following would now return an error because `a` is locked:
# env_bind(env, a = "foo")
# with_env(env, a <- "bar")

# Let's unlock it. Note that the return value indicate which
# bindings were locked:
were_locked <- env_binding_unlock(env)
were_locked
#>     a     b 
#>  TRUE FALSE 

# Now that it is unlocked we can modify it again:
env_bind(env, a = "foo")
with_env(env, a <- "bar")
env$a
#> [1] "bar"
```
