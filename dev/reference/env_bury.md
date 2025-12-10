# Mask bindings by defining symbols deeper in a scope

**\[superseded\]**

This function is superseded. Please use
[`env()`](https://rlang.r-lib.org/dev/reference/env.md) (and possibly
[`set_env()`](https://rlang.r-lib.org/dev/reference/get_env.md) if
you're masking the bindings for another object like a closure or a
formula) instead.

`env_bury()` is like
[`env_bind()`](https://rlang.r-lib.org/dev/reference/env_bind.md) but it
creates the bindings in a new child environment. This makes sure the new
bindings have precedence over old ones, without altering existing
environments. Unlike
[`env_bind()`](https://rlang.r-lib.org/dev/reference/env_bind.md), this
function does not have side effects and returns a new environment (or
object wrapping that environment).

## Usage

``` r
env_bury(.env, ...)
```

## Arguments

- .env:

  An environment.

- ...:

  \<[dynamic](https://rlang.r-lib.org/dev/reference/dyn-dots.md)\> Named
  objects
  ([`env_bind()`](https://rlang.r-lib.org/dev/reference/env_bind.md)),
  expressions
  [`env_bind_lazy()`](https://rlang.r-lib.org/dev/reference/env_bind.md),
  or functions
  ([`env_bind_active()`](https://rlang.r-lib.org/dev/reference/env_bind.md)).
  Use [`zap()`](https://rlang.r-lib.org/dev/reference/zap.md) to remove
  bindings.

## Value

A copy of `.env` enclosing the new environment containing bindings to
`...` arguments.

## See also

[`env_bind()`](https://rlang.r-lib.org/dev/reference/env_bind.md),
[`env_unbind()`](https://rlang.r-lib.org/dev/reference/env_unbind.md)

## Examples

``` r
orig_env <- env(a = 10)
fn <- set_env(function() a, orig_env)

# fn() currently sees `a` as the value `10`:
fn()
#> [1] 10

# env_bury() will bury the current scope of fn() behind a new
# environment:
fn <- env_bury(fn, a = 1000)
fn()
#> [1] 1000

# Even though the symbol `a` is still defined deeper in the scope:
orig_env$a
#> [1] 10
```
