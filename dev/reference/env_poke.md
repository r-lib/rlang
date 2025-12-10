# Poke an object in an environment

`env_poke()` will assign or reassign a binding in `env` if `create` is
`TRUE`. If `create` is `FALSE` and a binding does not already exists, an
error is issued.

## Usage

``` r
env_poke(env = caller_env(), nm, value, inherit = FALSE, create = !inherit)
```

## Arguments

- env:

  An environment.

- nm:

  Name of binding, a string.

- value:

  The value for a new binding.

- inherit:

  Whether to look for bindings in the parent environments.

- create:

  Whether to create a binding if it does not already exist in the
  environment.

## Value

The old value of `nm` or a [zap
sentinel](https://rlang.r-lib.org/dev/reference/zap.md) if the binding
did not exist yet.

## Details

If `inherit` is `TRUE`, the parents environments are checked for an
existing binding to reassign. If not found and `create` is `TRUE`, a new
binding is created in `env`. The default value for `create` is a
function of `inherit`: `FALSE` when inheriting, `TRUE` otherwise.

This default makes sense because the inheriting case is mostly for
overriding an existing binding. If not found, something probably went
wrong and it is safer to issue an error. Note that this is different to
the base R operator `<<-` which will create a binding in the global
environment instead of the current environment when no existing binding
is found in the parents.

## See also

[`env_bind()`](https://rlang.r-lib.org/dev/reference/env_bind.md) for
binding multiple elements.
[`env_cache()`](https://rlang.r-lib.org/dev/reference/env_cache.md) for
a variant of `env_poke()` designed to cache values.
