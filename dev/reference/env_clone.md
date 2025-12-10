# Clone or coalesce an environment

- `env_clone()` creates a new environment containing exactly the same
  bindings as the input, optionally with a new parent.

- `env_coalesce()` copies binding from the RHS environment into the LHS.
  If the RHS already contains bindings with the same name as in the LHS,
  those are kept as is.

Both these functions preserve active bindings and promises.

## Usage

``` r
env_clone(env, parent = env_parent(env))

env_coalesce(env, from)
```

## Arguments

- env:

  An environment.

- parent:

  The parent of the cloned environment.

- from:

  Environment to copy bindings from.

## Examples

``` r
# A clone initially contains the same bindings as the original
# environment
env <- env(a = 1, b = 2)
clone <- env_clone(env)

env_print(clone)
#> <environment: 0x5570b146b278>
#> Parent: <environment: 0x5570af91d660>
#> Bindings:
#> • a: <dbl>
#> • b: <dbl>
env_print(env)
#> <environment: 0x5570b0fef078>
#> Parent: <environment: 0x5570af91d660>
#> Bindings:
#> • a: <dbl>
#> • b: <dbl>

# But it can acquire new bindings or change existing ones without
# impacting the original environment
env_bind(clone, a = "foo", c = 3)

env_print(clone)
#> <environment: 0x5570b146b278>
#> Parent: <environment: 0x5570af91d660>
#> Bindings:
#> • a: <chr>
#> • b: <dbl>
#> • c: <dbl>
env_print(env)
#> <environment: 0x5570b0fef078>
#> Parent: <environment: 0x5570af91d660>
#> Bindings:
#> • a: <dbl>
#> • b: <dbl>


# `env_coalesce()` copies bindings from one environment to another
lhs <- env(a = 1)
rhs <- env(a = "a", b = "b", c = "c")
env_coalesce(lhs, rhs)
env_print(lhs)
#> <environment: 0x5570aca071e0>
#> Parent: <environment: 0x5570af91d660>
#> Bindings:
#> • a: <dbl>
#> • b: <chr>
#> • c: <chr>

# To copy all the bindings from `rhs` into `lhs`, first delete the
# conflicting bindings from `rhs`
env_unbind(lhs, env_names(rhs))
env_coalesce(lhs, rhs)
env_print(lhs)
#> <environment: 0x5570aca071e0>
#> Parent: <environment: 0x5570af91d660>
#> Bindings:
#> • a: <chr>
#> • b: <chr>
#> • c: <chr>
```
