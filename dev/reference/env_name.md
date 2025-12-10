# Label of an environment

Special environments like the global environment have their own names.
`env_name()` returns:

- "global" for the global environment.

- "empty" for the empty environment.

- "base" for the base package environment (the last environment on the
  search path).

- "namespace:pkg" if `env` is the namespace of the package "pkg".

- The `name` attribute of `env` if it exists. This is how the [package
  environments](https://rlang.r-lib.org/dev/reference/search_envs.md)
  and the [imports
  environments](https://rlang.r-lib.org/dev/reference/ns_env.md) store
  their names. The name of package environments is typically
  "package:pkg".

- The empty string `""` otherwise.

`env_label()` is exactly like `env_name()` but returns the memory
address of anonymous environments as fallback.

## Usage

``` r
env_name(env)

env_label(env)
```

## Arguments

- env:

  An environment.

## Examples

``` r
# Some environments have specific names:
env_name(global_env())
#> [1] "global"
env_name(ns_env("rlang"))
#> [1] "namespace:rlang"

# Anonymous environments don't have names but are labelled by their
# address in memory:
env_name(env())
#> [1] ""
env_label(env())
#> [1] "0x55b26fb9cbc0"
```
