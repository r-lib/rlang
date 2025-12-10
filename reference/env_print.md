# Pretty-print an environment

This prints:

- The [label](https://rlang.r-lib.org/reference/env_name.md) and the
  parent label.

- Whether the environment is
  [locked](https://rlang.r-lib.org/reference/env_lock.md).

- The bindings in the environment (up to 20 bindings). They are printed
  succinctly using
  [`pillar::type_sum()`](https://pillar.r-lib.org/reference/type_sum.html)
  (if available, otherwise uses an internal version of that generic). In
  addition [fancy
  bindings](https://rlang.r-lib.org/reference/env_bind.md) (actives and
  promises) are indicated as such.

- Locked bindings get a `[L]` tag

Note that printing a package namespace (see
[`ns_env()`](https://rlang.r-lib.org/reference/ns_env.md)) with
`env_print()` will typically tag function bindings as `<lazy>` until
they are evaluated the first time. This is because package functions are
lazily-loaded from disk to improve performance when loading a package.

## Usage

``` r
env_print(env = caller_env())
```

## Arguments

- env:

  An environment, or object that can be converted to an environment by
  [`get_env()`](https://rlang.r-lib.org/reference/get_env.md).
