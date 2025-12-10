# Get the namespace of a package

Namespaces are the environment where all the functions of a package
live. The parent environments of namespaces are the `imports`
environments, which contain all the functions imported from other
packages.

## Usage

``` r
ns_env(x = caller_env())

ns_imports_env(x = caller_env())

ns_env_name(x = caller_env())
```

## Arguments

- x:

  - For `ns_env()`, the name of a package or an environment as a string.

    - An environment (the current environment by default).

    - A function.

    In the latter two cases, the environment ancestry is searched for a
    namespace with
    [`base::topenv()`](https://rdrr.io/r/base/ns-topenv.html). If the
    environment doesn't inherit from a namespace, this is an error.

## See also

[`pkg_env()`](https://rlang.r-lib.org/reference/search_envs.md)
