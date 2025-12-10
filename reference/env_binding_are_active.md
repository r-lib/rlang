# What kind of environment binding?

**\[experimental\]**

## Usage

``` r
env_binding_are_active(env, nms = NULL)

env_binding_are_lazy(env, nms = NULL)
```

## Arguments

- env:

  An environment.

- nms:

  Names of bindings. Defaults to all bindings in `env`.

## Value

A logical vector as long as `nms` and named after it.
