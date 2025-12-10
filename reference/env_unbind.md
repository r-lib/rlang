# Remove bindings from an environment

`env_unbind()` is the complement of
[`env_bind()`](https://rlang.r-lib.org/reference/env_bind.md). Like
[`env_has()`](https://rlang.r-lib.org/reference/env_has.md), it ignores
the parent environments of `env` by default. Set `inherit` to `TRUE` to
track down bindings in parent environments.

## Usage

``` r
env_unbind(env = caller_env(), nms, inherit = FALSE)
```

## Arguments

- env:

  An environment.

- nms:

  A character vector of binding names to remove.

- inherit:

  Whether to look for bindings in the parent environments.

## Value

The input object `env` with its associated environment modified in
place, invisibly.

## Examples

``` r
env <- env(foo = 1, bar = 2)
env_has(env, c("foo", "bar"))
#>  foo  bar 
#> TRUE TRUE 

# Remove bindings with `env_unbind()`
env_unbind(env, c("foo", "bar"))
env_has(env, c("foo", "bar"))
#>   foo   bar 
#> FALSE FALSE 

# With inherit = TRUE, it removes bindings in parent environments
# as well:
parent <- env(empty_env(), foo = 1, bar = 2)
env <- env(parent, foo = "b")

env_unbind(env, "foo", inherit = TRUE)
env_has(env, c("foo", "bar"))
#>   foo   bar 
#> FALSE FALSE 
env_has(env, c("foo", "bar"), inherit = TRUE)
#>  foo  bar 
#> TRUE TRUE 
```
