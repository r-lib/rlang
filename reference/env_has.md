# Does an environment have or see bindings?

`env_has()` is a vectorised predicate that queries whether an
environment owns bindings personally (with `inherit` set to `FALSE`, the
default), or sees them in its own environment or in any of its parents
(with `inherit = TRUE`).

## Usage

``` r
env_has(env = caller_env(), nms, inherit = FALSE)
```

## Arguments

- env:

  An environment.

- nms:

  A character vector of binding names for which to check existence.

- inherit:

  Whether to look for bindings in the parent environments.

## Value

A named logical vector as long as `nms`.

## Examples

``` r
parent <- child_env(NULL, foo = "foo")
env <- child_env(parent, bar = "bar")

# env does not own `foo` but sees it in its parent environment:
env_has(env, "foo")
#>   foo 
#> FALSE 
env_has(env, "foo", inherit = TRUE)
#>  foo 
#> TRUE 
```
