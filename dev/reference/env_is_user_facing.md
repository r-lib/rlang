# Is frame environment user facing?

Detects if `env` is user-facing, that is, whether it's an environment
that inherits from:

- The global environment, as would happen when called interactively

- A package that is currently being tested

If either is true, we consider `env` to belong to an evaluation frame
that was called *directly* by the end user. This is by contrast to
*indirect* calls by third party functions which are not user facing.

For instance the [lifecycle](https://lifecycle.r-lib.org/) package uses
`env_is_user_facing()` to figure out whether a deprecated function was
called directly or indirectly, and select an appropriate verbosity level
as a function of that.

## Usage

``` r
env_is_user_facing(env)
```

## Arguments

- env:

  An environment.

## Escape hatch

You can override the return value of `env_is_user_facing()` by setting
the global option `"rlang_user_facing"` to:

- `TRUE` or `FALSE`.

- A package name as a string. Then `env_is_user_facing(x)` returns
  `TRUE` if `x` inherits from the namespace corresponding to that
  package name.

## Examples

``` r
fn <- function() {
  env_is_user_facing(caller_env())
}

# Direct call of `fn()` from the global env
with(global_env(), fn())
#> Error in fn(): could not find function "fn"

# Indirect call of `fn()` from a package
with(ns_env("utils"), fn())
#> Error in fn(): could not find function "fn"
```
