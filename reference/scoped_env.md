# Deprecated `scoped` functions

**\[deprecated\]**

These functions are deprecated as of rlang 0.3.0. Please use
[`is_attached()`](https://rlang.r-lib.org/reference/search_envs.md)
instead.

## Usage

``` r
scoped_env(nm)

is_scoped(nm)
```

## Arguments

- nm:

  The name of an environment attached to the search path. Call
  [`base::search()`](https://rdrr.io/r/base/search.html) to see what is
  currently on the path.
