# Create a list of quosures

This small S3 class provides methods for `[` and
[`c()`](https://rdrr.io/r/base/c.html) and ensures the following
invariants:

- The list only contains quosures.

- It is always named, possibly with a vector of empty strings.

`new_quosures()` takes a list of quosures and adds the `quosures` class
and a vector of empty names if needed. `as_quosures()` calls
[`as_quosure()`](https://rlang.r-lib.org/dev/reference/new_quosure.md)
on all elements before creating the `quosures` object.

## Usage

``` r
new_quosures(x)

as_quosures(x, env, named = FALSE)

is_quosures(x)
```

## Arguments

- x:

  A list of quosures or objects to coerce to quosures.

- env:

  The default environment for the new quosures.

- named:

  Whether to name the list with
  [`quos_auto_name()`](https://rlang.r-lib.org/dev/reference/exprs_auto_name.md).
