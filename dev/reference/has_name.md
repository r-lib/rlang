# Does an object have an element with this name?

This function returns a logical value that indicates if a data frame or
another named object contains an element with a specific name. Note that
`has_name()` only works with vectors. For instance, environments need
the specialised function
[`env_has()`](https://rlang.r-lib.org/dev/reference/env_has.md).

## Usage

``` r
has_name(x, name)
```

## Arguments

- x:

  A data frame or another named object

- name:

  Element name(s) to check

## Value

A logical vector of the same length as `name`

## Details

Unnamed objects are treated as if all names are empty strings. `NA`
input gives `FALSE` as output.

## Examples

``` r
has_name(iris, "Species")
#> [1] TRUE
has_name(mtcars, "gears")
#> [1] FALSE
```
