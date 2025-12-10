# Create vectors matching a given length

**\[questioning\]**

These functions construct vectors of a given length, with attributes
specified via dots. Except for `new_list()` and `new_raw()`, the empty
vectors are filled with typed
[missing](https://rlang.r-lib.org/dev/reference/missing.md) values. This
is in contrast to the base function
[`base::vector()`](https://rdrr.io/r/base/vector.html) which creates
zero-filled vectors.

## Usage

``` r
new_logical(n, names = NULL)

new_integer(n, names = NULL)

new_double(n, names = NULL)

new_character(n, names = NULL)

new_complex(n, names = NULL)

new_raw(n, names = NULL)

new_list(n, names = NULL)
```

## Arguments

- n:

  The vector length.

- names:

  Names for the new vector.

## Lifecycle

These functions are likely to be replaced by a vctrs equivalent in the
future. They are in the questioning lifecycle stage.

## See also

rep_along

## Examples

``` r
new_list(10)
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
#> 
#> [[3]]
#> NULL
#> 
#> [[4]]
#> NULL
#> 
#> [[5]]
#> NULL
#> 
#> [[6]]
#> NULL
#> 
#> [[7]]
#> NULL
#> 
#> [[8]]
#> NULL
#> 
#> [[9]]
#> NULL
#> 
#> [[10]]
#> NULL
#> 
new_logical(10)
#>  [1] NA NA NA NA NA NA NA NA NA NA
```
