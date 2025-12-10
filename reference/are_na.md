# Test for missing values

**\[questioning\]**

`are_na()` checks for missing values in a vector and is equivalent to
[`base::is.na()`](https://rdrr.io/r/base/NA.html). It is a vectorised
predicate, meaning that its output is always the same length as its
input. On the other hand, `is_na()` is a scalar predicate and always
returns a scalar boolean, `TRUE` or `FALSE`. If its input is not scalar,
it returns `FALSE`. Finally, there are typed versions that check for
particular [missing
types](https://rlang.r-lib.org/reference/missing.md).

## Usage

``` r
are_na(x)

is_na(x)

is_lgl_na(x)

is_int_na(x)

is_dbl_na(x)

is_chr_na(x)

is_cpl_na(x)
```

## Arguments

- x:

  An object to test

## Details

The scalar predicates accept non-vector inputs. They are equivalent to
[`is_null()`](https://rlang.r-lib.org/reference/type-predicates.md) in
that respect. In contrast the vectorised predicate `are_na()` requires a
vector input since it is defined over vector values.

## Life cycle

These functions might be moved to the vctrs package at some point. This
is why they are marked as questioning.

## Examples

``` r
# are_na() is vectorised and works regardless of the type
are_na(c(1, 2, NA))
#> [1] FALSE FALSE  TRUE
are_na(c(1L, NA, 3L))
#> [1] FALSE  TRUE FALSE

# is_na() checks for scalar input and works for all types
is_na(NA)
#> [1] TRUE
is_na(na_dbl)
#> [1] TRUE
is_na(character(0))
#> [1] FALSE

# There are typed versions as well:
is_lgl_na(NA)
#> [1] TRUE
is_lgl_na(na_dbl)
#> [1] FALSE
```
