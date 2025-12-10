# Type predicates

These type predicates aim to make type testing in R more consistent.
They are wrappers around
[`base::typeof()`](https://rdrr.io/r/base/typeof.html), so operate at a
level beneath S3/S4 etc.

## Usage

``` r
is_list(x, n = NULL)

is_atomic(x, n = NULL)

is_vector(x, n = NULL)

is_integer(x, n = NULL)

is_double(x, n = NULL, finite = NULL)

is_complex(x, n = NULL, finite = NULL)

is_character(x, n = NULL)

is_logical(x, n = NULL)

is_raw(x, n = NULL)

is_bytes(x, n = NULL)

is_null(x)
```

## Arguments

- x:

  Object to be tested.

- n:

  Expected length of a vector.

- finite:

  Whether all values of the vector are finite. The non-finite values are
  `NA`, `Inf`, `-Inf` and `NaN`. Setting this to something other than
  `NULL` can be expensive because the whole vector needs to be traversed
  and checked.

## Details

Compared to base R functions:

- The predicates for vectors include the `n` argument for
  pattern-matching on the vector length.

- Unlike [`is.atomic()`](https://rdrr.io/r/base/is.recursive.html) in R
  \< 4.4.0, `is_atomic()` does not return `TRUE` for `NULL`. Starting in
  R 4.4.0 `is.atomic(NULL)` returns FALSE.

- Unlike [`is.vector()`](https://rdrr.io/r/base/vector.html),
  `is_vector()` tests if an object is an atomic vector or a list.
  `is.vector` checks for the presence of attributes (other than name).

## See also

[bare-type-predicates](https://rlang.r-lib.org/dev/reference/bare-type-predicates.md)
[scalar-type-predicates](https://rlang.r-lib.org/dev/reference/scalar-type-predicates.md)
