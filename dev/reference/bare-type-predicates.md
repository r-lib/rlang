# Bare type predicates

These predicates check for a given type but only return `TRUE` for bare
R objects. Bare objects have no class attributes. For example, a data
frame is a list, but not a bare list.

## Usage

``` r
is_bare_list(x, n = NULL)

is_bare_atomic(x, n = NULL)

is_bare_vector(x, n = NULL)

is_bare_double(x, n = NULL)

is_bare_complex(x, n = NULL)

is_bare_integer(x, n = NULL)

is_bare_numeric(x, n = NULL)

is_bare_character(x, n = NULL)

is_bare_logical(x, n = NULL)

is_bare_raw(x, n = NULL)

is_bare_string(x, n = NULL)

is_bare_bytes(x, n = NULL)
```

## Arguments

- x:

  Object to be tested.

- n:

  Expected length of a vector.

## Details

- The predicates for vectors include the `n` argument for
  pattern-matching on the vector length.

- Like
  [`is_atomic()`](https://rlang.r-lib.org/dev/reference/type-predicates.md)
  and unlike base R
  [`is.atomic()`](https://rdrr.io/r/base/is.recursive.html) for R \<
  4.4.0, `is_bare_atomic()` does not return `TRUE` for `NULL`. Starting
  in R 4.4.0, `is.atomic(NULL)` returns FALSE.

- Unlike base R [`is.numeric()`](https://rdrr.io/r/base/numeric.html),
  `is_bare_double()` only returns `TRUE` for floating point numbers.

## See also

[type-predicates](https://rlang.r-lib.org/dev/reference/type-predicates.md),
[scalar-type-predicates](https://rlang.r-lib.org/dev/reference/scalar-type-predicates.md)
