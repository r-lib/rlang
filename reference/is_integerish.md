# Is a vector integer-like?

These predicates check whether R considers a number vector to be
integer-like, according to its own tolerance check (which is in fact
delegated to the C library). This function is not adapted to data
analysis, see the help for
[`base::is.integer()`](https://rdrr.io/r/base/integer.html) for examples
of how to check for whole numbers.

Things to consider when checking for integer-like doubles:

- This check can be expensive because the whole double vector has to be
  traversed and checked.

- Large double values may be integerish but may still not be coercible
  to integer. This is because integers in R only support values up to
  `2^31 - 1` while numbers stored as double can be much larger.

## Usage

``` r
is_integerish(x, n = NULL, finite = NULL)

is_bare_integerish(x, n = NULL, finite = NULL)

is_scalar_integerish(x, finite = NULL)
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

## See also

[`is_bare_numeric()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
for testing whether an object is a base numeric type (a bare double or
integer vector).

## Examples

``` r
is_integerish(10L)
#> [1] TRUE
is_integerish(10.0)
#> [1] TRUE
is_integerish(10.0, n = 2)
#> [1] FALSE
is_integerish(10.000001)
#> [1] FALSE
is_integerish(TRUE)
#> [1] FALSE
```
