# Missing values

**\[questioning\]**

Missing values are represented in R with the general symbol `NA`. They
can be inserted in almost all data containers: all atomic vectors except
raw vectors can contain missing values. To achieve this, R automatically
converts the general `NA` symbol to a typed missing value appropriate
for the target vector. The objects provided here are aliases for those
typed `NA` objects.

## Usage

``` r
na_lgl

na_int

na_dbl

na_chr

na_cpl
```

## Format

An object of class `logical` of length 1.

An object of class `integer` of length 1.

An object of class `numeric` of length 1.

An object of class `character` of length 1.

An object of class `complex` of length 1.

## Details

Typed missing values are necessary because R needs sentinel values of
the same type (i.e. the same machine representation of the data) as the
containers into which they are inserted. The official typed missing
values are `NA_integer_`, `NA_real_`, `NA_character_` and `NA_complex_`.
The missing value for logical vectors is simply the default `NA`. The
aliases provided in rlang are consistently named and thus simpler to
remember. Also, `na_lgl` is provided as an alias to `NA` that makes
intent clearer.

Since `na_lgl` is the default `NA`, expressions such as `c(NA, NA)`
yield logical vectors as no data is available to give a clue of the
target type. In the same way, since lists and environments can contain
any types, expressions like `list(NA)` store a logical `NA`.

## Life cycle

These shortcuts might be moved to the vctrs package at some point. This
is why they are marked as questioning.

## Examples

``` r
typeof(NA)
#> [1] "logical"
typeof(na_lgl)
#> [1] "logical"
typeof(na_int)
#> [1] "integer"

# Note that while the base R missing symbols cannot be overwritten,
# that's not the case for rlang's aliases:
na_dbl <- NA
typeof(na_dbl)
#> [1] "logical"
```
