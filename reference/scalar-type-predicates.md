# Scalar type predicates

These predicates check for a given type and whether the vector is
"scalar", that is, of length 1.

In addition to the length check, `is_string()` and `is_bool()` return
`FALSE` if their input is missing. This is useful for type-checking
arguments, when your function expects a single string or a single `TRUE`
or `FALSE`.

## Usage

``` r
is_scalar_list(x)

is_scalar_atomic(x)

is_scalar_vector(x)

is_scalar_integer(x)

is_scalar_double(x)

is_scalar_complex(x)

is_scalar_character(x)

is_scalar_logical(x)

is_scalar_raw(x)

is_string(x, string = NULL)

is_scalar_bytes(x)

is_bool(x)
```

## Arguments

- x:

  object to be tested.

- string:

  A string to compare to `x`. If a character vector, returns `TRUE` if
  at least one element is equal to `x`.

## See also

[type-predicates](https://rlang.r-lib.org/reference/type-predicates.md),
[bare-type-predicates](https://rlang.r-lib.org/reference/bare-type-predicates.md)
