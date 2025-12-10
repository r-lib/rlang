# Is object named?

- `is_named()` is a scalar predicate that checks that `x` has a `names`
  attribute and that none of the names are missing or empty (`NA` or
  `""`).

- `is_named2()` is like `is_named()` but always returns `TRUE` for empty
  vectors, even those that don't have a `names` attribute. In other
  words, it tests for the property that each element of a vector is
  named. `is_named2()` composes well with
  [`names2()`](https://rlang.r-lib.org/reference/names2.md) whereas
  `is_named()` composes with
  [`names()`](https://rdrr.io/r/base/names.html).

- `have_name()` is a vectorised variant.

## Usage

``` r
is_named(x)

is_named2(x)

have_name(x)
```

## Arguments

- x:

  A vector to test.

## Value

`is_named()` and `is_named2()` are scalar predicates that return `TRUE`
or `FALSE`. `have_name()` is vectorised and returns a logical vector as
long as the input.

## Details

`is_named()` always returns `TRUE` for empty vectors because

## Examples

``` r
# is_named() is a scalar predicate about the whole vector of names:
is_named(c(a = 1, b = 2))
#> [1] TRUE
is_named(c(a = 1, 2))
#> [1] FALSE

# Unlike is_named2(), is_named() returns `FALSE` for empty vectors
# that don't have a `names` attribute.
is_named(list())
#> [1] FALSE
is_named2(list())
#> [1] TRUE

# have_name() is a vectorised predicate
have_name(c(a = 1, b = 2))
#> [1] TRUE TRUE
have_name(c(a = 1, 2))
#> [1]  TRUE FALSE

# Empty and missing names are treated as invalid:
invalid <- set_names(letters[1:5])
names(invalid)[1] <- ""
names(invalid)[3] <- NA

is_named(invalid)
#> [1] FALSE
have_name(invalid)
#> [1] FALSE  TRUE FALSE  TRUE  TRUE

# A data frame normally has valid, unique names
is_named(mtcars)
#> [1] TRUE
have_name(mtcars)
#>  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

# A matrix usually doesn't because the names are stored in a
# different attribute
mat <- matrix(1:4, 2)
colnames(mat) <- c("a", "b")
is_named(mat)
#> [1] FALSE
names(mat)
#> NULL
```
