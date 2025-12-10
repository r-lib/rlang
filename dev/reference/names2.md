# Get names of a vector

`names2()` always returns a character vector, even when an object does
not have a `names` attribute. In this case, it returns a vector of empty
names `""`. It also standardises missing names to `""`.

The replacement variant `names2<-` never adds `NA` names and instead
fills unnamed vectors with `""`.

## Usage

``` r
names2(x)

names2(x) <- value
```

## Arguments

- x:

  A vector.

- value:

  New names.

## Examples

``` r
names2(letters)
#>  [1] "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""
#> [23] "" "" "" ""

# It also takes care of standardising missing names:
x <- set_names(1:3, c("a", NA, "b"))
names2(x)
#> [1] "a" ""  "b"

# Replacing names with the base `names<-` function may introduce
# `NA` values when the vector is unnamed:
x <- 1:3
names(x)[1:2] <- "foo"
names(x)
#> [1] "foo" "foo" NA   

# Use the `names2<-` variant to avoid this
x <- 1:3
names2(x)[1:2] <- "foo"
names(x)
#> [1] "foo" "foo" ""   
```
