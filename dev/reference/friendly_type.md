# Format a type for error messages

**\[deprecated\]**

`friendly_type()` is deprecated. Please use the `standalone-obj-type.R`
file instead. You can import it in your package with
`usethis::use_standalone("r-lib/rlang", "obj-type")`.

## Usage

``` r
friendly_type(type)
```

## Arguments

- type:

  A type as returned by
  [`typeof()`](https://rdrr.io/r/base/typeof.html).

## Value

A string of the prettified type, qualified with an indefinite article.
