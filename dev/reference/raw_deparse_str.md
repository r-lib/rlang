# Serialize a raw vector to a string

**\[experimental\]**

This function converts a raw vector to a hexadecimal string, optionally
adding a prefix and a suffix. It is roughly equivalent to
`paste0(prefix, paste(format(x), collapse = ""), suffix)` and much
faster.

## Usage

``` r
raw_deparse_str(x, prefix = NULL, suffix = NULL)
```

## Arguments

- x:

  A raw vector.

- prefix, suffix:

  Prefix and suffix strings, or \`NULL.

## Value

A string.

## Examples

``` r
raw_deparse_str(raw())
#> [1] ""
raw_deparse_str(charToRaw("string"))
#> [1] "737472696e67"
raw_deparse_str(raw(10), prefix = "'0x", suffix = "'")
#> [1] "'0x00000000000000000000'"
```
