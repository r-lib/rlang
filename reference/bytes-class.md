# Human readable memory sizes

Construct, manipulate and display vectors of byte sizes. These are
numeric vectors, so you can compare them numerically, but they can also
be compared to human readable values such as '10MB'.

- `parse_bytes()` takes a character vector of human-readable bytes and
  returns a structured bytes vector.

- `as_bytes()` is a generic conversion function for objects representing
  bytes.

Note: A
[`bytes()`](https://rlang.r-lib.org/reference/vector-construction.md)
constructor will be exported soon.

## Usage

``` r
as_bytes(x)

parse_bytes(x)
```

## Arguments

- x:

  A numeric or character vector. Character representations can use
  shorthand sizes (see examples).

## Details

These memory sizes are always assumed to be base 1000, rather than 1024.

## Examples

``` r
parse_bytes("1")
#> [1] 1 B
parse_bytes("1K")
#> [1] 1 kB
parse_bytes("1Kb")
#> [1] 1 kB
parse_bytes("1KiB")
#> [1] 1 kB
parse_bytes("1MB")
#> [1] 1 MB

parse_bytes("1KB") < "1MB"
#> [1] TRUE

sum(parse_bytes(c("1MB", "5MB", "500KB")))
#> [1] 6.50 MB
```
