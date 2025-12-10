# Hashing

- `hash()` hashes an arbitrary R object.

- `hash_file()` hashes the data contained in a file.

The generated hash is guaranteed to be reproducible across platforms
that have the same endianness and are using the same R version.

## Usage

``` r
hash(x)

hash_file(path)
```

## Arguments

- x:

  An object.

- path:

  A character vector of paths to the files to be hashed.

## Value

- For `hash()`, a single character string containing the hash.

- For `hash_file()`, a character vector containing one hash per file.

## Details

These hashers use the XXH128 hash algorithm of the xxHash library, which
generates a 128-bit hash. Both are implemented as streaming hashes,
which generate the hash with minimal extra memory usage.

For `hash()`, objects are converted to binary using R's native
serialization tools. Serialization version 3 is used. See
[`serialize()`](https://rdrr.io/r/base/serialize.html) for more
information about the serialization version.

## Examples

``` r
hash(c(1, 2, 3))
#> [1] "702f7dd6e81ea41d26ea3b248627ece4"
hash(mtcars)
#> [1] "d0487363db4e6cc64fdb740cb6617fc0"

authors <- file.path(R.home("doc"), "AUTHORS")
copying <- file.path(R.home("doc"), "COPYING")
hashes <- hash_file(c(authors, copying))
hashes
#> [1] "fc4d57b62fa48aa0e3fb847cdce2cf88"
#> [2] "cdb3a24318136e74f38209c219ca104b"

# If you need a single hash for multiple files,
# hash the result of `hash_file()`
hash(hashes)
#> [1] "a061b5938a6bd7c4beb6e2dc25f470a3"
```
