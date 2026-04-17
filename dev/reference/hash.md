# Hashing

- `hash()` hashes an arbitrary R object.

- `hash_file()` hashes the data contained in a file.

For ordinary data objects (vectors, lists, data frames, etc.), the
generated hash is reproducible across sessions and R versions on
platforms that have the same endianness. However, hash values may change
between rlang versions, although that should be rare. Reference-like
objects (environments, external pointers, builtins) are hashed by
identity, so their hashes are only stable within a session. Closures
hash their formals, body, and environment identity. Byte-compiled and
uncompiled closures hash identically because the body is always hashed
from the original language tree.

By default, source references are stripped before hashing so that
closures and calls that are textually identical produce the same hash
regardless of where they were parsed. Set `zap_srcref` to `FALSE` to
include source references in the hash.

## Usage

``` r
hash(x, zap_srcref = TRUE)

hash_file(path)
```

## Arguments

- x:

  An object.

- zap_srcref:

  Whether to ignore source references when hashing (default `TRUE`).
  Source references depend on parse location, so including them makes
  hashes of closures and calls non-reproducible across sessions.

- path:

  A character vector of paths to the files to be hashed.

## Value

- For `hash()`, a single character string containing the hash.

- For `hash_file()`, a character vector containing one hash per file.

## Details

These hashers use the XXH128 hash algorithm of the xxHash library, which
generates a 128-bit hash. Both are implemented as streaming hashes,
which generate the hash with minimal extra memory usage.

For `hash()`, a custom object walker feeds the object's type, length,
data bytes, and attributes directly into the hash algorithm. This avoids
dependency on R's serialization format, making the hash immune to
internal representation details (e.g. ALTREP compact forms, the growable
vector bit).

## Examples

``` r
hash(c(1, 2, 3))
#> [1] "7f210cbfa5470651f6f9e9d220634266"
hash(mtcars)
#> [1] "6755d143ff87b73a1196c186cec7e86a"

authors <- file.path(R.home("doc"), "AUTHORS")
copying <- file.path(R.home("doc"), "COPYING")
hashes <- hash_file(c(authors, copying))
hashes
#> [1] "fc4d57b62fa48aa0e3fb847cdce2cf88"
#> [2] "cdb3a24318136e74f38209c219ca104b"

# If you need a single hash for multiple files,
# hash the result of `hash_file()`
hash(hashes)
#> [1] "6f31dd06464e362fbe7e9d4040340326"
```
