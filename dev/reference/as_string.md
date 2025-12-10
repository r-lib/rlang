# Cast symbol to string

`as_string()` converts
[symbols](https://rlang.r-lib.org/dev/reference/sym.md) to character
strings.

## Usage

``` r
as_string(x)
```

## Arguments

- x:

  A string or symbol. If a string, the attributes are removed, if any.

## Value

A character vector of length 1.

## Unicode tags

Unlike [`base::as.symbol()`](https://rdrr.io/r/base/name.html) and
[`base::as.name()`](https://rdrr.io/r/base/name.html), `as_string()`
automatically transforms unicode tags such as `"<U+5E78>"` to the proper
UTF-8 character. This is important on Windows because:

- R on Windows has no UTF-8 support, and uses native encoding instead.

- The native encodings do not cover all Unicode characters. For example,
  Western encodings do not support CKJ characters.

- When a lossy UTF-8 -\> native transformation occurs, uncovered
  characters are transformed to an ASCII unicode tag like `"<U+5E78>"`.

- Symbols are always encoded in native. This means that transforming the
  column names of a data frame to symbols might be a lossy operation.

- This operation is very common in the tidyverse because of data masking
  APIs like dplyr where data frames are transformed to environments.
  While the names of a data frame are stored as a character vector, the
  bindings of environments are stored as symbols.

Because it reencodes the ASCII unicode tags to their UTF-8
representation, the string -\> symbol -\> string roundtrip is more
stable with `as_string()`.

## See also

[`as_name()`](https://rlang.r-lib.org/dev/reference/as_name.md) for a
higher-level variant of `as_string()` that automatically unwraps
quosures.

## Examples

``` r
# Let's create some symbols:
foo <- quote(foo)
bar <- sym("bar")

# as_string() converts symbols to strings:
foo
#> foo
as_string(foo)
#> [1] "foo"

typeof(bar)
#> [1] "symbol"
typeof(as_string(bar))
#> [1] "character"
```
