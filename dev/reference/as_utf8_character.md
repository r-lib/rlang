# Coerce to a character vector and attempt encoding conversion

**\[experimental\]**

Unlike specifying the `encoding` argument in
[`as_string()`](https://rlang.r-lib.org/dev/reference/as_string.md) and
[`as_character()`](https://rlang.r-lib.org/dev/reference/vector-coercion.md),
which is only declarative, these functions actually attempt to convert
the encoding of their input. There are two possible cases:

- The string is tagged as UTF-8 or latin1, the only two encodings for
  which R has specific support. In this case, converting to the same
  encoding is a no-op, and converting to native always works as
  expected, as long as the native encoding, the one specified by the
  `LC_CTYPE` locale has support for all characters occurring in the
  strings. Unrepresentable characters are serialised as unicode points:
  "\<U+xxxx\>".

- The string is not tagged. R assumes that it is encoded in the native
  encoding. Conversion to native is a no-op, and conversion to UTF-8
  should work as long as the string is actually encoded in the locale
  codeset.

When translating to UTF-8, the strings are parsed for serialised unicode
points (e.g. strings looking like "U+xxxx") with
[`chr_unserialise_unicode()`](https://rlang.r-lib.org/dev/reference/chr_unserialise_unicode.md).
This helps to alleviate the effects of character-to-symbol-to-character
roundtrips on systems with non-UTF-8 native encoding.

## Usage

``` r
as_utf8_character(x)
```

## Arguments

- x:

  An object to coerce.

## Examples

``` r
# Let's create a string marked as UTF-8 (which is guaranteed by the
# Unicode escaping in the string):
utf8 <- "caf\uE9"
Encoding(utf8)
#> [1] "UTF-8"
charToRaw(utf8)
#> [1] 63 61 66 c3 a9
```
