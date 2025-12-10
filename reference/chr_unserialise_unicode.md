# Translate unicode points to UTF-8

**\[experimental\]**

For historical reasons, R translates strings to the native encoding when
they are converted to symbols. This string-to-symbol conversion is not a
rare occurrence and happens for instance to the names of a list of
arguments converted to a call by
[`do.call()`](https://rdrr.io/r/base/do.call.html).

If the string contains unicode characters that cannot be represented in
the native encoding, R serialises those as an ASCII sequence
representing the unicode point. This is why Windows users with western
locales often see strings looking like `<U+xxxx>`. To alleviate some of
the pain, rlang parses strings and looks for serialised unicode points
to translate them back to the proper UTF-8 representation. This
transformation occurs automatically in functions like
[`env_names()`](https://rlang.r-lib.org/reference/env_names.md) and can
be manually triggered with
[`as_utf8_character()`](https://rlang.r-lib.org/reference/as_utf8_character.md)
and `chr_unserialise_unicode()`.

## Usage

``` r
chr_unserialise_unicode(chr)
```

## Arguments

- chr:

  A character vector.

## Life cycle

This function is experimental.

## Examples

``` r
ascii <- "<U+5E78>"
chr_unserialise_unicode(ascii)
#> [1] "幸"

identical(chr_unserialise_unicode(ascii), "\u5e78")
#> [1] TRUE
```
