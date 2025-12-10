# Zap source references

There are a number of situations where R creates source references:

- Reading R code from a file with
  [`source()`](https://rdrr.io/r/base/source.html) and
  [`parse()`](https://rdrr.io/r/base/parse.html) might save source
  references inside calls to `function` and `{`.

- [`sys.call()`](https://rdrr.io/r/base/sys.parent.html) includes a
  source reference if possible.

- Creating a closure stores the source reference from the call to
  `function`, if any.

These source references take up space and might cause a number of
issues. `zap_srcref()` recursively walks through expressions and
functions to remove all source references.

## Usage

``` r
zap_srcref(x)
```

## Arguments

- x:

  An R object. Functions and calls are walked recursively.
