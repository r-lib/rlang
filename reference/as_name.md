# Extract names from symbols

`as_name()` converts [symbols](https://rlang.r-lib.org/reference/sym.md)
to character strings. The conversion is deterministic. That is, the
roundtrip `symbol -> name -> symbol` always gives the same result.

- Use `as_name()` when you need to transform a symbol to a string to
  *refer* to an object by its name.

- Use [`as_label()`](https://rlang.r-lib.org/reference/as_label.md) when
  you need to transform any kind of object to a string to *represent*
  that object with a short description.

## Usage

``` r
as_name(x)
```

## Arguments

- x:

  A string or symbol, possibly wrapped in a
  [quosure](https://rlang.r-lib.org/reference/quosure-tools.md). If a
  string, the attributes are removed, if any.

## Value

A character vector of length 1.

## Details

`rlang::as_name()` is the *opposite* of
[`base::as.name()`](https://rdrr.io/r/base/name.html). If you're writing
base R code, we recommend using
[`base::as.symbol()`](https://rdrr.io/r/base/name.html) which is an
alias of [`as.name()`](https://rdrr.io/r/base/name.html) that follows a
more modern terminology (R types instead of S modes).

## See also

[`as_label()`](https://rlang.r-lib.org/reference/as_label.md) for
converting any object to a single string suitable as a label.
[`as_string()`](https://rlang.r-lib.org/reference/as_string.md) for a
lower-level version that doesn't unwrap quosures.

## Examples

``` r
# Let's create some symbols:
foo <- quote(foo)
bar <- sym("bar")

# as_name() converts symbols to strings:
foo
#> foo
as_name(foo)
#> [1] "foo"

typeof(bar)
#> [1] "symbol"
typeof(as_name(bar))
#> [1] "character"

# as_name() unwraps quosured symbols automatically:
as_name(quo(foo))
#> [1] "foo"
```
