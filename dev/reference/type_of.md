# Base type of an object

**\[soft-deprecated\]** **\[experimental\]**

This is equivalent to
[`base::typeof()`](https://rdrr.io/r/base/typeof.html) with a few
differences that make dispatching easier:

- The type of one-sided formulas is "quote".

- The type of character vectors of length 1 is "string".

- The type of special and builtin functions is "primitive".

## Usage

``` r
type_of(x)
```

## Arguments

- x:

  An R object.

## Examples

``` r
type_of(10L)
#> Warning: `type_of()` is deprecated as of rlang 0.4.0.
#> Please use `typeof()` or your own version instead.
#> This warning is displayed once every 8 hours.
#> [1] "integer"

# Quosures are treated as a new base type but not formulas:
type_of(quo(10L))
#> [1] "formula"
type_of(~10L)
#> [1] "formula"

# Compare to base::typeof():
typeof(quo(10L))
#> [1] "language"

# Strings are treated as a new base type:
type_of(letters)
#> [1] "character"
type_of(letters[[1]])
#> [1] "string"

# This is a bit inconsistent with the core language tenet that data
# types are vectors. However, treating strings as a different
# scalar type is quite helpful for switching on function inputs
# since so many arguments expect strings:
switch_type("foo", character = abort("vector!"), string = "result")
#> [1] "result"

# Special and builtin primitives are both treated as primitives.
# That's because it is often irrelevant which type of primitive an
# input is:
typeof(list)
#> [1] "builtin"
typeof(`$`)
#> [1] "special"
type_of(list)
#> [1] "primitive"
type_of(`$`)
#> [1] "primitive"
```
