# Dispatch on base types

**\[soft-deprecated\]** **\[experimental\]**

`switch_type()` is equivalent to
[`switch`](https://rdrr.io/r/base/switch.html)`(`[`type_of`](https://rlang.r-lib.org/reference/type_of.md)`(x, ...))`,
while `switch_class()` switchpatches based on `class(x)`. The `coerce_`
versions are intended for type conversion and provide a standard error
message when conversion fails.

## Usage

``` r
switch_type(.x, ...)

coerce_type(.x, .to, ...)

switch_class(.x, ...)

coerce_class(.x, .to, ...)
```

## Arguments

- .x:

  An object from which to dispatch.

- ...:

  Named clauses. The names should be types as returned by
  [`type_of()`](https://rlang.r-lib.org/reference/type_of.md).

- .to:

  This is useful when you switchpatch within a coercing function. If
  supplied, this should be a string indicating the target type. A
  catch-all clause is then added to signal an error stating the
  conversion failure. This type is prettified unless `.to` inherits from
  the S3 class `"AsIs"` (see
  [`base::I()`](https://rdrr.io/r/base/AsIs.html)).

## Examples

``` r
switch_type(3L,
  double = "foo",
  integer = "bar",
  "default"
)
#> Warning: `switch_type()` is soft-deprecated as of rlang 0.4.0.
#> Please use `switch(typeof())` or `switch(my_typeof())` instead.
#> This warning is displayed once every 8 hours.
#> [1] "bar"

# Use the coerce_ version to get standardised error handling when no
# type matches:
to_chr <- function(x) {
  coerce_type(x, "a chr",
    integer = as.character(x),
    double = as.character(x)
  )
}
to_chr(3L)
#> Warning: `coerce_type()` is soft-deprecated as of rlang 0.4.0.
#> This warning is displayed once every 8 hours.
#> [1] "3"

# Strings have their own type:
switch_type("str",
  character = "foo",
  string = "bar",
  "default"
)
#> [1] "bar"

# Use a fallthrough clause if you need to dispatch on all character
# vectors, including strings:
switch_type("str",
  string = ,
  character = "foo",
  "default"
)
#> [1] "foo"

# special and builtin functions are treated as primitive, since
# there is usually no reason to treat them differently:
switch_type(base::list,
  primitive = "foo",
  "default"
)
#> [1] "foo"
switch_type(base::`$`,
  primitive = "foo",
  "default"
)
#> [1] "foo"

# closures are not primitives:
switch_type(rlang::switch_type,
  primitive = "foo",
  "default"
)
#> [1] "default"
```
