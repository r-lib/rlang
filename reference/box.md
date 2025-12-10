# Box a value

`new_box()` is similar to
[`base::I()`](https://rdrr.io/r/base/AsIs.html) but it protects a value
by wrapping it in a scalar list rather than by adding an attribute.
`unbox()` retrieves the boxed value. `is_box()` tests whether an object
is boxed with optional class.
[`as_box()`](https://rlang.r-lib.org/reference/as_box.md) ensures that a
value is wrapped in a box.
[`as_box_if()`](https://rlang.r-lib.org/reference/as_box.md) does the
same but only if the value matches a predicate.

## Usage

``` r
new_box(.x, class = NULL, ...)

is_box(x, class = NULL)

unbox(box)
```

## Arguments

- class:

  For `new_box()`, an additional class for the boxed value (in addition
  to `rlang_box`). For `is_box()`, a class or vector of classes passed
  to
  [`inherits_all()`](https://rlang.r-lib.org/reference/inherits_any.md).

- ...:

  Additional attributes passed to
  [`base::structure()`](https://rdrr.io/r/base/structure.html).

- x, .x:

  An R object.

- box:

  A boxed value to unbox.

## Examples

``` r
boxed <- new_box(letters, "mybox")
is_box(boxed)
#> [1] TRUE
is_box(boxed, "mybox")
#> [1] TRUE
is_box(boxed, "otherbox")
#> [1] FALSE

unbox(boxed)
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [17] "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"

# as_box() avoids double-boxing:
boxed2 <- as_box(boxed, "mybox")
boxed2
#> [[1]]
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [17] "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
#> 
#> attr(,"class")
#> [1] "mybox"     "rlang_box"
unbox(boxed2)
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [17] "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"

# Compare to:
boxed_boxed <- new_box(boxed, "mybox")
boxed_boxed
#> [[1]]
#> [[1]]
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [17] "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
#> 
#> attr(,"class")
#> [1] "mybox"     "rlang_box"
#> 
#> attr(,"class")
#> [1] "mybox"     "rlang_box"
unbox(unbox(boxed_boxed))
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [17] "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"

# Use `as_box_if()` with a predicate if you need to ensure a box
# only for a subset of values:
as_box_if(NULL, is_null, "null_box")
#> [[1]]
#> NULL
#> 
#> attr(,"class")
#> [1] "null_box"  "rlang_box"
as_box_if("foo", is_null, "null_box")
#> [1] "foo"
```
