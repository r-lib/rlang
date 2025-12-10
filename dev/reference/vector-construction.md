# Create vectors

**\[questioning\]**

The atomic vector constructors are equivalent to
[`c()`](https://rdrr.io/r/base/c.html) but:

- They allow you to be more explicit about the output type. Implicit
  coercions (e.g. from integer to logical) follow the rules described in
  [vector-coercion](https://rlang.r-lib.org/dev/reference/vector-coercion.md).

- They use [dynamic
  dots](https://rlang.r-lib.org/dev/reference/dyn-dots.md).

## Usage

``` r
lgl(...)

int(...)

dbl(...)

cpl(...)

chr(...)

bytes(...)
```

## Arguments

- ...:

  Components of the new vector. Bare lists and explicitly spliced lists
  are spliced.

## Life cycle

- All the abbreviated constructors such as `lgl()` will probably be
  moved to the vctrs package at some point. This is why they are marked
  as questioning.

- Automatic splicing is soft-deprecated and will trigger a warning in a
  future version. Please splice explicitly with `!!!`.

## Examples

``` r
# These constructors are like a typed version of c():
c(TRUE, FALSE)
#> [1]  TRUE FALSE
lgl(TRUE, FALSE)
#> [1]  TRUE FALSE

# They follow a restricted set of coercion rules:
int(TRUE, FALSE, 20)
#> [1]  1  0 20

# Lists can be spliced:
dbl(10, !!! list(1, 2L), TRUE)
#> [1] 10  1  2  1


# They splice names a bit differently than c(). The latter
# automatically composes inner and outer names:
c(a = c(A = 10), b = c(B = 20, C = 30))
#> a.A b.B b.C 
#>  10  20  30 

# On the other hand, rlang's constructors use the inner names and issue a
# warning to inform the user that the outer names are ignored:
dbl(a = c(A = 10), b = c(B = 20, C = 30))
#> Warning: Outer names are only allowed for unnamed scalar atomic inputs
#>  A  B  C 
#> 10 20 30 
dbl(a = c(1, 2))
#> Warning: Outer names are only allowed for unnamed scalar atomic inputs
#> [1] 1 2

# As an exception, it is allowed to provide an outer name when the
# inner vector is an unnamed scalar atomic:
dbl(a = 1)
#> a 
#> 1 

# Spliced lists behave the same way:
dbl(!!! list(a = 1))
#> a 
#> 1 
dbl(!!! list(a = c(A = 1)))
#> Warning: Outer names are only allowed for unnamed scalar atomic inputs
#> A 
#> 1 

# bytes() accepts integerish inputs
bytes(1:10)
#>  [1] 01 02 03 04 05 06 07 08 09 0a
bytes(0x01, 0xff, c(0x03, 0x05), list(10, 20, 30L))
#> [1] 01 ff 03 05 0a 14 1e
```
